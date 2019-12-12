/*
 * Copyright 2018-2019 Scala Steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.gitlab.http4s

import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import org.http4s.{Request, Status, Uri}
import org.scalasteward.core.git.{Branch, Sha1}
import org.scalasteward.core.gitlab._
import org.scalasteward.core.util.uri.uriDecoder
import org.scalasteward.core.util.{HttpJsonClient, MonadThrowable, UnexpectedResponse}
import org.scalasteward.core.vcs.VCSApiAlg
import org.scalasteward.core.vcs.data._

final private[http4s] case class ForkPayload(id: String, namespace: String)
final private[http4s] case class MergeRequestPayload(
    id: String,
    title: String,
    description: String,
    target_project_id: Long,
    source_branch: String,
    target_branch: Branch
)
final private[http4s] case class AcceptMergeRequestPayload(
    squash: Boolean,
    should_remove_source_branch: Boolean,
    merge_when_pipeline_succeeds: Boolean
)
private[http4s] object MergeRequestPayload {
  def apply(id: String, projectId: Long, data: NewPullRequestData): MergeRequestPayload =
    MergeRequestPayload(id, data.title, data.body, projectId, data.head, data.base)
}
final private[http4s] case class MergeRequestOut(
    iid: Long,
    web_url: Uri,
    state: PullRequestState,
    title: String
) {
  val pullRequestOut: PullRequestOut = PullRequestOut(web_url, state, title)
}
final private[http4s] case class CommitId(id: Sha1) {
  val commitOut: CommitOut = CommitOut(id)
}
final private[http4s] case class ProjectId(id: Long)

private[http4s] object GitlabJsonCodec {
  // prevent IntelliJ from removing the import of uriDecoder
  locally(uriDecoder)

  implicit val forkPayloadEncoder: Encoder[ForkPayload] = deriveEncoder
  implicit val userOutDecoder: Decoder[UserOut] = Decoder.instance {
    _.downField("username").as[String].map(UserOut(_))
  }
  implicit val repoOutDecoder: Decoder[RepoOut] = Decoder.instance { c =>
    for {
      name <- c.downField("name").as[String]
      owner <- c
        .downField("owner")
        .as[UserOut]
        .orElse(c.downField("namespace").downField("name").as[String].map(UserOut(_)))
      cloneUrl <- c.downField("http_url_to_repo").as[Uri]
      parent <- c
        .downField("forked_from_project")
        .as[Option[RepoOut]]
      defaultBranch <- c
        .downField("default_branch")
        .as[Option[Branch]]
        .map(_.getOrElse(Branch("master")))
    } yield RepoOut(name, owner, parent, cloneUrl, defaultBranch)
  }

  implicit val projectIdDecoder: Decoder[ProjectId] = deriveDecoder
  implicit val mergeRequestPayloadEncoder: Encoder[MergeRequestPayload] = deriveEncoder
  implicit val mergeRequestOutDecoder: Decoder[MergeRequestOut] = deriveDecoder
  implicit val acceptMergeRequestPayloadEncoder: Encoder[AcceptMergeRequestPayload] = deriveEncoder
  implicit val commitOutDecoder: Decoder[CommitOut] = deriveDecoder[CommitId].map(_.commitOut)
  implicit val branchOutDecoder: Decoder[BranchOut] = deriveDecoder[BranchOut]
}

class Http4sGitLabApiAlg[F[_]: MonadThrowable](
    gitlabApiHost: Uri,
    user: AuthenticatedUser,
    modify: Repo => Request[F] => F[Request[F]],
    doNotFork: Boolean,
    mergeWhenPipelineSucceed: Boolean
)(
    implicit
    client: HttpJsonClient[F]
) extends VCSApiAlg[F] {
  import GitlabJsonCodec._

  val url = new Url(gitlabApiHost)

  override def listPullRequests(repo: Repo, head: String, base: Branch): F[List[PullRequestOut]] =
    for {
      mr <- client
        .get[List[MergeRequestOut]](url.listMergeRequests(repo, head, base.name), modify(repo))
    } yield mr.map(_.pullRequestOut)

  def createFork(repo: Repo): F[RepoOut] = {
    val userOwnedRepo = repo.copy(owner = user.login)
    val data = ForkPayload(url.encodedProjectId(userOwnedRepo), user.login)
    client
      .postWithBody[RepoOut, ForkPayload](url.createFork(repo), data, modify(repo))
      .recoverWith {
        case UnexpectedResponse(_, _, _, Status.Conflict, _) => getRepo(userOwnedRepo)
        // workaround for https://gitlab.com/gitlab-org/gitlab-ce/issues/65275
        // see also https://github.com/fthomas/scala-steward/pull/761
        case UnexpectedResponse(_, _, _, Status.NotFound, _) => getRepo(userOwnedRepo)
      }
  }

  def createPullRequest(repo: Repo, data: NewPullRequestData): F[PullRequestOut] = {
    val targetRepo = if (doNotFork) repo else repo.copy(owner = user.login)
    for {
      projectId <- client.get[ProjectId](url.repos(repo), modify(repo))
      payload = MergeRequestPayload(url.encodedProjectId(targetRepo), projectId.id, data)
      mr <- client.postWithBody[MergeRequestOut, MergeRequestPayload](
        url.mergeRequest(targetRepo),
        payload,
        modify(repo)
      )
      _ <- if (mergeWhenPipelineSucceed) acceptMergeRequest(targetRepo, mr.iid) else ().pure[F]
    } yield mr.pullRequestOut
  }

  def acceptMergeRequest(repo: Repo, mergeRequestId: Long) = {
    val payload = AcceptMergeRequestPayload(false, true, true)
    client.postWithBody[Unit, AcceptMergeRequestPayload](
      url.acceptMergeRequest(repo, mergeRequestId),
      payload,
      modify(repo)
    )
  }

  def getBranch(repo: Repo, branch: Branch): F[BranchOut] =
    client.get(url.getBranch(repo, branch), modify(repo))

  def getRepo(repo: Repo): F[RepoOut] =
    client.get(url.repos(repo), modify(repo))
}
