package mobi.upod.app.data

import java.net.{URL, URI}
import org.joda.time.DateTime
import mobi.upod.data.MappingProvider

final case class EpisodeListItem(
  id: Long,
  uri: URI,
  published: DateTime,
  title: String,
  link: Option[String],
  media: Media,
  podcast: URI,
  podcastInfo: EpisodePodcastInfo,
  isNew: Boolean,
  library: Boolean,
  starred: Boolean,
  downloadInfo: EpisodeDownloadInfo,
  playbackInfo: EpisodePlaybackInfo)
  extends EpisodeBase
  with EpisodeBaseWithDownloadInfo
  with EpisodeBaseWithPlaybackInfo

object EpisodeListItem extends MappingProvider[EpisodeListItem] {
  import mobi.upod.data.Mapping._

  val mapping = map(
    "id" -> long,
    "uri" -> uri,
    "published" -> dateTime,
    "title" -> string,
    "link" -> optional(string),
    "media" -> Media.mapping,
    "podcast" -> uri,
    "podcastInfo" -> EpisodePodcastInfo.mapping,
    "new" -> boolean,
    "library" -> boolean,
    "starred" -> boolean,
    "downloadInfo" -> EpisodeDownloadInfo.mapping,
    "playbackInfo" -> EpisodePlaybackInfo.mapping
  )(apply)(unapply)
}
