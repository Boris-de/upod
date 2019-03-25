package mobi.upod.app.data

import java.net.{URL, URI}
import mobi.upod.data.{Mapping, MappingProvider}
import org.joda.time.DateTime

case class Episode(
    podcast: URI,
    uri: URI,
    published: DateTime,
    title: String,
    subTitle: Option[String],
    link: Option[String],
    author: Option[String],
    keywords: Set[String],
    description: Option[String],
    media: Media,
    podcastInfo: EpisodePodcastInfo,
    isNew: Boolean = false,
    library: Boolean = false,
    starred: Boolean = false,
    cached: Boolean = false,
    downloadInfo: EpisodeDownloadInfo = EpisodeDownloadInfo.default,
    playbackInfo: EpisodePlaybackInfo = EpisodePlaybackInfo.default,
    rowId: Option[Long] = None)
  extends EpisodeBase
  with EpisodeBaseWithDownloadInfo
  with EpisodeBaseWithPlaybackInfo {

  def id = rowId.getOrElse(0)
}

object Episode extends MappingProvider[Episode] {

  import Mapping._

  val mapping = map(
     "podcast" -> uri,
     "uri" -> uri,
     "published" -> dateTime,
     "title" -> string,
     "subTitle" -> optional(string),
     "link" -> optional(string),
     "author" -> optional(string),
     "keywords" -> set(string),
     "description" -> optional(string),
     "media" -> Media.mapping,
     "podcastInfo" -> EpisodePodcastInfo.mapping,
     "new" -> boolean,
     "library" -> boolean,
     "starred" -> boolean,
     "cached" -> boolean,
     "downloadInfo" -> EpisodeDownloadInfo.mapping,
     "playbackInfo" -> EpisodePlaybackInfo.mapping,
     "id" -> optional(long)
   )(apply)(unapply)
}
