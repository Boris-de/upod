package mobi.upod.app.services.sync

import java.net.{URI, URL}

import mobi.upod.app.data._
import mobi.upod.data.Mapping._
import mobi.upod.data.MappingProvider
import org.joda.time.DateTime

case class EpisodeSyncInfo(
  podcast: URI,
  uri: URI,
  published: DateTime,
  title: String,
  subTitle: Option[String],
  link: Option[String],
  author: Option[String],
  keywords: Set[String],
  description: Option[String],
  media: Media) {

  def toEpisode(podcastInfo: EpisodePodcastInfo): Episode = Episode(
    podcast,
    uri,
    published,
    title,
    subTitle,
    link,
    author,
    keywords,
    description,
    media,
    podcastInfo
  )
}

object EpisodeSyncInfo extends MappingProvider[EpisodeSyncInfo] {

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
    "media" -> Media.mapping
  )(apply)(unapply)

  def jsonMapping(podcast: URI) = map(
    "uri" -> uri,
    "published" -> dateTime,
    "title" -> string,
    "subTitle" -> optional(string),
    "link" -> optional(string),
    "author" -> optional(string),
    "keywords" -> csvStrings,
    "description" -> optional(string),
    "media" -> Media.jsonMapping
  )(apply(podcast, _, _, _, _, _, _, _, _, _))(e => Some(e.uri, e.published, e.title, e.subTitle, e.link, e.author, e.keywords, e.description, e.media))
}
