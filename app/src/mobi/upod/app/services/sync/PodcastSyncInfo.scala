package mobi.upod.app.services.sync

import java.net.{URI, URL}

import mobi.upod.app.data._
import mobi.upod.data.MappingProvider
import org.joda.time.DateTime

final case class PodcastSyncInfo(
  uri: URI,
  url: URL,
  title: String,
  subTitle: Option[String],
  link: Option[String],
  authorName: Option[String],
  authorEmail: Option[String],
  categories: Set[Category],
  keywords: Set[String],
  description: Option[String],
  imageUrl: Option[URL],
  colors: Option[PodcastColors],
  modified: Option[DateTime],
  eTag: Option[String]) {

  def toPodcast: Podcast = Podcast(
    uri,
    url,
    title,
    subTitle,
    link,
    authorName,
    authorEmail,
    categories,
    keywords,
    description,
    imageUrl,
    colors,
    modified,
    eTag
  )

  def toPodcast(subscriptionSettings: SubscriptionSettings): Podcast = Podcast(
    uri,
    url,
    title,
    subTitle,
    link,
    authorName,
    authorEmail,
    categories,
    keywords,
    description,
    imageUrl,
    colors,
    modified,
    eTag,
    true,
    subscriptionSettings
  )

  def toEpisodePodcastInfo: EpisodePodcastInfo = EpisodePodcastInfo(
    0,
    url,
    title,
    imageUrl,
    colors
  )
}

object PodcastSyncInfo extends MappingProvider[PodcastSyncInfo] {

  import mobi.upod.data.Mapping._

  val mapping = map(
    "uri" -> uri,
    "url" -> url,
    "title" -> string,
    "subTitle" -> optional(string),
    "link" -> optional(string),
    "authorName" -> optional(string),
    "authorEmail" -> optional(string),
    "categories" -> set(Category.mapping),
    "keywords" -> set(string),
    "description" -> optional(string),
    "imageUrl" -> optional(url),
    "colors" -> PodcastColors.optionalMapping,
    "modified" -> optional(dateTime),
    "eTag" -> optional(string)
  )(apply)(unapply)


  val jsonMapping = map(
    "uri" -> uri,
    "url" -> url,
    "title" -> string,
    "subTitle" -> optional(string),
    "link" -> optional(string),
    "authorName" -> optional(string),
    "authorEmail" -> optional(string),
    "categories" -> Categories.mapping,
    "keywords" -> csvStrings,
    "description" -> optional(string),
    "imageUrl" -> optional(url),
    "colors" -> PodcastColors.optionalMapping,
    "modified" -> optional(dateTime),
    "eTag" -> optional(string)
  )(apply)(unapply)
}
