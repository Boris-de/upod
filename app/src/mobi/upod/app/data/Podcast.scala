package mobi.upod.app.data

import java.net.{URI, URL}
import mobi.upod.data.{Mapping, MappingProvider}
import org.joda.time.DateTime
import mobi.upod.app.services.sync.SubscriptionSettings

final case class Podcast(
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
  eTag: Option[String],
  subscribed: Boolean = false,
  settings: SubscriptionSettings = SubscriptionSettings.default,
  syncError: Option[String] = None,
  rowId: Option[Long] = None)
  extends PodcastBase {

  override def id: Long =
    rowId.getOrElse(0)

  def subTitleDifferentToTitle: Option[String] =
    subTitle.flatMap(st => if (st != title) Some(st) else None)

  def longestOfDescriptionAndSubTitle: Option[String] =
    Seq(subTitle, description).maxBy(_.map(_.length).getOrElse(0))
}

object Podcast extends MappingProvider[Podcast] {

  import Mapping._

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
    "eTag" -> optional(string),
    "subscribed" -> boolean,
    "settings" -> SubscriptionSettings.mapping,
    "syncError" -> optional(string),
    "id" -> optional(long)
  )(apply)(unapply)
}

