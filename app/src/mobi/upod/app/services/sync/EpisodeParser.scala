package mobi.upod.app.services.sync

import java.net.{URI, URL}

import com.google.code.rome.android.repackaged.com.sun.syndication.feed.module.content.ContentModule
import com.google.code.rome.android.repackaged.com.sun.syndication.feed.module.itunes.EntryInformation
import com.google.code.rome.android.repackaged.com.sun.syndication.feed.synd.{SyndEnclosure, SyndEntry}
import mobi.upod.app.data._
import mobi.upod.net.UriUtils
import org.joda.time.DateTime

import scala.collection.JavaConverters._
import scala.util.Try

private[sync] object EpisodeParser {

  private def getFirstMediaEnclosure(podcastUrl: URL, entry: SyndEntry): Option[SyndEnclosure] = {
    entry.getEnclosures.asScala collectFirst {
      case e: SyndEnclosure if MimeType(e).exists(_.isPlayable) && Try(new URL(podcastUrl, e.getUrl)).isSuccess => e
    }
  }

  def isValidEpisode(podcastUrl: URL, entry: SyndEntry): Boolean =
    getFirstMediaEnclosure(podcastUrl, entry).isDefined

  def createEpisode(podcast: PodcastSyncInfo, episode: SyndEntry, mediaUrlForUri: Boolean): EpisodeSyncInfo = {
    require(isValidEpisode(podcast.url, episode))

    val iTunesInfo = Option(episode.getModule("http://www.itunes.com/dtds/podcast-1.0.dtd").asInstanceOf[EntryInformation])
    val content = Option(episode.getModule("http://purl.org/rss/1.0/modules/content/").asInstanceOf[ContentModule])

    def iTunesOption[A](property: EntryInformation => A): Option[A] = iTunesInfo.map(property) match {
      case Some(null) => None
      case option => option
    }

    def contentOption: Option[String] = content match {
      case Some(c) if !c.getEncodeds.isEmpty =>
        Some(c.getEncodeds.get(0).asInstanceOf[String])
      case _ => None
    }

    def mergeOption(preferred: Option[String], fallback: => Option[String]): Option[String] = {
      preferred match {
        case Some(option) if !option.isEmpty => Some(option)
        case _ => fallback
      }
    }

    def nonEmptyTextOption(text: String): Option[String] =
      Option(text).flatMap(t => if (t.isEmpty) None else Some(t))

    val enclosure = getFirstMediaEnclosure(podcast.url, episode).get
    val media = Media(
      new URL(podcast.url, enclosure.getUrl),
      MimeType(enclosure).get,
      enclosure.getLength,
      iTunesOption(_.getDuration).map(_.getMilliseconds).getOrElse(0))
    val uri = if (mediaUrlForUri) new URI(media.url.toString.replaceAll(" ", "%20")) else UriUtils.create(Option(episode.getUri), Some(media.url))
    new EpisodeSyncInfo(
      podcast.uri,
      uri,
      new DateTime(episode.getPublishedDate),
      nonEmptyTextOption(episode.getTitle).getOrElse("<no title>"),
      iTunesOption(_.getSubtitle),
      Option(episode.getLink),
      mergeOption(iTunesOption(_.getAuthor), Option(episode.getAuthor)),
      iTunesInfo.map(_.getKeywords.toSet).getOrElse(Set()),
      mergeOption(contentOption, mergeOption(Option(episode.getDescription).map(_.getValue), iTunesOption(_.getSummary))),
      media
    )
  }

  def list(podcast: PodcastSyncInfo, entries: IndexedSeq[SyndEntry]): IndexedSeq[EpisodeSyncInfo] = {
    val episodes = entries.filter(isValidEpisode(podcast.url, _))
    val hasUniqueGuids = {
      val distinctGuidCount = episodes.map(_.getUri).distinct.size
      distinctGuidCount == episodes.size || distinctGuidCount > 1
    }
    episodes.map(createEpisode(podcast, _, !hasUniqueGuids))
  }
}
