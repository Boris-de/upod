package mobi.upod.app.services.sync

import java.net.URL

import com.google.code.rome.android.repackaged.com.sun.syndication.feed.module.itunes.FeedInformation
import com.google.code.rome.android.repackaged.com.sun.syndication.feed.module.itunes.types.{Category => ITunesCategory}
import com.google.code.rome.android.repackaged.com.sun.syndication.feed.synd.SyndFeed
import mobi.upod.app.data.Category
import mobi.upod.net.UriUtils
import mobi.upod.syndication.CacheInfo

import scala.collection.JavaConverters._
import scala.util.Try

private[sync] object PodcastParser {

   def createPodcast(feed: SyndFeed, url: URL, cacheInfo: Option[CacheInfo]): PodcastSyncInfo = {
     val iTunesInfo = Option(feed.getModule("http://www.itunes.com/dtds/podcast-1.0.dtd").asInstanceOf[FeedInformation])

     val safeTitle = {

       def urlToTitle: String = {
         val host = url.getHost match {
           case h if h.startsWith("www.") => h.drop(4)
           case h => h
         }
         host + url.getPath
       }

       feed.getTitle match {
         case null => urlToTitle
         case t => t
       }
     }

     def iTunesOption[A](property: FeedInformation => A): Option[A] = iTunesInfo.flatMap(i => Try(property(i)).toOption) match {
       case Some(null) => None
       case option => option
     }

     def mergeOption[A](preferred: Option[A], fallback: => Option[A]): Option[A] = {
       preferred match {
         case None => fallback
         case option => option
       }
     }

     def getLanguage: Option[String] = Option(feed.getLanguage) match {
       case Some(language) =>
         Some(language.split('-')(0).take(2).toLowerCase)
       case None =>
         None
     }

     new PodcastSyncInfo(
       UriUtils.createFromUrl(url),
       url,
       safeTitle,
       iTunesOption(_.getSubtitle),
       Option(feed.getLink),
       mergeOption(iTunesOption(_.getOwnerName), Option(feed.getAuthor)),
       iTunesOption(_.getOwnerEmailAddress),
       iTunesInfo.map(_.getCategories.asScala.map(category => Category(category.asInstanceOf[ITunesCategory])).toSet).getOrElse(Set()),
       iTunesInfo.map(_.getKeywords.map(_.trim).toSet).getOrElse(Set()),
       mergeOption(iTunesOption(_.getSummary), Option(feed.getDescription)),
       getFeedImageUrl(iTunesOption(_.getImage), Option(feed.getImage).flatMap(img => Try(new URL(url, img.getUrl)).toOption)),
       None,
       cacheInfo.flatMap(_.lastModified),
       cacheInfo.flatMap(_.eTag)
     )
   }

   private def getFeedImageUrl(preferredImage: Option[URL], fallbackImage: Option[URL]): Option[URL] = {

     def isValidImage(imageUrl: Option[URL]) =
       imageUrl.isDefined

     if (isValidImage(preferredImage))
       preferredImage
     else if (isValidImage(fallbackImage))
       fallbackImage
     else
       None
   }

 }
