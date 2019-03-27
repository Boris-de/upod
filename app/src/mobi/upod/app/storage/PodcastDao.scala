package mobi.upod.app.storage

import java.net.{URI, URL}

import com.escalatesoft.subcut.inject.BindingModule
import mobi.upod.app.data._
import mobi.upod.app.services.sync.{PodcastFetchInfo, PodcastSyncInfo, Subscription, SubscriptionSettings}
import mobi.upod.data.Mapping
import mobi.upod.data.sql.SqlGenerator
import mobi.upod.sql.Sql
import mobi.upod.util.Cursor

import scala.collection.mutable

class PodcastDao(dbHelper: DatabaseHelper)(implicit bindingModule: BindingModule)
  extends Dao[Podcast](PodcastDao.table, dbHelper, Podcast.mapping) with IncludeNewEpisodesInLibraryCondition {
  import mobi.upod.app.storage.EpisodeDao.{cached => episodeCached, downloadComplete, episode, isNew, mediaType, playbackFinished, podcast => podcastRef, starred, uri => episodeUri}
  import mobi.upod.app.storage.PodcastDao._

  private val podcastSyncInfoSqlGenerator = SqlGenerator(PodcastSyncInfo)
  private lazy val episodeDao = inject[EpisodeDao]

  protected def columns = Map(
    id -> PRIMARY_KEY,
    uri -> UNIQUE_TEXT,
    url -> TEXT,
    title -> TEXT,
    subTitle -> TEXT,
    link -> TEXT,
    authorName -> TEXT,
    authorEmail -> TEXT,
    categories -> TEXT,
    keywords -> TEXT,
    description -> TEXT,
    imageUrl -> TEXT,
    backgroundColor -> INTEGER,
    keyColor -> INTEGER,
    flattrLink -> TEXT,
    modified -> INTEGER,
    eTag -> TEXT,
    subscribed -> INTEGER,
    settingsAutoAddToPlaylist -> INTEGER,
    settingsAutoAdd -> INTEGER,
    settingsAutoDownload -> INTEGER,
    settingsMaxKeptEpisodes -> INTEGER,
    settingsPlaybackSpeed -> REAL,
    settingsVolumeGain -> REAL,
    syncError -> TEXT,
    listed -> INTEGER
  )

  override protected def indices = Seq(
    Index(table, 'podcastUri, true, TextIndexColumn(uri)),
    Index(table, 'podcastTitle, false, TextIndexColumn(title)),
    Index(table, 'url, false, TextIndexColumn(url))
  )

  override protected[storage] def upgrade(database: Database, oldVersion: Int, newVersion: Int): Unit = {
    log.info(s"upgrading podcast table from $oldVersion to $newVersion")
  }

  def find(id: Long): Option[Podcast] = findOne(sql"SELECT * FROM $podcast WHERE ${PodcastDao.id}=$id")

  def find(uri: URI): Option[Podcast] = findOne(sql"SELECT * FROM $podcast WHERE ${PodcastDao.uri}=$uri")

  def find(url: URL): Option[Podcast] = findOne(sql"SELECT * FROM $podcast WHERE ${PodcastDao.url}=$url")

  def findReferences(ids: Traversable[URI]): Cursor[PodcastReference] =
    findMultiple(sql"SELECT $uri, $modified FROM $podcast WHERE $uri IN ($ids)", PodcastReference)

  private def findListItems(whereClause: Sql): Cursor[PodcastListItem] =
    findMultiple(sql"""
      SELECT
        $podcast.$id, $podcast.$uri, $podcast.$url, $podcast.$title, $podcast.$imageUrl, $podcast.$backgroundColor, $podcast.$keyColor, $podcast.$subscribed, $podcast.$syncError, COUNT($episode.$episodeUri) AS episodeCount
      FROM
        $podcast
        LEFT JOIN $episode ON $episode.$podcastRef = $podcast.$uri
      WHERE
        $whereClause
      GROUP BY
        $podcast.$uri
      HAVING
        episodeCount > 0
      ORDER BY
        $podcast.$title COLLATE NOCASE""",
      PodcastListItem)

  def findNewListItems = findListItems(sql"$episode.$isNew = 1")

  private def findUnfinishedLibraryListItemsWhere(condition: Sql): Cursor[PodcastListItem] =
    findListItems(sql"$includeNewEpisodesCondition AND $episode.$playbackFinished=0 AND $condition")

  def findUnfinishedLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"1=1")

  def findUnfinishedAudioLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"$episode.$mediaType<>'video'")

  def findUnfinishedVideoLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"$episode.$mediaType='video'")

  def findUnfinishedDownloadedLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"$episode.$downloadComplete<>0")

  def findRecentlyFinishedLibraryListItems = findListItems(episodeDao.recentlyFinished)

  def findStarredListItems = findListItems(sql"$episode.$starred <> 0")

  def findPodcastListItems: Cursor[PodcastListItem] =
    findMultiple(sql"""
      SELECT
        $podcast.$id, $podcast.$uri, $podcast.$url, $podcast.$title, $podcast.$imageUrl, $podcast.$backgroundColor, $podcast.$keyColor, $podcast.$subscribed, $podcast.$syncError, COUNT($episode.$episodeUri) AS episodeCount
      FROM
        $podcast
        LEFT JOIN $episode ON $episode.$podcastRef=$podcast.$uri AND $episode.$playbackFinished=0
      GROUP BY
        $podcast.$uri
      ORDER BY
        $podcast.$title COLLATE NOCASE""",
      PodcastListItem)

  def findSubscriptionUris: Cursor[URI] =
    findMultiple(sql"SELECT $uri FROM $podcast WHERE $subscribed<>0", Mapping.simpleMapping(uri.name, Mapping.uri))

  def findSubscriptionListItems: Cursor[PodcastListItem] = findMultiple(sql"""
    SELECT $id, $uri, $url, $title, $imageUrl, $backgroundColor, $keyColor, $subscribed, $syncError, 0 AS episodeCount
    FROM $podcast
    WHERE $subscribed<>0
    ORDER BY $title COLLATE NOCASE""",
    PodcastListItem
  )

  def listAllImageUrls: Cursor[URL] = findMultiple(
    sql"SELECT DISTINCT $imageUrl FROM $podcast WHERE $imageUrl IS NOT NULL",
    Mapping.simpleMapping(imageUrl.name -> Mapping.url)
  )

  def findWithMissingColor: Cursor[(URI, Option[URL])] = findMultiple(
    sql"SELECT $uri, $imageUrl FROM $podcast WHERE $backgroundColor IS NULL",
    Mapping.map("uri" -> Mapping.uri, "imageUrl" -> Mapping.optional(Mapping.url))((_, _))(t => Some(t))
  )

  def findFetchInfoFor(podcast: URI): Option[PodcastFetchInfo] = {
    findOne(
      sql"SELECT $uri, $url, $title, $modified, $eTag, $subscribed, $settingsAutoAdd, $settingsAutoAddToPlaylist, $settingsAutoDownload, $settingsMaxKeptEpisodes, $settingsPlaybackSpeed, $settingsVolumeGain FROM $table WHERE $uri=$podcast",
      PodcastFetchInfo)
  }

  def listFetchInfos: Cursor[PodcastFetchInfo] =
    findMultiple(sql"SELECT $uri, $url, $title, $modified, $eTag, $subscribed, $settingsAutoAdd, $settingsAutoAddToPlaylist, $settingsAutoDownload, $settingsMaxKeptEpisodes, $settingsPlaybackSpeed, $settingsVolumeGain FROM $podcast ORDER BY $title", PodcastFetchInfo)

  def update(podcastUri: URI, podcast: PodcastSyncInfo): Unit = {

    def colorColumnFilter(columnName: String): Boolean = columnName match {
      case keyColor.name => podcast.colors.isDefined
      case backgroundColor.name => podcast.colors.isDefined
      case _ => true
    }

    val valueAssignment = podcastSyncInfoSqlGenerator.generateUpdateValues(podcast, columnFilter = colorColumnFilter)
    execSql(sql"UPDATE $table SET $valueAssignment WHERE $uri=$podcastUri")
  }

  def updatePodcastColors(podcast: URI, colors: PodcastColors): Unit = {
    execSql(sql"UPDATE $table SET $backgroundColor=${colors.background.argb}, $keyColor=${colors.key.map(_.argb)} WHERE uri=$podcast")
    execSql(sql"UPDATE episode SET ${EpisodeDao.podcastBackgroundColor}=${colors.background.argb}, ${EpisodeDao.podcastKeyColor}=${colors.key.map(_.argb)} WHERE podcast=$podcast")
  }

  def markAllUnlisted(): Unit =
    execSql(sql"UPDATE $podcast SET $listed=0")

  /** Updates the specified subscriptions.
    *
    * @param subscriptions the subscriptions to be updated
    * @return the subscriptions which haven't been updated as they are unknown
    */
  def updateSubscriptions(subscriptions: TraversableOnce[Subscription]): Seq[Subscription] = {
    val unknownSubscriptions = mutable.Buffer[Subscription]()
    val statement = db.compileStatement(
      sql"UPDATE $podcast SET $subscribed=1, $settingsAutoAddToPlaylist=?, $settingsAutoAdd=?, $settingsAutoDownload=?, $settingsMaxKeptEpisodes=?, $settingsPlaybackSpeed=?, $settingsVolumeGain=?, $listed=1 WHERE $url=?")
    subscriptions foreach { s =>
      statement.bindLong(1, if (s.settings.autoAddToPlaylist) 1 else 0)
      statement.bindLong(2, if (s.settings.autoAddEpisodes) 1 else 0)
      statement.bindLong(3, if (s.settings.autoDownload) 1 else 0)
      s.settings.maxKeptEpisodes match {
        case Some(number) => statement.bindLong(4, number)
        case None => statement.bindNull(4)
      }
      s.settings.playbackSpeed match {
        case Some(number) => statement.bindDouble(5, number)
        case None => statement.bindNull(5)
      }
      s.settings.volumeGain match {
        case Some(number) => statement.bindDouble(6, number)
        case None => statement.bindNull(6)
      }
      statement.bindString(7, s.url.toString)
      if (statement.executeUpdateDelete() == 0) {
        unknownSubscriptions += s
      }
    }
    unknownSubscriptions
  }

  /** Marks the specified podcasts as not subscribed but still required.
    *
    * @param podcasts the URLs of the podcast to mark
    * @return the podcasts which haven't been marked as they are unknown
    */
  def markPodcastsNotSubscribedButListed(podcasts: TraversableOnce[URL]): Seq[URL] = {
    val unknownPodcasts = mutable.Buffer[URL]()
    val statement = db.compileStatement(
      sql"UPDATE $podcast SET $subscribed=0, $settingsAutoAddToPlaylist=0, $settingsAutoAdd=0, $settingsAutoDownload=0, $settingsMaxKeptEpisodes=NULL, $settingsPlaybackSpeed=NULL, $settingsVolumeGain=NULL, $listed=1 WHERE $url=?")
    podcasts foreach { p =>
      statement.bindString(1, p.toString)
      if (statement.executeUpdateDelete() == 0) {
        unknownPodcasts += p
      }
    }
    unknownPodcasts
  }

  def deleteUnlistedPodcasts(): Unit = {
    execSql(sql"DELETE FROM $episode WHERE podcast IN (SELECT $uri FROM $podcast WHERE $listed=0)")
    execSql(sql"DELETE FROM $podcast WHERE $listed=0")
  }

  private def subscribe(uri: URI, subscribe: Boolean) {
    execSql(sql"UPDATE $podcast SET $subscribed=$subscribe WHERE ${PodcastDao.uri}=$uri")
  }

  def subscribe(uri: URI) {
    subscribe(uri, true)
  }

  def unsubscribe(uri: URI) {
    subscribe(uri, false)
  }

  def delete(uri: URI): Unit =
    execSql(sql"DELETE FROM $podcast WHERE ${PodcastDao.uri}=$uri")

  def updateSettings(uri: URI, settings: SubscriptionSettings): Unit = {
    execSql(sql"""UPDATE $podcast SET
        $settingsAutoAddToPlaylist=${settings.autoAddToPlaylist},
        $settingsAutoAdd=${settings.autoAddEpisodes},
        $settingsAutoDownload=${settings.autoDownload},
        $settingsMaxKeptEpisodes=${settings.maxKeptEpisodes},
        $settingsPlaybackSpeed=${settings.playbackSpeed},
        $settingsVolumeGain=${settings.volumeGain}
      WHERE ${PodcastDao.uri}=$uri""")
  }

  def getAudioEffects(id: Long): Option[AudioEffects] = {
    import mobi.upod.data.Mapping._
    val mapping = map(
      settingsPlaybackSpeed.name -> optional(float),
      settingsVolumeGain.name -> optional(float)
    )(AudioEffects.apply)(AudioEffects.unapply)

    findOne(
      sql"SELECT $settingsPlaybackSpeed, $settingsVolumeGain FROM $podcast WHERE ${PodcastDao.id}=$id",
      mapping
    )
  }

  def updatePlaybackSpeed(id: Long, speed: Float): Unit =
    execSql(sql"UPDATE $podcast SET $settingsPlaybackSpeed=$speed WHERE ${PodcastDao.id}=$id")

  def resetPlaybackSpeed(id: Long): Unit =
    execSql(sql"UPDATE $podcast SET $settingsPlaybackSpeed=NULL WHERE ${PodcastDao.id}=$id")

  def updateVolumeGain(id: Long, gain: Float): Unit =
    execSql(sql"UPDATE $podcast SET $settingsVolumeGain=$gain WHERE ${PodcastDao.id}=$id")

  def resetVolumeGain(id: Long): Unit =
    execSql(sql"UPDATE $podcast SET $settingsVolumeGain=NULL WHERE ${PodcastDao.id}=$id")

  def setSyncError(podcastUrl: URL, error: String): Unit =
    execUpdateOrDelete(sql"UPDATE $podcast SET $syncError=$error WHERE $url=$podcastUrl")

  def resetSyncError(podcastUrl: URL): Unit =
    execUpdateOrDelete(sql"UPDATE $podcast SET $syncError=NULL WHERE $url=$podcastUrl")

  def deleteNotIn(ids: Traversable[URI]) {
    deleteWhere(sql"$uri NOT IN ($ids)")
  }

  def deleteUnreferenced() {
    deleteWhere(sql"$subscribed=0 AND $uri NOT IN (SELECT $podcastRef FROM $episode WHERE $episodeCached=0 GROUP BY $podcastRef)")
  }
}

object PodcastDao extends DaoObject {
  val table = Table('podcast)
  val podcast = table

  val id = Column('id)
  val uri = Column('uri)
  val url = Column('url)
  val title = Column('title)
  val subTitle = Column('subTitle)
  val link = Column('link)
  val authorName = Column('authorName)
  val authorEmail = Column('authorEmail)
  val categories = Column('categories)
  val keywords = Column('keywords)
  val description = Column('description)
  val imageUrl = Column('imageUrl)
  val backgroundColor = Column('colors_background)
  val keyColor = Column('colors_key)
  val flattrLink = Column('flattrLink)
  val modified = Column('modified)
  val eTag = Column('eTag)
  val subscribed = Column('subscribed)
  val settingsAutoAddToPlaylist = Column('settings_autoAddToPlaylist)
  val settingsAutoAdd = Column('settings_autoAddEpisodes)
  val settingsAutoDownload = Column('settings_autoDownload)
  val settingsMaxKeptEpisodes = Column('settings_maxKeptEpisodes)
  val settingsPlaybackSpeed = Column('settings_playbackSpeed)
  val settingsVolumeGain = Column('settings_volumeGain)
  val syncError = Column('syncError)
  val listed = Column('listed)
}
