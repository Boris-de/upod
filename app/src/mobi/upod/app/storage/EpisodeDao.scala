package mobi.upod.app.storage

import java.net.{URI, URL}

import android.database.sqlite.{SQLiteConstraintException, SQLiteStatement}
import com.escalatesoft.subcut.inject.BindingModule
import com.github.nscala_time.time.Imports._
import com.google.gson.{JsonParser, JsonElement}
import mobi.upod.android.database.{Cursor => DbCursor}
import mobi.upod.app.EpisodeCounterItem
import mobi.upod.app.data.EpisodeStatus.EpisodeStatus
import mobi.upod.app.data._
import mobi.upod.app.services.sync.{EpisodePlaybackInfo, EpisodeSyncInfo}
import mobi.upod.data.Mapping
import mobi.upod.data.json.{JsonWriter, JsonStreamReader, JsonReader}
import mobi.upod.data.sql.{SqlGenerator, SqlReader}
import mobi.upod.media.MediaChapter
import mobi.upod.net.UriUtils
import mobi.upod.sql.{ColumnList, Sql}
import mobi.upod.util.Cursor
import org.joda.time.DateTime


class EpisodeDao(dbHelper: DatabaseHelper)(implicit bindingModule: BindingModule)
  extends Dao[Episode](EpisodeDao.table, dbHelper, Episode) with IncludeNewEpisodesInLibraryCondition {

  import mobi.upod.app.storage.EpisodeDao._

  private val episodeSyncInfoSqlGenerator = SqlGenerator(EpisodeSyncInfo)

  protected def columns = Map(
    id -> PRIMARY_KEY,
    eid -> UNIQUE_TEXT,
    uri -> TEXT,
    podcast -> TEXT,
    published -> INTEGER,
    title -> TEXT,
    subTitle -> TEXT,
    link -> TEXT,
    author -> TEXT,
    keywords -> TEXT,
    description -> TEXT,
    mediaUrl -> TEXT,
    mediaType -> TEXT,
    mediaSubType -> TEXT,
    mediaLength -> INTEGER,
    mediaDuration -> INTEGER,
    flattrLink -> TEXT,
    podcastId -> INTEGER,
    podcastUrl -> TEXT,
    podcastTitle -> TEXT,
    podcastImageUrl -> TEXT,
    podcastBackgroundColor -> INTEGER,
    podcastKeyColor -> INTEGER,
    isNew -> INTEGER,
    library -> INTEGER,
    starred -> INTEGER,
    cached -> INTEGER,
    downloadListPosition -> INTEGER,
    downloadFile -> TEXT,
    downloadComplete -> INTEGER,
    downloadedBytes -> INTEGER,
    downloadAttempts -> INTEGER,
    lastDownloadErrorText -> TEXT,
    playlistPosition -> INTEGER,
    playbackFinished -> INTEGER,
    playbackPosition -> INTEGER,
    playbackSpeed -> REAL,
    volumeGain -> REAL,
    playbackPositionTimestamp -> INTEGER,
    listed -> INTEGER,
    chapters -> TEXT
  )

  private val listItemColumns = ColumnList(
    id,
    uri,
    published,
    title,
    link,
    mediaUrl,
    mediaType,
    mediaSubType,
    mediaLength,
    mediaDuration,
    flattrLink,
    podcast,
    podcastId,
    podcastUrl,
    podcastTitle,
    podcastImageUrl,
    podcastBackgroundColor,
    podcastKeyColor,
    isNew,
    library,
    starred,
    downloadListPosition,
    downloadFile,
    downloadComplete,
    downloadedBytes,
    downloadAttempts,
    lastDownloadErrorText,
    playlistPosition,
    playbackFinished,
    playbackPosition,
    playbackSpeed,
    volumeGain,
    playbackPositionTimestamp
  )

  private val mediaFileSourceAttributeColumns = ColumnList(
    mediaUrl,
    podcast,
    podcastTitle,
    uri,
    title,
    published
  )

  override protected def generatedColumns(entity: Episode): Seq[(String, String)] =
    Seq(eid.name -> sql"${EpisodeId.toString(entity.podcast, entity.uri)}")

  override protected def indices = Seq(
    Index(table, 'episodeEid, true, TextIndexColumn(eid)),
    Index(table, 'episodePodcast, false, TextIndexColumn(podcast)),
    Index(table, 'episodeLibrary, false, IntIndexColumn(library)),
    Index(table, 'episodeNew, false, IntIndexColumn(isNew)),
    Index(table, 'episodePodcastLibrary, false, IntIndexColumn(podcastId), IntIndexColumn(library)),
    Index(table, 'episodePodcastNew, false, IntIndexColumn(podcastId), IntIndexColumn(isNew)),
    Index(table, 'episodeStarred, false, IntIndexColumn(starred)),
    Index(table, 'episodePodcastStarred, false, IntIndexColumn(podcastId), IntIndexColumn(starred)),
    Index(table, 'episodeNewLibrary, false, IntIndexColumn(library), IntIndexColumn(published)),
    Index(table, 'episodeNewPodcastLibrary, false, IntIndexColumn(podcastId), IntIndexColumn(library), IntIndexColumn(published)),
    Index(table, 'episodePodcastGrouped, false, TextIndexColumn(podcastTitle), IntIndexColumn(podcastId), IntIndexColumn(published)),
    Index(table, 'episodePlaylist, true, IntIndexColumn(playlistPosition)),
    Index(table, 'episodeDownloadList, true, IntIndexColumn(downloadListPosition)),
    Index(table, 'episodeMediaType, false, TextIndexColumn(mediaType)),
    Index(table, 'episodeDownloadCompleted, false, IntIndexColumn(downloadComplete))
  )

  override protected val triggers = Seq(
    Trigger(
      'resetDownloadInfo,
      sql"AFTER UPDATE OF $mediaUrl ON $episode FOR EACH ROW WHEN NEW.$mediaUrl IS NOT OLD.$mediaUrl",
      sql"UPDATE $episode SET $downloadComplete=0, $downloadedBytes=0, $downloadAttempts=0, $lastDownloadErrorText=NULL WHERE $eid=NEW.$eid")
  )

  private val idMapping = Mapping.simpleMapping(id.name, Mapping.long)

  override protected[storage] def upgrade(database: Database, oldVersion: Int, newVersion: Int): Unit = {
    log.info(s"upgrading episode table from $oldVersion to $newVersion")
  }

  def calculateNewEpisodesHash: EpisodeListHash =
    findOne(sql"SELECT COUNT(id) count, ifnull(SUM(id), 0) hash FROM $episode WHERE $isNew=1", EpisodeListHash).get

  def readLibraryEpisodeCount: Int =
    findOne(sql"SELECT COUNT(id) count FROM $episode WHERE $playbackFinished=0 AND $includeNewEpisodesCondition", Mapping.simpleMapping("count" -> Mapping.int)).get

  def find(id: EpisodeId): Option[Episode] = findOne(sql"SELECT * FROM $table WHERE $eid=$id")

  def find(key: Long): Option[Episode] = findOne(sql"SELECT * FROM $table WHERE $id=$key")

  def findCounterItem: Option[EpisodeCounterItem] = {

    def countIf(condition: Sql): Sql =
      sql"ifnull(sum(case when $condition then 1 else 0 end), 0)"

    def countUnfinishedIf(condition: Sql): Sql =
      countIf(sql"$playbackFinished=0 AND $includeNewEpisodesCondition AND $condition")

    findOne(sql"""SELECT
      ${countIf(sql"$isNew=1")} isNew,
      ${countUnfinishedIf(sql"1=1")} unfinished,
      ${countUnfinishedIf(sql"$mediaType<>'video'")} audio,
      ${countUnfinishedIf(sql"$mediaType='video'")} video,
      ${countUnfinishedIf(sql"$downloadComplete<>0")} downloaded,
      ${countIf(recentlyFinished)} finished,
      ${countIf(sql"$starred")} starred,
      count($playlistPosition) playlist,
      count($downloadListPosition) downloadQueue
      FROM $episode WHERE $cached=0""", EpisodeCounterItem)
  }

  def findUnfinishedNewAndLibraryIds: Cursor[Long] =
    findMultiple(sql"SELECT $id FROM $episode WHERE ($isNew=1 OR $library=1) AND $playbackFinished=0", idMapping)

  def findUnfinishedLibraryIds: Cursor[Long] =
    findMultiple(sql"SELECT $id FROM $episode WHERE $library=1 AND $playbackFinished=0", idMapping)

  def findPlaylistIds: Cursor[Long] =
    findMultiple(sql"SELECT $id FROM $episode WHERE $playlistPosition IS NOT NULL", idMapping)

  private def findListItems(whereClause: Sql, sortAscending: Boolean = false): Cursor[EpisodeListItem] = {
    val order = if (sortAscending) sql"ASC" else sql"DESC"
    findMultiple( sql"""
      SELECT $listItemColumns
      FROM $episode
      WHERE $whereClause
      ORDER BY $published $order""",
      EpisodeListItem)
  }

  def findListItemById(id: Long): Option[EpisodeListItem] = findListItems(sql"${EpisodeDao.id}=$id").nextAndClose()

  def findListItemsByIds(ids: Traversable[Long]) = findListItems(sql"$id IN ($ids)")

  def findListItemByUrl(url: URL): Option[EpisodeListItem] = findListItems(sql"$mediaUrl=$url").nextAndClose()

  def findNewListItems = findListItems(sql"$isNew = 1")

  def findNewListItems(podcast: Long, sortAscending: Boolean) =
    findListItems(sql"$podcastId = $podcast AND $isNew = 1", sortAscending)

  private def findUnfinishedLibraryListItemsWhere(condition: Sql): Cursor[EpisodeListItem] =
    findListItems(sql"$includeNewEpisodesCondition AND $playbackFinished=0 AND $condition")

  private def findUnfinishedLibraryListItemsWhere(podcast: Long, condition: Sql, sortAscending: Boolean = false): Cursor[EpisodeListItem] =
    findListItems(sql"$podcastId = $podcast AND $includeNewEpisodesCondition AND $playbackFinished=0 AND $condition", sortAscending)

  def findUnfinishedLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"1=1")

  def findUnfinishedLibraryListItems(podcast: Long, sortAscending: Boolean) =
    findUnfinishedLibraryListItemsWhere(podcast, sql"1=1", sortAscending)

  def findUnfinishedAudioLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"$mediaType<>'video'")

  def findUnfinishedAudioLibraryListItems(podcast: Long, sortAscending: Boolean) =
    findUnfinishedLibraryListItemsWhere(podcast, sql"$mediaType<>'video'", sortAscending)

  def findUnfinishedVideoLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"$mediaType='video'")

  def findUnfinishedVideoLibraryListItems(podcast: Long, sortAscending: Boolean) =
    findUnfinishedLibraryListItemsWhere(podcast, sql"$mediaType='video'", sortAscending)

  def findUnfinishedDownloadedLibraryListItems = findUnfinishedLibraryListItemsWhere(sql"$downloadComplete<>0")

  def findUnfinishedDownloadedLibraryListItems(podcast: Long, sortAscending: Boolean) =
    findUnfinishedLibraryListItemsWhere(podcast, sql"$downloadComplete<>0", sortAscending)

  private[storage] val recentlyFinished =
    sql"($cached=0 AND $playbackFinished=1 AND $downloadedBytes>0 AND $playbackPositionTimestamp IS NOT NULL AND $NOW-$playbackPositionTimestamp<24*60*60*1000)"

  def findRecentlyFinishedLibraryListItems = findListItems(recentlyFinished)

  def findRecentlyFinishedLibraryListItems(podcast: Long, sortAscending: Boolean) =
    findListItems(sql"$podcastId=$podcast AND $recentlyFinished", sortAscending)

  def findDownloadFilesToKeep: Cursor[String] = {
    findMultiple(
      sql"""
        SELECT $downloadFile
        FROM $episode
        WHERE
          $cached=0 AND
          $downloadFile IS NOT NULL AND
          ($downloadedBytes>0 OR $downloadListPosition IS NOT NULL) AND
          ($playbackFinished=0 OR $recentlyFinished)""",
      Mapping.simpleMapping(downloadFile.name -> Mapping.string)
    )
  }

  def findStarredListItems = findListItems(sql"$starred <> 0")

  def findStarredListItems(podcast: Long, sortAscending: Boolean) =
    findListItems(sql"$podcastId = $podcast AND $starred <> 0", sortAscending)

  def findOnlinePodcastListItems(podcast: Long, sortAscending: Boolean) =
    findListItems(sql"$podcastId=$podcast", sortAscending)

  private def findGroupedListItems(whereClause: Sql): Cursor[EpisodeListItem] =
    findMultiple(sql"""
      SELECT $listItemColumns
      FROM $episode
      WHERE $whereClause
      ORDER BY $podcastTitle COLLATE NOCASE, $podcastId, $published DESC""",
      EpisodeListItem)

  def findGroupedNewListItems = findGroupedListItems(sql"$isNew = 1")

  private def findGroupedUnfinishedLibraryListItemsWhere(condition: Sql) =
    findGroupedListItems(sql"$includeNewEpisodesCondition AND $playbackFinished=0 AND $condition")

  def findGroupedUnfinishedLibraryListItems = findGroupedUnfinishedLibraryListItemsWhere(sql"1=1")

  def findGroupedUnfinishedAudioLibraryListItems = findGroupedUnfinishedLibraryListItemsWhere(sql"$mediaType<>'video'")

  def findGroupedUnfinishedVideoLibraryListItems = findGroupedUnfinishedLibraryListItemsWhere(sql"$mediaType='video'")

  def findGroupedUnfinishedDownloadedLibraryListItems = findGroupedUnfinishedLibraryListItemsWhere(sql"$downloadComplete<>0")

  def findGroupedStarredListItems = findGroupedListItems(sql"$starred <> 0")

  def findGroupedRecentlyFinishedLibraryListItems = findGroupedListItems(recentlyFinished)

  def findRandomNonPlaylistLibraryEpisodeIdPreferringDownloaded: Option[Long] = findOne(sql"""
    SELECT $id
    FROM $episode
    WHERE
      $includeNewEpisodesCondition AND
      $playbackFinished=0 AND
      $playlistPosition IS NULL
    ORDER BY
      $downloadComplete DESC,
      RANDOM()
    LIMIT 1""",
    idMapping
  )

  def findAllPodcastEpisodeUris(podcast: URL): Cursor[URI] =
    findMultiple(sql"SELECT $uri FROM $episode WHERE $podcastUrl=$podcast", Mapping.simpleMapping(uri.name -> Mapping.uri))

  def markUncached(podcastUri: URI): Unit =
    execSql(sql"UPDATE $episode SET $cached=0 WHERE $podcast=$podcastUri")

  def markAllUncached(): Unit = {
    execSql(sql"UPDATE $episode SET $cached=0")
  }

  def deleteCached() {
    deleteWhere(sql"$cached<>0")
  }

  def deletePodcastNewEpisodes(podcast: URI) {
    deleteWhere(sql"$isNew=1 AND ${EpisodeDao.podcast}=$podcast")
  }

  def deletePodcastEpisodes(podcast: URI): Unit =
    deleteWhere(sql"${EpisodeDao.podcast}=$podcast")

  def updatePodcastProperties(): Unit = {
    execSql(sql"""UPDATE $episode SET
      $podcastId=(SELECT p.id FROM Podcast p WHERE p.uri=$podcast),
      $podcastUrl=(SELECT p.url FROM Podcast p WHERE p.uri=$podcast),
      $podcastTitle=(SELECT p.title FROM Podcast p WHERE p.uri=$podcast),
      $podcastImageUrl=(SELECT p.imageUrl FROM Podcast p WHERE p.uri=$podcast),
      $podcastBackgroundColor=(SELECT p.colors_background FROM Podcast p WHERE p.uri=$podcast),
      $podcastKeyColor=(SELECT p.colors_key FROM Podcast p WHERE p.uri=$podcast)""")
    execSql(sql"""DELETE FROM $episode WHERE $podcastId IS NULL""")
  }

  def updatePodcastProperties(podcastUri: URI) {
    execSql(sql"""
      UPDATE $episode SET
      $podcastId=(SELECT p.id FROM Podcast p WHERE p.uri=$podcast),
      $podcastUrl=(SELECT p.url FROM Podcast p WHERE p.uri=$podcast),
      $podcastTitle=(SELECT p.title FROM Podcast p WHERE p.uri=$podcast),
      $podcastImageUrl=(SELECT p.imageUrl FROM Podcast p WHERE p.uri=$podcast),
      $podcastBackgroundColor=(SELECT p.colors_background FROM Podcast p WHERE p.uri=$podcast),
      $podcastKeyColor=(SELECT p.colors_key FROM Podcast p WHERE p.uri=$podcast)
      WHERE $podcast=$podcastUri""")
  }

  def deleteEpisodesWithoutPodcast(): Unit =
    execSql(sql"DELETE FROM $episode WHERE $podcast NOT IN (SELECT p.uri FROM podcast p)")

  def removeAllFromLibrary() {
    execSql(sql"UPDATE $episode SET $library=0, $starred=0")
  }

  def addAllNewEpisodesToLibrary(): Unit = {
    execSql(sql"UPDATE $episode SET $isNew=0, $library=1 WHERE $isNew=1")
  }

  def addToLibrary(ids: Traversable[Long]) {
    execSql(sql"""
      UPDATE $episode SET
        $isNew=0,
        $library=1,
        $cached=0,
        $playbackFinished=0,
        $playbackPositionTimestamp=$NOW
      WHERE
        $id IN ($ids)""")
  }

  def addToLibraryByIds(ids: Traversable[EpisodeId]) {
    execSql(sql"""
      UPDATE $episode SET
        $isNew=0,
        $library=1,
        $cached=0,
        $playbackFinished=0,
        $playbackPositionTimestamp=$NOW
      WHERE
        $eid IN ($ids)""")
  }

  def addUnfinishedToLibrary(podcast: Long): Unit = {
    execSql(sql"""
      UPDATE $episode SET
        $isNew=0,
        $library=1,
        $cached=0
      WHERE
        $podcastId=$podcast""")
  }

  def insertFullOrUpdateWithoutDownloadInfo(e: Episode): Unit = {
    try {
      insertOrFail(e)
    } catch {
      case ex: SQLiteConstraintException =>
        val mediaFileUrl = findOne(sql"SELECT $mediaUrl FROM $episode WHERE $eid=${e.episodeId}", Mapping.simpleMapping(mediaUrl.name, Mapping.url))
        val updatedEpisode = if (mediaFileUrl.contains(e.media.url)) {
          val downloadInfo = findOne(
            sql"""SELECT
              $downloadListPosition listPosition,
              $downloadFile file,
              $downloadComplete complete,
              $downloadedBytes fetchedBytes,
              $downloadAttempts attempts,
              $lastDownloadErrorText lastError_text
              FROM $episode
              WHERE $eid=${e.episodeId}""",
            EpisodeDownloadInfo)
          e.copy(downloadInfo = downloadInfo.get)
        } else {
          e
        }
        save(updatedEpisode)
    }
  }

  def updateOrInsertListed(updateInfo: EpisodeSyncInfo, insertInfo: => Episode): Unit = {
    val values = episodeSyncInfoSqlGenerator.generateUpdateValues(updateInfo, columnFilter = {
      case mediaDuration.name => false
      case mediaLength.name => false
      case _ => true
    })
    val updateCount = execUpdate(sql"UPDATE $table SET $values, $listed=1 where $eid=${EpisodeId.toString(updateInfo.podcast, updateInfo.uri)}")
    if (updateCount == 0) {
      insertOrFail(insertInfo)
    }
  }

  /** Same as #updateOrInsertListed but only for one time update to app version 4.4.0 where episode URIs may have changed. */
  @deprecated(message = "Only for one time update to app version 4.4.0 -- use updateOrInsertListed() instead", since = "4.4.0")
  def updateOrInsertListedWithPotentialNewUri(updateInfo: EpisodeSyncInfo, insertInfo: => Episode): Unit = {
    val oldUri = new URI(updateInfo.media.url.toString.replaceAll(" ", "%20"))
    val oldEid = EpisodeId.toString(updateInfo.podcast, oldUri)
    val newEid = EpisodeId.toString(updateInfo.podcast, updateInfo.uri)
    val values = episodeSyncInfoSqlGenerator.generateUpdateValues(updateInfo, columnFilter = {
      case mediaDuration.name => false
      case mediaLength.name => false
      case _ => true
    })
    val updateCount = execUpdate(sql"UPDATE $table SET $values, $eid=$newEid, $listed=1 where $eid=$oldEid")
    if (updateCount == 0) {
      insertOrFail(insertInfo)
    }
  }

  def updateEpisodeStatus(episodes: TraversableOnce[(EpisodeId, EpisodeStatus, Option[PlaybackInfo])]): Unit = {
    implicit val statement = prepareEpisodeStatusUpdate
    try {
      episodes foreach { case (episodeId, status, playbackInfo) =>
        updateEpisodeStatus(episodeId, status, playbackInfo)
      }
    } finally {
      statement.close()
    }
  }

  private def prepareEpisodeStatusUpdate: SQLiteStatement = {
    db.compileStatement(
      sql"""UPDATE $table SET
             $isNew=?,
             $library=?,
             $starred=?,
             $playbackFinished=?,
             $playbackPosition=?,
             $playbackSpeed=?,
             $volumeGain=?,
             $playbackPositionTimestamp=?,
             $mediaDuration=max($mediaDuration, ?)
            WHERE $eid=?""")
  }

  private def updateEpisodeStatus(eid: EpisodeId, status: EpisodeStatus, playbackInfo: Option[PlaybackInfo])(implicit statement: SQLiteStatement): Unit = {

    def longOf(flag: Boolean): Long = if (flag) 1 else 0

    def bindFlag(index: Int, flag: Boolean): Unit =
      statement.bindLong(index, longOf(flag))

    def bindLong(index: Int, num: Long): Unit =
      statement.bindLong(index, num)

    def bindString(index: Int, str: String): Unit =
      statement.bindString(index, str)

    def bindOptionalFloat(index: Int, num: Option[Float]): Unit = num match {
      case Some(f) => statement.bindDouble(index, f)
      case None => statement.bindNull(index)
    }

    def bindTimestamp(index: Int, timestamp: DateTime): Unit =
      statement.bindLong(index, timestamp.getMillis)

    val pi = playbackInfo.getOrElse(PlaybackInfo.default)
    val newState = status == EpisodeStatus.New && !pi.finished
    val libraryState = status == EpisodeStatus.Library || status == EpisodeStatus.Starred
    val starredState = status == EpisodeStatus.Starred
    val finishedState = status == EpisodeStatus.Finished || pi.finished

    bindFlag(1, newState)
    bindFlag(2, libraryState)
    bindFlag(3, starredState)
    bindFlag(4, finishedState)
    bindLong(5, pi.position)
    bindOptionalFloat(6, pi.speed)
    bindOptionalFloat(7, pi.gain)
    bindTimestamp(8, pi.modified)
    bindLong(9, pi.duration)
    bindString(10, eid.toString)

    statement.executeUpdateDelete()
  }

  def findNewestNotNewPublishedTimestamp(podcast: URL): Option[DateTime] = {
    findOne(
      sql"SELECT MAX($published) maxPublished FROM $episode WHERE $podcastUrl=$podcast AND $isNew=0",
      Mapping.simpleMapping("maxPublished" -> Mapping.optional(Mapping.dateTime))).flatten
  }

  def markOldUnknownEpisodesFinished(podcastUri: URI, newestNotNewPublishedTimestamp: Option[DateTime]): Unit = {
    execSql(
      sql"""UPDATE $table SET $playbackFinished=1 WHERE
            $podcast=$podcastUri AND
            $isNew=0 AND $library=0 AND $starred=0 AND $playbackFinished=0 AND
            $published<=$newestNotNewPublishedTimestamp""")
  }

  def markUnknownEpisodesNew(podcastUri: URI): Unit =
    execSql(sql"UPDATE $table SET $isNew=1 WHERE $podcast=$podcastUri AND $isNew=0 AND $library=0 AND $starred=0 AND $playbackFinished=0")

  def markLatestUnknownEpisodeNew(podcastUri: URI): Unit =
    execSql(sql"UPDATE $table SET $isNew=1 WHERE $id IN (SELECT $id FROM $table WHERE $podcast=$podcastUri ORDER BY $published DESC LIMIT 1) AND $isNew=0 AND $library=0 AND $starred=0 AND $playbackFinished=0")

  def addUnknownEpisodesToLibrary(podcastUri: URI): Unit =
    execSql(sql"UPDATE $table SET $library=1 WHERE $podcast=$podcastUri AND $isNew=0 AND $library=0 AND $starred=0 AND $playbackFinished=0")

  def addUnknownEpisodesToPlaylist(podcastUri: URI): Unit = {
    val unknownEpisodeIds =
      findMultiple(
        sql"SELECT $id FROM $table WHERE $podcast=$podcastUri AND $isNew=0 AND $library=0 AND $starred=0 AND $playbackFinished=0 ORDER BY $published ASC",
        Mapping.simpleMapping("id" -> Mapping.long)
      ).toSeqAndClose()
    addToPlaylist(unknownEpisodeIds)
  }

  def limitPodcastLibraryEpisodeCount(podcastUri: URI, maxCount: Int): Unit = {
    execSql(
      sql"""
            UPDATE $table
            SET
              $playbackFinished=1, $library=0, $isNew=0, $playlistPosition=NULL
            WHERE
              $podcast=$podcastUri AND
              $starred=0 AND
              ($library=1 OR $isNew=1) AND
              $id NOT IN (SELECT $id FROM $table WHERE $podcast=$podcastUri AND ($library=1 OR $isNew=1) AND $starred=0 ORDER BY $published DESC LIMIT $maxCount)
         """)
  }

  def markAllUnlisted(pcastUri: URI): Unit =
    execSql(sql"UPDATE $table SET $listed=0 WHERE $podcast=$pcastUri")

  def deleteUnlistedUnreferenced(podcastUri: URI): Unit =
    execSql(sql"DELETE FROM $table WHERE $podcast=$podcastUri AND $listed=0 AND $starred=0 AND $downloadedBytes=0")

  //
  // starring
  //

  def starEpisodes(ids: Traversable[Long], star: Boolean): Unit =
    execSql(sql"UPDATE $episode SET $isNew=0, $library=1, $starred=$star WHERE $id IN ($ids)")

  def starEpisodesByIds(ids: Traversable[EpisodeId]): Unit = {
    execSql(sql"UPDATE $episode SET $isNew=0, $library=1, $starred=1 WHERE $eid IN ($ids)")
  }

  //
  // download stuff
  //

  def hasDownloadsInQueue: Boolean = findFirstDownloadEpisode().isDefined

  def findFirstDownloadEpisode(ignoredEpisodeIds: Traversable[Long] = Traversable()): Option[EpisodeListItem] =
    findOne(sql"""
      SELECT $listItemColumns
      FROM $episode
      WHERE $downloadListPosition IS NOT NULL AND $id NOT IN ($ignoredEpisodeIds)
      ORDER BY $downloadListPosition
      LIMIT 1""", EpisodeListItem)

  private def findDownloadList[A](selection: ColumnList, mapping: Mapping[A]): Cursor[A] =
    findMultiple(sql"""
      SELECT $selection
      FROM $episode
      WHERE $downloadListPosition IS NOT NULL
      ORDER BY $downloadListPosition""",
      mapping)

  def findDownloadListEpisodeIds: Cursor[EpisodeId] =
    findDownloadList(ColumnList(podcast, uri), EpisodeId.mapping)

  def findDownloadListItems: Cursor[EpisodeListItem] =
    findDownloadList(listItemColumns, EpisodeListItem)

  def updateDownloadList(episodes: Seq[EpisodeId]) {
    execSql(sql"UPDATE $episode SET $downloadListPosition=NULL")
    val statement = db.compileStatement(sql"UPDATE $episode SET $isNew=0, $library=1, $cached=0, $downloadListPosition=? WHERE $eid=?")
    for ((id, index) <- episodes.zipWithIndex) {
      statement.bindLong(1, index)
      statement.bindString(2, id.toString)
      statement.execute()
    }
  }

  def findNewAutoDownloadEpisodeIds: Cursor[Long] = {
    findMultiple(sql"""
      SELECT $id
      FROM $episode
      WHERE
        $cached=0 AND
        $downloadListPosition IS NULL AND
        $downloadComplete=0 AND
        $playbackFinished=0 AND
        $podcast IN (SELECT uri FROM podcast WHERE settings_autoDownload<>0)""", idMapping)
  }

  def addAutoDownloadPodcastEpisodesToDownloadListIfNotAlready(): Unit = {
    execSql(sql"""
      UPDATE $episode
      SET $isNew=0, $library=1, $cached=0, $downloadListPosition=CASE
        WHEN $downloadListPosition IS NOT NULL THEN $downloadListPosition
        WHEN $downloadComplete<>0 THEN NULL
        ELSE (SELECT IFNULL(MAX($downloadListPosition) + 1, 0) FROM $episode WHERE $downloadListPosition IS NOT NULL)
      END
      WHERE
    """)
  }

  def addToDownloadListIfNotAlready(ids: Traversable[Long], addToLibrary: Boolean = true) {
    val setLibrary = if (addToLibrary) sql"$library=$addToLibrary, " else sql""
    ids.foreach { episodeId =>
      execSql(sql"""
      UPDATE $episode
      SET $setLibrary $cached=0, $downloadListPosition=CASE
        WHEN $downloadListPosition IS NOT NULL THEN $downloadListPosition
        WHEN $downloadComplete<>0 THEN NULL
        ELSE (SELECT IFNULL(MAX($downloadListPosition) + 1, 0) FROM $episode WHERE $downloadListPosition IS NOT NULL)
      END
      WHERE $id=$episodeId""")
    }
  }

  def addToDownloadListEnd(ids: Traversable[Long]) {
    ids.foreach { episodeId =>
      execSql(sql"""
        UPDATE $episode
        SET $isNew=0, $library=1, $cached=0, $downloadListPosition=(SELECT IFNULL(MAX($downloadListPosition) + 1, 0) FROM $episode WHERE $downloadListPosition IS NOT NULL)
        WHERE $id=$episodeId""")
    }
  }

  def insertAtStartOfDownloadList(episodeId: EpisodeId) {
    val list = findDownloadListEpisodeIds.toListAndClose().filter(_ != episodeId) match {
      case Nil => episodeId :: Nil
      case tail => episodeId :: tail
    }
    updateDownloadList(list)
  }

  def insertIntoDownloadList(index: Int, episodeId: EpisodeId): Unit = {
    val list = findDownloadListEpisodeIds.toSeqAndClose().toIndexedSeq.filter(_ != episodeId)
    val updatedList = (list.take(index) :+ episodeId) ++ list.drop(index)
    updateDownloadList(updatedList)
  }

  def removeFromDownloadList(ids: Traversable[Long]) {
    execSql(sql"UPDATE $episode SET $downloadListPosition=NULL WHERE $id IN ($ids)")
  }

  def removeNonLibraryEpisodesFromDownloadList(): Unit =
    execSql(sql"UPDATE $episode SET $downloadListPosition=NULL WHERE $library=0")

  def updateDownloadFile(id: Long, file: String): Unit =
    execSql(sql"UPDATE $episode SET $downloadFile=$file WHERE ${EpisodeDao.id}=$id")

  def updateDownloadInfo(id: Long, fetchedBytes: Long, length: Long, duration: Long, complete: Boolean, attempts: Int, downloadErrorText: Option[String]) {
    execSql(sql"""
      UPDATE $episode
      SET
        $downloadedBytes=$fetchedBytes,
        $mediaLength=$length,
        $mediaDuration=$duration,
        $downloadComplete=$complete,
        $downloadAttempts=$attempts,
        $lastDownloadErrorText=$downloadErrorText
      WHERE ${EpisodeDao.id}=$id""")
    if (complete) {
      removeFromDownloadList(Traversable(id))
    }
  }

  def resetDownloadInfo(id: Long) {
    execSql(sql"""
      UPDATE $episode
      SET
        $downloadedBytes=0,
        $downloadComplete=0,
        $downloadAttempts=0,
        $lastDownloadErrorText=null,
        $downloadFile=null
      WHERE ${EpisodeDao.id}=$id""")
  }

  def resetDownloadInfoForOldFinishedEpisodes(): Unit = {
    execUpdateOrDelete(sql"""
      UPDATE $episode
      SET
        $downloadedBytes=0,
        $downloadComplete=0,
        $downloadAttempts=0,
        $lastDownloadErrorText=NULL,
        $downloadFile=NULL
      WHERE
        $downloadedBytes>0 AND
        $starred=0 AND
        $playbackFinished=1 AND
        ($playbackPositionTimestamp IS NULL OR ($playbackPositionTimestamp IS NOT NULL AND $NOW-$playbackPositionTimestamp>=24*60*60*1000))"""
    )
  }

  def resetDownloadInfoForRecentlyFinishedEpisodes(): Unit = {
    execSql(sql"""
      UPDATE $episode
      SET
        $downloadedBytes=0,
        $downloadComplete=0,
        $downloadAttempts=0,
        $lastDownloadErrorText=null,
        $downloadFile=null
      WHERE $recentlyFinished""")
  }

  def resetDownloadInfoForRecentlyFinishedEpisodes(podcast: Long): Unit = {
    execSql(sql"""
      UPDATE $episode
      SET
        $downloadedBytes=0,
        $downloadComplete=0,
        $downloadAttempts=0,
        $lastDownloadErrorText=null,
        $downloadFile=null
      WHERE
        $podcastId=$podcast AND
        $recentlyFinished""")
  }

  def findDownloadedIds: Cursor[Long] =
    findMultiple(sql"SELECT $id FROM $episode WHERE $downloadedBytes>0", idMapping)

  def resetDownloads(): Unit =
    execSql(sql"UPDATE $episode SET $downloadedBytes=0, $downloadComplete=0")

  //
  // playlist stuff
  //

  def findFirstPlaylistEpisodeId: Option[Long] =
    findOne(
      sql"SELECT $id FROM $episode WHERE $playlistPosition IS NOT NULL ORDER BY $playlistPosition LIMIT 1",
      idMapping)

  def findFirstFullyDownloadedPlaylistEpisodeId: Option[Long] =
    findOne(
      sql"SELECT $id FROM $episode WHERE $playlistPosition IS NOT NULL AND $downloadComplete<>0 ORDER BY $playlistPosition LIMIT 1",
      idMapping)

  def findSecondPlaylistEpisode: Option[EpisodeListItem] =
    findOne(
      sql"SELECT $listItemColumns FROM $episode WHERE $playlistPosition IS NOT NULL ORDER BY $playlistPosition LIMIT 1 OFFSET 1",
      EpisodeListItem)

  private def findPlaylist[A](selection: ColumnList, mapping: Mapping[A]): Cursor[A] =
    findMultiple(sql"""
      SELECT $selection
      FROM $episode
      WHERE $playlistPosition IS NOT NULL
      ORDER BY $playlistPosition""",
      mapping)

  def findPlaylistItems: Cursor[EpisodeListItem] =
    findPlaylist(listItemColumns, EpisodeListItem)

  def findPlaylistEpisodeIds: Cursor[EpisodeId] =
    findPlaylist(ColumnList(podcast, uri), EpisodeId.mapping)

  def findPlaylistEpisodeReferences: Cursor[EpisodeReference] =
    findPlaylist(ColumnList(podcastUrl, uri), EpisodeReference.mapping)

  def updatePlaylist(episodes: Seq[EpisodeId]) {
    execSql(sql"UPDATE $episode SET $playlistPosition=NULL WHERE $playlistPosition IS NOT NULL")
    val statementString = sql"UPDATE $episode SET $playlistPosition=? WHERE $eid=? AND $playbackFinished=0"
    logSql("query", s"for ${episodes.size} episodes: $statementString")
    val statement = db.compileStatement(statementString)
    for ((id, index) <- episodes.zipWithIndex) {
      statement.bindLong(1, index)
      statement.bindString(2, id.toString)
      statement.execute()
    }
  }

  def updatePlaylistByReferences(episodes: Seq[EpisodeReference]) {
    execSql(sql"UPDATE $episode SET $playlistPosition=NULL WHERE $playlistPosition IS NOT NULL")
    val statementString = sql"UPDATE $episode SET $playlistPosition=? WHERE $podcastUrl=? AND $uri=? AND $playbackFinished=0"
    logSql("query", s"for ${episodes.size} episodes: $statementString")
    val statement = db.compileStatement(statementString)
    for ((ref, index) <- episodes.zipWithIndex) {
      statement.bindLong(1, index)
      statement.bindString(2, ref.podcast.toString)
      statement.bindString(3, ref.uri.toString)
      statement.execute()
    }
  }

  def addToPlaylist(ids: Traversable[Long]) {
    ids.foreach { episodeId =>
      execSql(sql"""
        UPDATE $episode
        SET $playlistPosition=(SELECT IFNULL(MAX($playlistPosition) + 1, 0) FROM $episode WHERE $playlistPosition IS NOT NULL)
        WHERE $id=$episodeId""")
    }
  }

  def insertAtStartOfPlaylist(episodes: Seq[EpisodeId]): Unit = {
    val eidSet = episodes.toSet
    val list = findPlaylistEpisodeIds.toListAndClose().filter(!eidSet.contains(_)) match {
      case Nil => episodes
      case tail => episodes ++ tail
    }
    updatePlaylist(list)
  }

  def insertAtStartOfPlaylist(episodeId: EpisodeId): Unit =
    insertAtStartOfPlaylist(Seq(episodeId))

  def playNext(episode: EpisodeId, firstPositionLocked: Boolean) {

    case class EpisodeIdWithPlaybackPosition(podcast: URI, uri: URI, playbackPosition: Long) {
      val id = EpisodeId(podcast, uri)
    }

    val mapping = Mapping.map(
      "podcast" -> Mapping.uri,
      "uri" -> Mapping.uri,
      "playbackInfo_playbackPosition" -> Mapping.long
    )(EpisodeIdWithPlaybackPosition.apply)(EpisodeIdWithPlaybackPosition.unapply)

    def findPlaylistEpisodeIdsWithPlaybackPosition =
      findPlaylist(ColumnList(podcast, uri, playbackPosition), mapping)

    val playlist = findPlaylistEpisodeIdsWithPlaybackPosition.toListAndClose().filter(_.id != episode) match {
      case Nil => episode :: Nil
      case head :: Nil if firstPositionLocked || head.playbackPosition > 0 => head.id :: episode :: Nil
      case head :: Nil => episode :: head.id :: Nil
      case head :: tail if firstPositionLocked || head.playbackPosition > 0 => head.id :: episode :: tail.map(_.id)
      case head :: tail => episode :: head.id :: tail.map(_.id)
    }
    updatePlaylist(playlist)
  }

  def removeFromPlaylist(ids: Traversable[Long]) {
    execSql(sql"UPDATE $episode SET $playlistPosition=NULL WHERE $id IN ($ids)")
  }

  def updatePlaybackPosition(id: Long, position: Long, duration: Long, finished: Boolean) {
    execSql(sql"""
      UPDATE $episode
      SET $isNew=0, $library=1, $mediaDuration=$duration, $playbackPosition=$position, $playbackFinished=$finished, $playbackPositionTimestamp=$NOW
      WHERE ${EpisodeDao.id}=$id""")
  }

  def updatePlaybackSpeed(id: Long, speed: Float): Unit =
    execSql(sql"UPDATE $episode SET $playbackSpeed=$speed WHERE ${EpisodeDao.id}=$id")

  def resetPlaybackSpeed(podcastId: Long): Unit =
    execSql(sql"UPDATE $episode SET $playbackSpeed=NULL WHERE ${EpisodeDao.podcastId}=$podcastId")

  def updateVolumeGain(id: Long, gain: Float): Unit =
    execSql(sql"UPDATE $episode SET $volumeGain=$gain WHERE ${EpisodeDao.id}=$id")

  def resetVolumeGain(podcastId: Long): Unit =
    execSql(sql"UPDATE $episode SET $volumeGain=NULL WHERE ${EpisodeDao.podcastId}=$podcastId")

  def updatePlaybackFinished(ids: Traversable[Long]): Unit = execSql(sql"""
    UPDATE $episode
      SET $isNew=0, $library=0, $playlistPosition=NULL, $playbackFinished=1, $playbackPositionTimestamp=$NOW, $downloadListPosition=NULL
      WHERE $id IN ($ids)"""
  )

  def updatePlaybackFinished(id: Long): Unit =
    updatePlaybackFinished(Traversable(id))

  def updatePlaybackFinishedThisAndOlder(podcastUri: URI, episodeId: Long): Unit = execSql(sql"""
    UPDATE $episode
      SET $isNew=0, $library=0, $playlistPosition=NULL, $playbackFinished=1, $playbackPositionTimestamp=$NOW, $downloadListPosition=NULL
      WHERE $podcast=$podcastUri AND $published<=(SELECT $published FROM $episode WHERE $id=$episodeId)"""
  )

  def updatePlaybackUnfinished(ids: Traversable[Long]): Unit = execSql(sql"""
    UPDATE $episode
      SET $isNew=0, $library=1, $playbackFinished=0, $playbackPosition=0, $playbackPositionTimestamp=$NOW
      WHERE $id IN ($ids)"""
  )

  def updatePlaybackUnfinished(id: Long): Unit =
    updatePlaybackUnfinished(Traversable(id))

  def findChangedPlaybackPositionsCount(since: DateTime): Int = findOne(
    sql"SELECT COUNT($id) num FROM $episode WHERE $playbackPositionTimestamp>=$since",
    Mapping.simpleMapping("num" -> Mapping.int)
  ).getOrElse(0)

  def findChangedPlaybackPositions(since: DateTime, limit: Int, offset: Int): Cursor[EpisodePlaybackInfo] = {
    findMultiple(sql"""
      SELECT
        $podcastUrl podcast,
        $uri,
        $playbackPosition playbackInfo_position,
        $mediaDuration playbackInfo_duration,
        $playbackFinished playbackInfo_finished,
        $playbackSpeed playbackInfo_speed,
        $volumeGain playbackInfo_volumeGain,
        $playbackPositionTimestamp playbackInfo_modified
      FROM $episode
      WHERE $playbackPositionTimestamp>=$since
      LIMIT $limit OFFSET $offset""", EpisodePlaybackInfo)
  }

  //
  // download info backup
  //

  private def dropEpisodeDownloadInfoBackup(): Unit = {
    execSql(sql"DROP TABLE IF EXISTS episodeDownloadInfoBackup")
  }

  def backupDownloadInfo(): Unit = {
    dropEpisodeDownloadInfoBackup()
    execSql(sql"""
      CREATE TABLE episodeDownloadInfoBackup(
       $podcast TEXT,
       $podcastTitle TEXT,
       $mediaUrl TEXT,
       $downloadListPosition INTEGER,
       $downloadFile TEXT,
       $downloadComplete INTEGER,
       $downloadedBytes INTEGER,
       $downloadAttempts INTEGER,
       $lastDownloadErrorText TEXT)""")
    execSql(sql"""
      INSERT INTO episodeDownloadInfoBackup
        ($podcast, $podcastTitle, $mediaUrl, $downloadListPosition, $downloadFile, $downloadComplete, $downloadedBytes, $downloadAttempts, $lastDownloadErrorText)
        SELECT
          $podcast, $podcastTitle, $mediaUrl, $downloadListPosition, $downloadFile, $downloadComplete, $downloadedBytes, $downloadAttempts, $lastDownloadErrorText
        FROM $episode""")
  }

  def restoreDownloadInfo(): Unit = {

    def selectValueFor(column: Symbol, default: Any) = sql"""coalesce(
      (SELECT b.$column FROM episodeDownloadInfoBackup b WHERE b.$mediaUrl=$episode.$mediaUrl AND b.$podcast=$episode.$podcast),
      (SELECT b.$column FROM episodeDownloadInfoBackup b WHERE b.$mediaUrl=$episode.$mediaUrl AND b.$podcastTitle=$episode.$podcastTitle),
      (SELECT b.$column FROM episodeDownloadInfoBackup b WHERE b.$mediaUrl=$episode.$mediaUrl),
      $default)"""

    def selectValueForPodcast(column: Symbol, default: Any) = sql"""coalesce(
      (SELECT b.$column FROM episodeDownloadInfoBackup b WHERE b.$mediaUrl=$episode.$mediaUrl AND b.$podcast=$episode.$podcast),
      $default)"""

    execSql(sql"""UPDATE $episode SET
      $downloadListPosition=${selectValueForPodcast(downloadListPosition, 'Null)},
      $downloadFile=${selectValueFor(downloadFile, 'Null)},
      $downloadComplete=${selectValueFor(downloadComplete, 0)},
      $downloadedBytes=${selectValueFor(downloadedBytes, 0)},
      $downloadAttempts=${selectValueFor(downloadAttempts, 0)},
      $lastDownloadErrorText=${selectValueFor(lastDownloadErrorText, 'Null)}""")

    dropEpisodeDownloadInfoBackup()
  }

  //
  // chapter stuff
  //

  def resetChapters(episodeId: Long): Unit =
    execSql(sql"UPDATE $episode SET ${EpisodeDao.chapters}=NULL WHERE $id=$episodeId")

  def updateChapters(episodeId: Long, chapters: Seq[MediaChapter]): Unit = {
    val json = JsonWriter(Mapping.seq(MediaChapterMapping)).writeString(chapters)
    val statement = db.compileStatement(sql"UPDATE $episode SET ${EpisodeDao.chapters}=? WHERE $id=?")
    statement.bindString(1, json)
    statement.bindLong(2, episodeId)
    statement.execute()
  }

  def findChapters(episodeId: Long): Option[Seq[MediaChapter]] = {
    findOne(
      sql"SELECT $chapters FROM $episode WHERE $id=$episodeId",
      Mapping.simpleMapping(chapters.name, Mapping.optional(Mapping.seq(MediaChapterMapping)))
    ).flatten
  }

  //
  // upod 4 upgrade helper
  //

  def rememberUnfinished(): Unit = {
    execSql(db, sql"CREATE TABLE IF NOT EXISTS knownEpisodes (episodeRef TEXT)")
    execUpdateOrDelete(sql"INSERT INTO knownEpisodes (episodeRef) SELECT $podcastUrl || '::' || $mediaUrl episodeRef FROM $episode")
  }

  def markNotRemberedUnfinished(): Unit = {
    execUpdateOrDelete(sql"""
      UPDATE $episode
      SET $isNew=0, $library=0, $playlistPosition=NULL, $playbackFinished=1, $playbackPositionTimestamp=$NOW, $downloadListPosition=NULL
      WHERE $podcastUrl || '::' || $mediaUrl NOT IN (SELECT episodeRef FROM knownEpisodes)""")
    execSql(db, sql"DROP TABLE knownEpisodes")
  }
}

object EpisodeDao extends DaoObject {
  val table = Table('episode)
  val episode = table

  val podcast = Column('podcast)
  val id = Column('id)
  val eid = Column('eid)
  val uri = Column('uri)
  val published = Column('published)
  val title = Column('title)
  val subTitle = Column('subTitle)
  val link = Column('link)
  val author = Column('author)
  val keywords = Column('keywords)
  val description = Column('description)
  val mediaUrl = Column('media_url)
  val mediaType = Column('media_mimeType_mediaType)
  val mediaSubType = Column('media_mimeType_subType)
  val mediaLength = Column('media_length)
  val mediaDuration = Column('media_duration)
  val flattrLink = Column('flattrLink)
  val podcastId = Column('podcastInfo_id)
  val podcastUrl = Column('podcastInfo_url)
  val podcastTitle = Column('podcastInfo_title)
  val podcastImageUrl = Column('podcastInfo_imageUrl)
  val podcastBackgroundColor = Column('podcastInfo_colors_background)
  val podcastKeyColor = Column('podcastInfo_colors_key)
  val isNew = Column('new)
  val library = Column('library)
  val starred = Column('starred)
  val cached = Column('cached)
  val downloadListPosition = Column('downloadInfo_listPosition)
  val downloadFile = Column('downloadInfo_file)
  val downloadComplete = Column('downloadInfo_complete)
  val downloadedBytes = Column('downloadInfo_fetchedBytes)
  val downloadAttempts = Column('downloadInfo_attempts)
  val lastDownloadErrorText = Column('downloadInfo_lastError_text)
  val playlistPosition = Column('playbackInfo_listPosition)
  val playbackFinished = Column('playbackInfo_finished)
  val playbackPosition = Column('playbackInfo_playbackPosition)
  val playbackSpeed = Column('playbackInfo_playbackSpeed)
  val volumeGain = Column('playbackInfo_volumeGain)
  val playbackPositionTimestamp = Column('playbackInfo_playbackPositionTimestamp)
  val listed = Column('listed)
  val chapters = Column('chapters)
}