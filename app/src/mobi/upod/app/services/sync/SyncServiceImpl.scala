package mobi.upod.app.services.sync

import java.io.IOException
import java.net.{SocketTimeoutException, URI}

import android.app.{IntentService, PendingIntent}
import android.content.{Context, Intent}
import android.support.v4.app.NotificationCompat
import com.github.nscala_time.time.Imports._
import com.google.gson.JsonIOException
import de.wcht.upod.R
import mobi.upod.android.app.{AppException, IntegratedNotificationManager, UpodNotificationChannels}
import mobi.upod.android.content.IntentHelpers._
import mobi.upod.android.logging.Logging
import mobi.upod.android.os.{BundleSerializableValue, PowerManager}
import mobi.upod.app._
import mobi.upod.app.gui.MainActivity
import mobi.upod.app.storage.{EpisodeUriState, InternalSyncPreferences, SyncPreferences}
import org.joda.time.DateTime

class SyncServiceImpl
  extends IntentService("SyncServiceImpl")
  with IntegratedNotificationManager
  with AppInjection
  with Logging {

  import mobi.upod.app.services.sync.SyncServiceImpl._

  private val syncService = inject[SyncService]
  private val syncPreferences = inject[SyncPreferences]
  private val internalSyncPreferences = inject[InternalSyncPreferences]
  private lazy val notificationBuilder = createNotificationBuilder

  private def createNotificationBuilder: NotificationCompat.Builder = {
    createNotificationOnDefaultChannel(this, UpodNotificationChannels.Sync).
      setOngoing(true).
      setSmallIcon(R.drawable.ic_stat_sync).
      setContentTitle(getString(R.string.sync_notification_title)).
      setProgress(0, 0, true).
      setContentIntent(PendingIntent.getActivity(inject[App], 0, new Intent(inject[App], classOf[MainActivity]), PendingIntent.FLAG_UPDATE_CURRENT)).
      asInstanceOf[NotificationCompat.Builder]
  }

  def onHandleIntent(intent: Intent): Unit = {
    HighFrequencyIntentFilter.ifAllowed(intent) {
      inject[PowerManager].partiallyWaked {
        inForeground(notificationBuilder.build, syncPreferences.showSyncNotification) {
          if (internalSyncPreferences.episodeUriState.get != EpisodeUriState.UpToDate) {
            doSync(FullSyncAction, "full with URI update", fullSync)
          } else intent.getAction match {
            case FullSyncAction =>
              doSync(FullSyncAction, "full", fullSync)
            case PodcastSyncAction =>
              intent.getExtra(PodcastToSync) foreach { podcast =>
                doSync(PodcastSyncAction, "podcast $podcast", podcastSync(_, podcast, _))
              }
            case unknownAction =>
              log.error(s"Unknown action $unknownAction")
          }
        }
      }
    } /* else */ {
      intent.getAction match {
        case FullSyncAction => scheduleNextAutomaticSync()
        case _ => // ignore
      }
    }
  }

  override def onDestroy(): Unit = {
    cancelNotification()
    log.info("destroying sync service")
    scheduleNextAutomaticSync()
    super.onDestroy()
  }

  private def doSync(action: String, syncType: String, sync: (Syncer, SyncProgressIndicator) => Unit) {

    def logGenericError(error: Option[Throwable]): Unit = {
      log.error(s"$syncType sync failed", error.orNull)
      error.foreach(log.crashLogSend)
      app.notifyError(
        error.getClass.getSimpleName,
        getString(R.string.sync_failed_title),
        getString(R.string.sync_failed_content, error.map(_.getLocalizedMessage).getOrElse("null")))
    }

    def processException(error: Throwable, rootCause: Option[Throwable] = None): Unit = error match {
      case ex: AppException =>
        log.error(s"$syncType sync failed due to app error", rootCause.getOrElse(error))
        app.notifyError(ex)
      case ex: SocketTimeoutException =>
        log.error(s"$syncType sync failed due to socket timeout", rootCause.getOrElse(error))
      case ex: IOException =>
        log.error(s"$syncType sync failed due to IO error", rootCause.getOrElse(error))
      case ex: JsonIOException =>
        processException(ex.getCause, rootCause.orElse(Some(ex)))
      case ex: Throwable =>
        logGenericError(rootCause.orElse(Some(ex)))
      case null =>
        logGenericError(rootCause)
    }

    log.info(s"starting $syncType sync")
    syncService.onSyncStarted(action)
    try {
      withSyncer(sync(_, ProgressIndicator))
    } catch {
      case ex: Throwable =>
        processException(ex)
    } finally {
      syncService.onSyncFinished()
      log.info(s"finished $syncType sync")
    }
  }

  private def fullSync(syncer: Syncer, progressIndicator: SyncProgressIndicator): Unit = {
    new PullSynchronizer(syncer).syncAllPodcasts(progressIndicator)
  }

  private def podcastSync(syncer: Syncer, podcast: URI, progressIndicator: SyncProgressIndicator): Unit = {
    new PullSynchronizer(syncer).syncPodcast(podcast, progressIndicator)
  }

  private def crossDeviceSync(syncer: Syncer, progressIndicator: SyncProgressIndicator): Unit = {
    new PullSynchronizer(syncer).syncAllPodcastsWithNewerServerStatus(progressIndicator)
  }

  private def scheduleNextAutomaticSync(): Unit =
    syncService.scheduleAutomaticSync(1.minute.millis)

  private def withSyncer(f: Syncer => Unit): Unit = {
    ProgressIndicator.updateProgress(getString(R.string.sync_notification_sync))
    f(null)
  }

  private object ProgressIndicator extends SyncProgressIndicator {
    override def updateProgress(progress: Int, max: Int, taskDescription: String): Unit = {
      log.crashLogInfo(s"$progress/$max - $taskDescription")
      super.updateProgress(progress, max, taskDescription)
      if (syncPreferences.showSyncNotification) {
        notificationBuilder.setProgress(max, progress, false).setContentText(taskDescription)
        updateNotification(notificationBuilder.build)
      }
    }
  }

  private case class LastIntent(action: String, finished: DateTime) {

    def allowNextIntent(intent: Intent): Boolean =
      (finished to DateTime.now).millis > HighFrequencyIntentFilter.MinIntentGap
  }

  private object HighFrequencyIntentFilter {
    val MinIntentGap = 2.seconds.millis
    private var lastIntent: Option[LastIntent] = None

    def remember(intent: Intent): Unit =
      lastIntent = Some(LastIntent(intent.getAction, DateTime.now))

    def allow(intent: Intent): Boolean =
      lastIntent.isEmpty || lastIntent.exists(_.allowNextIntent(intent))

    def ifAllowed(intent: Intent)(f: => Unit)(otherwise: => Unit): Unit = {
      if (allow(intent)) {
        f
      } else {
        log.info(s"filtered out '${intent.getAction}' request due to high frequency")
        otherwise
      }
      remember(intent)
    }
  }
}

object SyncServiceImpl {
  private[sync] val FullSyncAction = IntentAction("sync.full")
  private[sync] val PodcastSyncAction = IntentAction("sync.podcast")

  private object PodcastToSync extends BundleSerializableValue[URI](IntentExtraKey("podcastToSync"))

  private def syncIntent(context: Context, syncAction: String): Intent = {
    val intent = new Intent(syncAction, null, context, classOf[SyncServiceImpl])
    intent
  }

  private[sync] def fullSyncIntent(context: Context): Intent = {
    syncIntent(context, FullSyncAction)
  }

  private[sync] def syncPodcastIntent(context: Context, podcast: URI): Intent = {
    val intent = syncIntent(context, PodcastSyncAction)
    intent.putExtra(PodcastToSync, podcast)
    intent
  }

  def requestFullSync(context: Context): Unit =
    context.startService(fullSyncIntent(context))

  def requestPodcastSync(context: Context, podcast: URI): Unit =
    context.startService(syncPodcastIntent(context, podcast))
}
