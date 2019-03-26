package mobi.upod.android.media

import android.app.PendingIntent
import android.content.{Context, Intent, IntentFilter}
import android.graphics.Bitmap
import android.media.MediaMetadataRetriever._
import android.media.RemoteControlClient._
import android.media.session.MediaSession
import android.media.{AudioManager, RemoteControlClient}
import mobi.upod.android.media.MediaSessionCompat.Token
import mobi.upod.android.media.PlaybackState._
import mobi.upod.android.util.ApiLevel
import mobi.upod.app
import mobi.upod.app.AppInjection
import mobi.upod.app.services.playback.{MediaButtonProcessor, MediaControlEventReceiver}

trait MediaSessionCompat {

  def token: Option[Token]

  def register(): Unit

  def unregister(): Unit

  def release(): Unit

  def setCallback(callback: Option[MediaSessionCompat.Callback]): Unit

  /** Set the track meta data for the current media session.
    *
    * @param artist the artist
    * @param title the track title
    * @param duration the track duration in milli seconds
    * @param coverart the tack's coverart, which will be freed (recycled) by the media session when no longer needed
    */
  def setMetaData(artist: String, title: String, duration: Long, coverart: Bitmap): Unit

  def setPlaybackState(state: PlaybackState, canPlay: Boolean, canPause: Boolean, canSeek: Boolean, canStop: Boolean)
}

object MediaSessionCompat {

  def apply(context: Context, sessionActivity: PendingIntent, mediaButtonIntent: PendingIntent): MediaSessionCompat = {
    if (ApiLevel >= ApiLevel.Oreo)
      new MediaSessionOreo(context, sessionActivity, mediaButtonIntent)
    else if (ApiLevel >= ApiLevel.Lollipop)
      new MediaSessionLollipop(context, sessionActivity, mediaButtonIntent)
    else if (ApiLevel >= ApiLevel.JellyBeanM2)
      new MediaSessionJellyBeanM2(context, mediaButtonIntent)
    else
      new MediaSessionIceCreamSandwich(context, mediaButtonIntent)
  }

  //
  // Token
  //

  /** Downwards compatible representation of a [[android.media.session.MediaSession.Token]].
    *
    * @param token the underlying token of type [[android.media.session.MediaSession.Token]].
    */
  type Token = android.support.v4.media.session.MediaSessionCompat.Token

  object Token {
    def apply(token: AnyRef): Token =
      android.support.v4.media.session.MediaSessionCompat.Token.fromToken(token)
  }

  //
  // Callback
  //

  trait Callback {

    def onPause(): Unit

    def onPlay(): Unit

    def onStop(): Unit

    def onFastForward(): Unit

    def onRewind(): Unit

    def onSeekTo(pos: Long): Unit

    def onSkip(): Unit
  }

  //
  // Oreo
  //

  private class MediaSessionOreo(context: Context, sessionActivity: PendingIntent, mediaButtonIntent: PendingIntent)
    extends MediaSessionLollipop(context, sessionActivity, mediaButtonIntent) {
    @volatile private var isEventReceiverRegistered = false
    private val mediaControlEventReceiver = new MediaControlEventReceiver

    override def register(): Unit = {
      super.register()
      context.registerReceiver(mediaControlEventReceiver, initIntentFilter)
      isEventReceiverRegistered = true
    }

    override def release(): Unit = {
      super.release()
      if (isEventReceiverRegistered) {
        isEventReceiverRegistered = false
        context.unregisterReceiver(mediaControlEventReceiver)
      }
    }

    private def initIntentFilter = {
      val intentFilter = new IntentFilter()
      intentFilter.addAction(AudioManager.ACTION_AUDIO_BECOMING_NOISY)
      intentFilter.addAction(Intent.ACTION_MEDIA_BUTTON)
      intentFilter
    }
  }

  //
  // Lollipop
  //

  private class MediaSessionLollipop(context: Context, sessionActivity: PendingIntent, mediaButtonIntent: PendingIntent)
    extends MediaSessionCompat
    with app.AppInjection{

    private val session = new MediaSession(context, "upod")
    private var callback: Option[Callback] = None

    init()

    private def init(): Unit = {
      session.setMediaButtonReceiver(mediaButtonIntent)
      session.setFlags(MediaSession.FLAG_HANDLES_MEDIA_BUTTONS | MediaSession.FLAG_HANDLES_TRANSPORT_CONTROLS)
      session.setSessionActivity(sessionActivity)
    }

    override def token: Option[Token] =
      Option(session.getSessionToken).map(Token.apply)

    override def register(): Unit = {
      session.setCallback(CallbackWrapper)
      session.setActive(true)
    }

    override def unregister(): Unit = {
      session.setCallback(null)
      session.setActive(false)
    }

    override def release(): Unit =
      session.release()

    override def setCallback(callback: Option[Callback]): Unit =
      this.callback = callback

    override def setMetaData(artist: String, title: String, duration: Long, coverart: Bitmap): Unit = {
      import android.media.MediaMetadata._

      val metadata = new Builder().
        putText(METADATA_KEY_ARTIST, artist).
        putText(METADATA_KEY_ALBUM_ARTIST, artist).
        putText(METADATA_KEY_TITLE, title).
        putLong(METADATA_KEY_DURATION, duration).
        putBitmap(METADATA_KEY_ART, coverart).
        build
      session.setMetadata(metadata)
      coverart.recycle()
    }

    override def setPlaybackState(state: PlaybackState, canPlay: Boolean, canPause: Boolean, canSeek: Boolean, canStop: Boolean): Unit = {
      import android.media.session.PlaybackState._
      import mobi.upod.android.media.PlaybackState._

      def flagsIf(condition: Boolean, flags: Long): Long =
        if (condition) flags else 0

      val builder = new Builder
      state match {
        case Connecting =>
          builder.setState(STATE_CONNECTING, 0, 0)
        case Buffering =>
          builder.setState(STATE_BUFFERING, 0, 0)
        case Playing(pos, speed) =>
          builder.setState(STATE_PLAYING, pos, speed)
        case Paused(pos) =>
          builder.setState(STATE_PAUSED, pos, 0)
        case Stopped =>
          builder.setState(STATE_STOPPED, 0, 0)
      }
      builder.setActions(
        flagsIf(canPlay, ACTION_PLAY) |
        flagsIf(canPause, ACTION_PAUSE) |
        flagsIf(canSeek, ACTION_REWIND | ACTION_FAST_FORWARD | ACTION_SEEK_TO | ACTION_SKIP_TO_NEXT) |
        flagsIf(canStop, ACTION_STOP)
      )
      session.setPlaybackState(builder.build)
    }

    //
    // media session callback
    //

    private object CallbackWrapper extends MediaSession.Callback with MediaButtonProcessor with AppInjection {
      override def onPlay(): Unit =
        callback.foreach(_.onPlay())

      override def onPause(): Unit =
        callback.foreach(_.onPause())

      override def onSkipToNext(): Unit =
        callback.foreach(_.onSkip())

      override def onFastForward(): Unit =
        callback.foreach(_.onFastForward())

      override def onRewind(): Unit =
        callback.foreach(_.onRewind())

      override def onStop(): Unit =
        callback.foreach(_.onStop())

      override def onSeekTo(pos: Long): Unit =
        callback.foreach(_.onSeekTo(pos))

      override def onMediaButtonEvent(mediaButtonIntent: Intent): Boolean =
        processMediaButtonEvent(mediaButtonIntent)
    }
  }

  //
  // ICS
  //

  private class MediaSessionIceCreamSandwich(context: Context, mediaButtonIntent: PendingIntent)
    extends MediaSessionCompat {

    private val audioManager = context.getSystemService(Context.AUDIO_SERVICE).asInstanceOf[AudioManager]
    protected val remoteControlClient = new RemoteControlClient(mediaButtonIntent)
    private var _callback: Option[Callback] = None
    protected def callback = _callback

    override def register(): Unit =
      audioManager.registerRemoteControlClient(remoteControlClient)

    override def unregister(): Unit =
      audioManager.unregisterRemoteControlClient(remoteControlClient)

    override def release(): Unit = {}

    override def token: Option[Token] = None

    override def setCallback(callback: Option[Callback]): Unit =
      _callback = callback

    override def setMetaData(artist: String, title: String, duration: Long, coverart: Bitmap): Unit = {
      val editor = remoteControlClient.editMetadata(true)
      editor.putString(METADATA_KEY_ARTIST, artist)
      editor.putString(METADATA_KEY_ALBUMARTIST, artist)
      editor.putString(METADATA_KEY_TITLE, title)
      editor.putLong(METADATA_KEY_DURATION, duration)
      editor.putBitmap(100, coverart)
      editor.apply()
    }

    final override def setPlaybackState(state: PlaybackState, canPlay: Boolean, canPause: Boolean, canSeek: Boolean, canStop: Boolean): Unit = {

      def flagsIf(condition: Boolean, flags: Int): Int =
        if (condition) flags else 0

      state match {
        case Connecting => onStateChanged(PLAYSTATE_BUFFERING)
        case Buffering => onStateChanged(PLAYSTATE_BUFFERING)
        case Playing(pos, speed) => onStateChanged(PLAYSTATE_PLAYING, pos, speed)
        case Paused(pos) => onStateChanged(PLAYSTATE_PAUSED, pos)
        case Stopped => onStateChanged(PLAYSTATE_STOPPED)
      }

      remoteControlClient.setTransportControlFlags(
        flagsIf(canPlay, FLAG_KEY_MEDIA_PLAY | FLAG_KEY_MEDIA_PLAY_PAUSE) |
        flagsIf(canPause, FLAG_KEY_MEDIA_PAUSE | FLAG_KEY_MEDIA_PLAY_PAUSE) |
        flagsIf(canSeek, FLAG_KEY_MEDIA_FAST_FORWARD | FLAG_KEY_MEDIA_REWIND | FLAG_KEY_MEDIA_PREVIOUS | FLAG_KEY_MEDIA_NEXT | FLAG_KEY_MEDIA_POSITION_UPDATE) |
        flagsIf(canStop, FLAG_KEY_MEDIA_STOP)
      )
    }

    protected def onStateChanged(state: Int, position: Long = 0, speed: Float = 0): Unit =
      remoteControlClient.setPlaybackState(state)
  }

  //
  // Jelly Bean M2
  //

  private class MediaSessionJellyBeanM2(context: Context, mediaButtonIntent: PendingIntent)
    extends MediaSessionIceCreamSandwich(context, mediaButtonIntent)
    with RemoteControlClient.OnGetPlaybackPositionListener
    with RemoteControlClient.OnPlaybackPositionUpdateListener {

    private var playbackPosition: Long = 0

    override def register(): Unit = {
      remoteControlClient.setOnGetPlaybackPositionListener(this)
      remoteControlClient.setPlaybackPositionUpdateListener(this)
      super.register()
    }

    override def unregister(): Unit = {
      super.unregister()
      remoteControlClient.setOnGetPlaybackPositionListener(null)
      remoteControlClient.setPlaybackPositionUpdateListener(null)
    }

    override protected def onStateChanged(state: Int, position: Long, speed: Float): Unit = {
      playbackPosition = position
      remoteControlClient.setPlaybackState(state, position, speed)
    }

    //
    // OnGetPlaybackPositionListener
    //

    override def onGetPlaybackPosition(): Long =
      playbackPosition

    //
    // OnPlaybackPositionUpdateListener
    //
    override def onPlaybackPositionUpdate(newPositionMs: Long): Unit =
      callback.foreach(_.onSeekTo(newPositionMs))
  }
}