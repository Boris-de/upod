package mobi.upod.app.gui.episode.online

import android.content.Loader
import android.view.View
import de.wcht.upod.R
import mobi.upod.android.app.action.{Action, SimpleFragmentActions}
import mobi.upod.android.logging.Logging
import mobi.upod.app.data.{EpisodeListItem, PodcastListItem}
import mobi.upod.app.gui.UsageTips.ShowcaseTip
import mobi.upod.app.gui.episode._
import mobi.upod.app.gui.episode.download.AddEpisodesToDownloadListAction
import mobi.upod.app.gui.episode.library.StarEpisodesAction
import mobi.upod.app.gui.episode.news.AddEpisodesToLibraryAction
import mobi.upod.app.gui.episode.playlist.AddEpisodesToPlaylistAction
import mobi.upod.app.gui.{MainNavigation, ReloadOnEpisodeListChangedFragment, UsageTips, UsageTipsKey}
import mobi.upod.app.services.OnlinePodcastService
import mobi.upod.app.storage.EpisodeDao
import mobi.upod.util.Cursor

final class OnlinePodcastEpisodeListFragment
  extends PodcastEpisodeListFragment
  with ReloadOnEpisodeListChangedFragment[IndexedSeq[EpisodeListItem]]
  with SimpleFragmentActions
  with UsageTips
  with Logging {

  private lazy val onlineService = inject[OnlinePodcastService]

  protected val optionsMenuResourceId = R.menu.online_podcast_episode_actions

  protected val emptyTextId: Int = R.string.no_items

  override protected def hasOptionsMenu: Boolean = enableActions

  protected val contextualMenuResourceId = R.menu.online_episode_contextual
  protected override val createActions: Map[Int, Action] = super.createActions ++ podcastActions ++ Map(
    R.id.action_add_to_library -> new AddEpisodesToLibraryAction(checkedEpisodes) with BulkEpisodeAdapterUpdate,
    R.id.action_add_download -> new AddEpisodesToDownloadListAction(checkedEpisodes) with BulkEpisodeAdapterUpdate,
    R.id.action_add_to_playlist -> new AddEpisodesToPlaylistAction(checkedEpisodes) with BulkEpisodeAdapterUpdate,
    R.id.action_star -> new StarEpisodesAction(checkedEpisodes, true) with BulkEpisodeAdapterUpdate,
    R.id.action_unstar -> new StarEpisodesAction(checkedEpisodes, false) with BulkEpisodeAdapterUpdate,
    R.id.action_mark_finished -> new MarkEpisodesFinishedAction(checkedEpisodes) with BulkEpisodeAdapterUpdate,
    R.id.action_mark_unfinished -> new MarkEpisodesUnfinishedAction(checkedEpisodes) with BulkEpisodeAdapterUpdate,
    R.id.action_save_unfinished_for_later -> new AddUnfinishedEpisodesToLibraryAction(podcast.id) with ActionReload
  )

  override def usageTips: Seq[ShowcaseTip] = super.usageTips ++ Seq(
    UsageTips.ShowcaseTip(UsageTipsKey.SUBSCRIBE_PODCAST, R.string.tip_subscribe_podcast, R.string.tip_subscribe_podcast_details, findSubscribeButton)
  )

  private def findSubscribeButton: View = {
    if (actions.get(R.id.action_podcast_subscribe).exists(_.isEnabled(getActivity)))
      childView(R.id.floatingActionButton)
    else
      null
  }

  protected def createAdapter(data: IndexedSeq[EpisodeListItem]) = {
    def config = ViewHolderConfiguration(false, true, false)
    new EpisodeListItemAdapter(data, new OnlineEpisodeListItemViewHolder(_, config))
  }

  override def onLoaderReset(loader: Loader[IndexedSeq[EpisodeListItem]]) {
    super.onLoaderReset(loader)
  }

  override protected def onAddHeaders(): Unit = {
    super.onAddHeaders()
    showUsageTipsDelayed()
  }

  protected def navigationItemId = MainNavigation.podcasts

  protected def loadEpisodes(dao: EpisodeDao, podcast: PodcastListItem, sortAscending: Boolean): Cursor[EpisodeListItem] =
    onlineService.getCachedOnlinePodcastEpisodes(podcast, sortAscending)
}
