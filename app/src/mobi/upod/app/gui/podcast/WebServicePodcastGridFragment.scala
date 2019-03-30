package mobi.upod.app.gui.podcast

import android.app.SearchManager
import android.content.{Context, Loader}
import android.os.Bundle
import android.support.v4.view.MenuItemCompat
import android.support.v7.widget.SearchView
import android.view.{Menu, MenuInflater, View}
import android.widget.{AbsListView, AdapterView, GridView}
import mobi.upod.android.app.AppException
import mobi.upod.android.app.action.{Action, SimpleFragmentActions}
import mobi.upod.android.content.AsyncCursorLoader
import mobi.upod.android.logging.Logging
import mobi.upod.android.view.DisplayMetrics
import mobi.upod.android.widget.OnScrolledToBottomListener
import de.wcht.upod.R
import mobi.upod.app.data.PodcastListItem
import mobi.upod.app.gui.UsageTips.ShowcaseTip
import mobi.upod.app.gui.{MainNavigation, UsageTips, UsageTipsKey}
import mobi.upod.app.services.PodcastDirectoryWebService
import mobi.upod.app.storage.{ImageSize, PodcastDao}
import mobi.upod.util.Cursor

import scala.util.Try

sealed private[podcast] abstract class WebServicePodcastGridFragment(
    navId: Long,
    emptyTextId: Int)
  extends PodcastGridFragment(navId, emptyTextId)
  with SimpleFragmentActions
  with Logging {

  private val PageSize = 100

  private lazy val podcastDao = inject[PodcastDao]
  private lazy val webService = inject[PodcastDirectoryWebService]
  protected lazy val scrollListener = new OnScrolledToBottomListener(loadAdditionalItemsIfApplicable())

  override protected def optionsMenuResourceId: Int = R.menu.popular_podcasts_actions

  override protected def createActions: Map[Int, Action] = Map()

  private var resultPage = 0
  private var hasFurtherPages = true
  private var loading = true

  private var _reloadOnCreate = true
  override protected def reloadOnCreate: Boolean = _reloadOnCreate

  override def onActivityCreated(savedInstanceState: Bundle): Unit = {
    _reloadOnCreate = savedInstanceState == null
    super.onActivityCreated(savedInstanceState)

    if (savedInstanceState != null) {
      restoreSavedInstanceState(savedInstanceState)
    }

    getGridView.setOnScrollListener(scrollListener)
  }

  private def restoreSavedInstanceState(state: Bundle): Unit = {
    try {
      val podcasts = state.getSerializable("podcasts").asInstanceOf[Array[PodcastListItem]].toIndexedSeq
      resultPage = state.getInt("resultPage")
      hasFurtherPages = state.getBoolean("hasFurtherPages")
      setAdapter(podcasts)
      getGridView.onRestoreInstanceState(state.getParcelable("podcastGrid"))
    } catch {
      case ex: Throwable =>
        log.error("failed to restore saved instance state", ex)
        reload()
    }
  }

  override def onSaveInstanceState(outState: Bundle): Unit = {
    super.onSaveInstanceState(outState)

    if (adapter != null) {
      outState.putSerializable("podcasts", adapter.items.toArray)
      outState.putInt("resultPage", resultPage)
      outState.putBoolean("hasFurtherPages", hasFurtherPages)
      outState.putParcelable("podcastGrid", getGridView.onSaveInstanceState())
    }
  }


  //
  // menu
  //

  override def onCreateOptionsMenu(menu: Menu, inflater: MenuInflater): Unit = {

    def initSearchView(): Unit = {
      val searchManager = getActivity.getSystemService(Context.SEARCH_SERVICE).asInstanceOf[SearchManager]
      val searchView = MenuItemCompat.getActionView(menu.findItem(R.id.action_search)).asInstanceOf[SearchView]
      searchView.setSearchableInfo(searchManager.getSearchableInfo(getActivity.getComponentName))
    }

    super.onCreateOptionsMenu(menu, inflater)
    initSearchView()
  }

  //
  // grid
  //

  override protected def choiceMode = AbsListView.CHOICE_MODE_NONE

  override protected def gridDisplayOptionsChanged = false

  private lazy val isLargeScreen =
    getActivity.getResources.getBoolean(R.bool.largeScreen)

  override protected def initGridMetrics(gridView: GridView)(implicit displayMetrics: DisplayMetrics): Unit = {
    gridView.setColumnWidth(getActivity.getResources.getDimensionPixelSize(R.dimen.discover_item_width))
    gridView.setNumColumns(GridView.AUTO_FIT)

    val padding = getResources.getDimensionPixelSize(R.dimen.space_xsmall)
    gridView.setPadding(padding, padding, padding, padding)

    val spacing = getResources.getDimensionPixelSize(R.dimen.discover_grid_spacing)
    gridView.setHorizontalSpacing(spacing)
    gridView.setVerticalSpacing(spacing)
  }

  override protected val itemLayoutResource: Int =
    R.layout.podcast_grid_discover_item

  override protected def itemImageSize: ImageSize =
    if (isLargeScreen) ImageSize.hugeList else ImageSize.largeList

  // images always have to be loaded from the network when searching for podcasts
  override protected def slowLoadingImages: Boolean = true

  // images from the webservice are scaled and expected to be small enough to download even on metered connections
  override protected def smallImageExpected: Boolean = true

  override protected def isGridDisplay: Boolean = false

  //
  // loading
  //

  protected def loadPodcasts(webService: PodcastDirectoryWebService, resultPage: Int): Cursor[PodcastListItem]

  def onCreateLoader(id: Int, args: Bundle) =
    AsyncCursorLoader(getActivity, loadCursor)

  override def onLoadFinished(loader: Loader[IndexedSeq[PodcastListItem]], data: IndexedSeq[PodcastListItem]): Unit = {
    hasFurtherPages = data.size >= PageSize
    showBottomLoadIndicator(false)
    Option(getGridAdapter) match {
      case Some(adapter: PodcastListItemAdapter) if resultPage > 0 =>
        adapter.setItems(adapter.items ++ data)
      case _ =>
        super.onLoadFinished(loader, data)
    }
  }

  private def loadCursor: Cursor[PodcastListItem] = {
    loading = true
    try {
      val subscriptions = podcastDao.findSubscriptionUris.toSetAndClose()
      val cursor = loadPodcasts(webService, resultPage).zipWithIndex.map { case (p, index) =>
        p.copy(
          id = resultPage * PageSize + index + 1,
          subscribed = subscriptions.contains(p.uri)
        )
      }
      resetLastError()
      cursor
    } catch {
      case ex: AppException =>
        log.error("failed to load podcasts via web service", ex)
        setLastError(ex.errorTitle(getActivity))
        Cursor.empty
      case ex: Exception =>
        log.error("failed to load podcasts via web service", ex)
        val msg = Try(getString(R.string.connection_error)).getOrElse("connection error")
        setLastError(msg)
        Cursor.empty
    } finally {
      loading = false
    }
  }

  private def loadAdditionalItemsIfApplicable(): Unit = if (hasFurtherPages && !loading) {
    resultPage += 1
    log.info(s"loading result page $resultPage")
    showBottomLoadIndicator(true)
    getLoaderManager.restartLoader(loaderId, null, this)
  }


  override def reload(): Unit = {
    resultPage = 0
    super.reload()
  }

  override def onItemClick(parent: AdapterView[_], view: View, position: Int, id: Long): Unit =
    OpenPodcastUriActivity.start(getActivity, adapter.items(translatePosition(position)))

  override def onEpisodeCountChanged(): Unit = {
    // do not reload when episode count changed
  }
}

sealed private[podcast] abstract class PopularCategoryPodcastsGridFragment(category: Option[String]) 
  extends WebServicePodcastGridFragment(MainNavigation.findPodcasts, R.string.empty_popular)
  with UsageTips {

  override def usageTips: Seq[ShowcaseTip] = Seq(
    UsageTips.ShowcaseTip(UsageTipsKey.CATEGORY_SELECTION, R.string.tip_category_selection, R.string.tip_category_selection_details, supportActionBar.getCustomView, canShow = state.started),
    UsageTips.ShowcaseTip(UsageTipsKey.ADD_PODCAST, R.string.tip_add_podcast, R.string.tip_add_podcast_details, getActivity.findViewById(R.id.action_add_podcast), buttonRight = false, canShow = state.started)
  )

  override def onActivityCreated(savedInstanceState: Bundle): Unit = {
    super.onActivityCreated(savedInstanceState)
    AddPodcastFabHelper.add(this, Some(scrollListener))
  }

  override protected def loadPodcasts(webService: PodcastDirectoryWebService, resultPage: Int): Cursor[PodcastListItem] =
    webService.findPopularPodcasts(resultPage, category)
}

final class PopularPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(None)
final class PopularArtsPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("arts"))
final class PopularBusinessPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("business"))
final class PopularComedyPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("comedy"))
final class PopularEducationPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("education"))
final class PopularGamesAndHobbiesPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("games-n-hobbies"))
final class PopularGovernmentAndOrganizationsPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("government-n-organizations"))
final class PopularHealthPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("health"))
final class PopularKidsAndFamilyPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("kids-n-family"))
final class PopularMusicPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("music"))
final class PopularNewsAndPoliticsPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("news-n-politics"))
final class PopularReligionAndSpiritualityPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("religion-n-spirituality"))
final class PopularScienceAndMedicinePodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("science-n-medicine"))
final class PopularSocietyAndCulturePodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("society-n-culture"))
final class PopularSportsAndRecreationPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("sports-n-recreation"))
final class PopularTechnologyPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("technology"))
final class PopularTVAndFilmPodcastsGridFragment extends PopularCategoryPodcastsGridFragment(Some("tv-n-film"))

final class SearchPodcastsGridFragment extends WebServicePodcastGridFragment(MainNavigation.findPodcasts, R.string.empty_search) {

  override protected def optionsMenuResourceId: Int = R.menu.search_podcasts_actions

  private def searchQuery: String =
    Option(getActivity.getIntent.getStringExtra(SearchManager.QUERY)).getOrElse("")

  override protected def loadPodcasts(webService: PodcastDirectoryWebService, resultPage: Int): Cursor[PodcastListItem] =
    webService.findPodcasts(searchQuery, resultPage)

  override def onCreateOptionsMenu(menu: Menu, inflater: MenuInflater): Unit = {
    super.onCreateOptionsMenu(menu, inflater)

    val query = searchQuery
    val searchView = menu.findItem(R.id.action_search).getActionView.asInstanceOf[SearchView]
    searchView.setQuery(query, false)
    getActivity.setTitle(query)
  }
}
