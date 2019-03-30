package mobi.upod.app.gui

import android.app.Activity
import android.view.View
import com.escalatesoft.subcut.inject.Injectable
import com.github.amlcurran.showcaseview.targets.ViewTarget
import com.github.amlcurran.showcaseview.{OnShowcaseEventListener, ShowcaseView}
import mobi.upod.android.os.Runnable

import scala.util.Try

trait UsageTips extends Injectable {
  import mobi.upod.app.gui.UsageTips._

  private lazy val usageTipPreferences = inject[mobi.upod.app.App].getSharedPreferences("tips", 0)

  def usageTips: Seq[ShowcaseTip] = Seq()

  def getActivity: Activity

  def showUsageTips(): Unit =
    showNextTipIfAny()

  def showUsageTipsDelayed(): Unit =
    getActivity.getWindow.getDecorView.postDelayed(Runnable(showNextTipIfAny()), 500)

  private def shouldShow(tip: ShowcaseTip): Boolean =
    usageTipPreferences.getBoolean(tip.prefKey, true)

  private def setShown(tip: KeyedTip): Unit = {
    val editor = usageTipPreferences.edit()
    editor.putBoolean(tip.prefKey, false)
    editor.commit()
  }

  def disableAllTips(): Unit =
    UsageTipsKey.values().map(new KeyedTip(_)).foreach(setShown)

  private def showTip(tip: ShowcaseTip): Unit = if (!showingTip && tip.canShow) {
    val targetView = Try(tip.target).getOrElse(null)
    if (targetView != null && targetView.isShown) {
      showingTip = true
      new ShowcaseView.Builder(getActivity).
        setTarget(new ViewTarget(targetView)).
        setContentTitle(tip.titleId).
        setContentText(tip.detailsId).
        setButtonRight(tip.buttonRight).
        setShowcaseEventListener(new ShowcaseEventListener(tip)).
        build()
    }
  }

  private def onTipFinished(tip: ShowcaseTip): Unit = {
    setShown(tip)
    showingTip = false
    showNextTipIfAny()
  }

  private def showNextTipIfAny(): Unit =
    usageTips.find(shouldShow) foreach showTip

  private class ShowcaseEventListener(tip: ShowcaseTip) extends OnShowcaseEventListener {

    override def onShowcaseViewHide(showcaseView: ShowcaseView): Unit =
      onTipFinished(tip)

    override def onShowcaseViewShow(showcaseView: ShowcaseView): Unit = ()

    override def onShowcaseViewDidHide(showcaseView: ShowcaseView): Unit = ()
  }
}

object UsageTips {
  private var showingTip: Boolean = false

  class KeyedTip(private val key: UsageTipsKey) {
    private[UsageTips] val prefKey = s"show_${key.getKey}_tip"
  }

  class ShowcaseTip(key: UsageTipsKey, val titleId: Int, val detailsId: Int, getTarget: => View, val buttonRight: Boolean, getCanShow: => Boolean) extends KeyedTip(key) {
    def target: View = getTarget

    def canShow: Boolean = Try(getCanShow).getOrElse(false)
  }

  object ShowcaseTip {
    def apply(key: UsageTipsKey, titleId: Int, detailsId: Int, target: => View, buttonRight: Boolean = true, canShow: => Boolean = true): ShowcaseTip =
      new ShowcaseTip(key, titleId, detailsId, target, buttonRight, canShow)
  }
}