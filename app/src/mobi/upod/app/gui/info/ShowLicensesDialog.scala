package mobi.upod.app.gui.info

import android.app.{Activity, Dialog, DialogFragment}
import android.os.Bundle
import android.support.v7.app.AlertDialog
import android.view.View
import android.webkit.WebView
import de.wcht.upod.R

class ShowLicensesDialog(val url: String) extends DialogFragment {

  override def onCreateDialog(savedInstanceState: Bundle): Dialog = {
    val context = getActivity

    val builder = new AlertDialog.Builder(context)
    builder.setView(createContentView)
    builder.setPositiveButton(R.string.close, null)
    builder.create
  }

  private def createContentView: View = {
    // GoogleApiAvailability.getInstance().getOpenSourceSoftwareLicenseInfo(getActivity) + licenses
    val webView = new WebView(getActivity)
    webView.loadUrl(url)
    webView
  }
}

object ShowLicensesDialog {
  private val tag = "LicensesDialog"

  def show(activity: Activity, url: String): Unit = {
    val fragment = new ShowLicensesDialog(url)
    fragment.show(activity.getFragmentManager, tag)
  }
}