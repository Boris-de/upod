package mobi.upod.app.storage

import java.io.File
import java.net.{URL, URLDecoder, URLEncoder}

import android.content.Context
import android.graphics.drawable.Drawable
import android.graphics.{Bitmap, BitmapFactory, Canvas}
import com.escalatesoft.subcut.inject.{BindingModule, Injectable}
import mobi.upod.android.logging.Logging
import mobi.upod.android.view.DisplayMetrics
import mobi.upod.io._

class CoverartProvider(context: Context)(implicit val bindingModule: BindingModule) extends Injectable with Logging {
  private lazy val storagePreferences = inject[StoragePreferences]

  private def storageProvider = storagePreferences.storageProvider

  private def coverartDir = storageProvider.coverartDirectory

  private def urlFromDirName(dirName: String) = new URL(URLDecoder.decode(dirName, CharsetName.utf8))

  private def dirNameFromUrl(url: URL) = URLEncoder.encode(url.toString, CharsetName.utf8)

  def storageAvailable = storageProvider.writable

  def availableImages: Set[URL] = Option(coverartDir).flatMap(dir => Option(dir.listFiles)) match {
    case Some(files) => files.map(dir => urlFromDirName(dir.getName)).toSet
    case None => Set()
  }

  private def getImageDir(url: URL): File = new File(coverartDir, dirNameFromUrl(url))

  def hasImage(url: URL): Boolean = getImageDir(url).exists()

  def deleteImage(url: URL) {
    getImageDir(url).deleteRecursive()
  }

  def prepareImage(url: URL) {
    getImageDir(url).mkdirs()
  }

  def getImageFile(url: URL, size: ImageSize): File = {
    val file = new File(getImageDir(url), size.fileName)
    if (file.length() > 0) {
      file
    } else {
      new File(getImageDir(url), ImageSize.full.fileName)
    }
  }

  def getExistingImageFile(url: URL, size: ImageSize): Option[File] = {
    if (storageProvider.readable) {
      getImageFile(url, size) match {
        case file if file.isFile => Some(file)
        case _ => None
      }
    } else {
      None
    }
  }

  def getImageBitmap(url: URL, size: ImageSize): Option[Bitmap] = {
    try {
      getExistingImageFile(url, size).flatMap(file => Option(BitmapFactory.decodeFile(file.getAbsolutePath)))
    } catch {
      case _: OutOfMemoryError =>
        size.smaller match {
          case Some(smallerSize) =>
            log.error(s"failed to display image $url at size $size due to OutOfMemoryError. trying $smallerSize instead")
            getImageBitmap(url, smallerSize)
          case None =>
            log.error(s"finally failed to display image $url at size $size due to OutOfMemoryError")
            None
        }
    }
  }

  def getPlaceholderBitmap(context: Context, size: ImageSize, placeholder: Drawable): Bitmap = {
    implicit val displayMetrics = DisplayMetrics(context)
    val pixels = if (size.size.dp > 0) size.size.toPx else displayMetrics.largestScreenDimension
    try {
      val bitmap = Bitmap.createBitmap(pixels, pixels, Bitmap.Config.ARGB_8888)
      val canvas = new Canvas(bitmap)
      placeholder.setBounds(0, 0, canvas.getWidth, canvas.getHeight)
      placeholder.draw(canvas)
      bitmap
    } catch {
      case ex: OutOfMemoryError =>
        log.error(s"failed to generate placeholder image with size of $pixels pixels")
        size.smaller match {
          case Some(smallerSize) => getPlaceholderBitmap(context, smallerSize, placeholder)
          case _ => throw ex
        }
    }
  }

  def getImageOrPlaceholderBitmap(context: Context, url: URL, size: ImageSize, placeholder: => Drawable): Bitmap =
    getImageBitmap(url, size).getOrElse(getPlaceholderBitmap(context, size, placeholder))
}

