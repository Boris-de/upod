package mobi.upod.app.services.sync

import android.graphics.{Bitmap, BitmapFactory}
import com.escalatesoft.subcut.inject.{BindingModule, Injectable}
import java.io.{FileOutputStream, File}
import java.net.URL
import mobi.upod.android.logging.Logging
import mobi.upod.android.os.AsyncTask
import mobi.upod.android.view.DisplayMetrics
import mobi.upod.app.AppUpgradeListener
import mobi.upod.app.storage.{PodcastDao, ImageSize, CoverartProvider}
import mobi.upod.io._
import mobi.upod.net._

import scala.util.Try

class ImageFetcher(implicit val bindingModule: BindingModule) extends Injectable with AppUpgradeListener with Logging {
  private val coverartProvider = inject[CoverartProvider]
  private lazy val urls = forCloseable(inject[PodcastDao].listAllImageUrls)(_.toSet)
  private implicit val metrics = inject[DisplayMetrics]

  override def onAppUpgrade(oldVersion: Int, newVersion: Int): Unit = {
    if (oldVersion < 4303) {

      def createLargestScreenDimensionVariation(): Unit = {
        urls foreach { url =>
          coverartProvider.getExistingImageFile(url, ImageSize.full) foreach { fullSizeFile =>
            log.info(s"creating size largestScreenDimension for $url")
            Try(downScaleImage(
              fullSizeFile,
              coverartProvider.getImageFile(url, ImageSize.largestScreenDimension),
              metrics.largestScreenDimension))
          }
        }
      }

      log.info("creating size largestScreenDimension for all coverart images")
      AsyncTask.execute(createLargestScreenDimensionVariation())
    }
  }


  def sync() {
    if (coverartProvider.storageAvailable) {
      cleanUpImages()
      fetchImages()
    } else {
      log.warn("skipping coverart sync as storage isn't available")
    }
  }

  private def cleanUpImages() {
    val noLongerRequiredImages = coverartProvider.availableImages -- urls
    noLongerRequiredImages.foreach { url =>
      coverartProvider.deleteImage(url)
    }
  }

  private def fetchImages() {
    val newImages = urls -- coverartProvider.availableImages
    newImages foreach { url =>
      fetchImage(url)
    }
  }

  def fetchImage(url: URL) {
    try {
      log.info(s"fetching image $url")
      coverartProvider.prepareImage(url)
      url.downloadTo(coverartProvider.getImageFile(url, ImageSize.full))
      createDownscaledVariations(url)
    } catch {
      case ex: Throwable =>
        log.warn(s"failed to download image $url", ex)
    }
  }

  private def createDownscaledVariations(url: URL) {
    val src = coverartProvider.getImageFile(url, ImageSize.full)
    downScaleImage(src, coverartProvider.getImageFile(url, ImageSize.largestScreenDimension), metrics.largestScreenDimension)
    downScaleImage(src, coverartProvider.getImageFile(url, ImageSize.smallestScreenDimension), metrics.smallestScreenDimension)
    downScaleImage(src, coverartProvider.getImageFile(url, ImageSize.grid), ImageSize.grid.size.px)
    downScaleImage(src, coverartProvider.getImageFile(url, ImageSize.list), ImageSize.list.size.px)
  }

  private def loadImageSize(src: File): (Int, Int) = {
    val options = new BitmapFactory.Options
    options.inJustDecodeBounds = true
    BitmapFactory.decodeFile(src.getAbsolutePath, options)
    (options.outWidth, options.outHeight)
  }

  private def loadImageForDownscaling(src: File, srcWidth: Int, srcHeight: Int, targetSize: Int): Bitmap = {
    val largestEdge = math.max(srcWidth, srcHeight)
    val options = new BitmapFactory.Options
    options.inSampleSize = largestEdge / targetSize
    log.debug(s"loading image $src with size of $srcWidth x $srcHeight with inSampleSize of ${options.inSampleSize}")
    BitmapFactory.decodeFile(src.getAbsolutePath, options)
  }

  private def downScaleImage(src: File, target: File, targetSize: Int) {
    val (width, height) = loadImageSize(src)
    if (width <= targetSize && height <= targetSize) {
      log.debug(s"copying $src to $target")
      src.copyTo(target)
    } else {
      downScaleImage(loadImageForDownscaling(src, width, height, targetSize), target, targetSize)
    }
  }

  private def downScaleImage(src: Bitmap, target: File, targetSize: Int) {

    def calculateDimensions = {
      val aspectRatio = src.getWidth.toDouble / src.getHeight
      if (src.getWidth >= src.getHeight)
        (targetSize, (targetSize / aspectRatio).round.toInt)
      else
        ((targetSize * aspectRatio).round.toInt, targetSize)
    }

    val (width, height) = calculateDimensions
    log.debug(s"downscaling from ${src.getWidth} x ${src.getHeight} to $width x $height")
    saveBitmap(Bitmap.createScaledBitmap(src, width, height, true), target)
  }

  private def saveBitmap(bmp: Bitmap, target: File) {
    forCloseable(new FileOutputStream(target).buffered) { stream =>
      val format = if (bmp.hasAlpha) Bitmap.CompressFormat.PNG else Bitmap.CompressFormat.JPEG
      if (!bmp.compress(format, 90, stream)) {
        log.warn(s"Failed to compress bitmap as $format to $target")
      }
    }
  }
}
