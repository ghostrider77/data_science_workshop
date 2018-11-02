package image_compression

import breeze.linalg.DenseMatrix
import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

final case class ColoredImage(redChannelNormalized: DenseMatrix[Double],
                              greenChannelNormalized: DenseMatrix[Double],
                              blueChannelNormalized: DenseMatrix[Double],
                              height: Int,
                              width: Int)

object ImageReader {
  private val MaxIntensity: Double = 255.0

  def readImage(folder: String, filename: String): ColoredImage = {
    val image = ImageIO.read(new File(folder + "/" + filename))
    val height: Int = image.getHeight()
    val width: Int = image.getWidth()

    val red: DenseMatrix[Double] = DenseMatrix.zeros[Double](height, width)
    val green: DenseMatrix[Double] = DenseMatrix.zeros[Double](height, width)
    val blue: DenseMatrix[Double] = DenseMatrix.zeros[Double](height, width)

    for {
      ix <- 0 until height
      jy <- 0 until width
    } {
      val pixel: Int = image.getRGB(jy, ix)
      red(ix, jy) = (pixel >> 16) & 0xff
      green(ix, jy) = (pixel >> 8) & 0xff
      blue(ix, jy) = pixel & 0xff
    }
    ColoredImage(red / MaxIntensity, green / MaxIntensity, blue / MaxIntensity, height, width)
  }

  def writeImageToFile(folder: String, filename: String, content: ColoredImage): Unit = {
    val ColoredImage(red, green, blue, height, width) = content
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for {
      ix <- 0 until height
      jy <- 0 until width
      r = (red(ix, jy) * MaxIntensity).toInt
      g = (green(ix, jy) * MaxIntensity).toInt
      b = (blue(ix, jy) * MaxIntensity).toInt
      pixel = (r << 16) | (g << 8) | b
    } image.setRGB(jy, ix, pixel)

    val outfile = new File(folder + "/" + filename)
    ImageIO.write(image, "jpeg", outfile)
  }

}
