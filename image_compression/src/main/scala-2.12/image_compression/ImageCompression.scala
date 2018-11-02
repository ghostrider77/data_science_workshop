package image_compression

import ImageReader.{readImage, writeImageToFile}
import breeze.linalg.{DenseMatrix, DenseVector, diag, svd}

object ImageCompression {

  def compressImage(folder: String, filename: String, k: Int): ColoredImage = {
    val ColoredImage(red, green, blue, height, width) = readImage(folder, filename)

    def compressChannel(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
      val svd.SVD(u, d, v) = svd(matrix)

      val uk: DenseMatrix[Double] = u(::, 0 until k)
      val vk: DenseMatrix[Double] = v(0 until k, ::)
      val dk: DenseVector[Double] = d(0 until k)

      val compressedMatrix: DenseMatrix[Double] = uk * diag(dk) * vk
      cropOutliers(compressedMatrix, height, width)
      compressedMatrix
    }

    ColoredImage(compressChannel(red), compressChannel(green), compressChannel(blue), height, width)
  }

  def cropOutliers(matrix: DenseMatrix[Double], height: Int, width: Int): Unit = {
    for {
      ix <- 0 until height
      jy <- 0 until width
    } {
      if (matrix(ix, jy) < 0) matrix(ix, jy) = 0.0
      if (matrix(ix, jy) > 1) matrix(ix, jy) = 1.0
    }
  }

  def main(args: Array[String]): Unit = {
    val folder: String = "PATH_TO_FOLDER"
    val filename: String = "tour_eiffel.jpg"
    val k: Int = 100
    val compressedImage: ColoredImage = compressImage(folder, filename, k)
    writeImageToFile(folder, "compressed.jpg", compressedImage)
  }

}
