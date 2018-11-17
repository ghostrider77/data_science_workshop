package dualnumbers

import spire.implicits._
import spire.math._
import scala.annotation.tailrec

object Jets {
  implicit val dim = JetDim(1)
  private val maxNrIterations: Int = 20

  def findRoot(func: Jet[Double] => Jet[Double], initial: Double, tolerance: Double = 1e-10): Option[Double] = {
    @tailrec
    def loop(current: Jet[Double], nrIterations: Int): Option[Double] = {
      val evaluated: Jet[Double] = func(current)
      if (nrIterations < maxNrIterations) {
        val updated: Double = current.real - evaluated.real / evaluated.infinitesimal(0)
        if (math.abs(updated - current.real) / (math.abs(current.real) + tolerance) < tolerance) Some(updated)
        else loop(updated + Jet.h[Double](0), nrIterations + 1)
      }
      else None
    }
    loop(initial + Jet.h[Double](0), 0)
  }

  def main(args: Array[String]): Unit = {
    // val f: Jet[Double] => Jet[Double] = x => x.pow(3) - 27
    // val x0: Double = 2
    val f: Jet[Double] => Jet[Double] = x => x.pow(3) + x - 1
    val x0: Double = 0.5
    val result: Option[Double] = findRoot(f, x0)
    result match {
      case None => println("Did not converge")
      case Some(root) => println(s"The root we have found is $root")
    }
  }
}
