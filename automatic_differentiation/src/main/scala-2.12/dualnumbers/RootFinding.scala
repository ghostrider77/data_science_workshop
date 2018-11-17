package dualnumbers

import scala.annotation.tailrec

object RootFinding {
  import Dual._

  private val maxNrIterations: Int = 20

  def evaluate(func: Dual => Dual, a: Double): Dual = {
    val x: Dual = new Dual(a, 1)
    func(x)
  }

  def findRoot(func: Dual => Dual, initial: Double, tolerance: Double = 1e-10): Option[Double] = {
    @tailrec
    def loop(current: Double, nrIterations: Int): Option[Double] = {
      val evaluated: Dual = evaluate(func, current)
      if (evaluated.isDefined && nrIterations < maxNrIterations) {
        val updated: Double = current - evaluated.value / evaluated.derivative
        if (math.abs(updated - current) / (math.abs(current) + tolerance) < tolerance) Some(updated)
        else loop(updated, nrIterations + 1)
      }
      else None
    }
    loop(initial, 0)
  }

  def main(args: Array[String]): Unit = {
    // val f: Dual => Dual = x => pow(x, 3) - 27
    // val x0: Double = 2.0
    val f: Dual => Dual = x => pow(x, 3) + x - 1000
    val x0: Double = 8
    val result: Option[Double] = findRoot(f, x0)
    result match {
      case None => println("Did not converge")
      case Some(root) => println(s"The root we have found is $root")
    }
  }
}
