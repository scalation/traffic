
package apps.minima

import scalation.linalgebra.VectorD
import scalation.minima._

object OptTest extends App
{
    def f (x: VectorD): Double = x(0) * x(0) * x(0) / 3.0 - 4.0 * x(0)

    val opt = new QuasiNewton (f)
    val x0 = VectorD (1.0)
    val res = opt.solve (x0)
    println ("Solution: " + res)
}
