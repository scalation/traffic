
package apps.traffic

import scala.util.Sorting.stableSort

import scalation.linalgebra._
import scalation.random._

class TrafficSPSA (f: VectorD => Double)
{

    private val DEBUG = true

    val rand = Bernoulli2 ()
    
    var h    = Array (5000.0, 5000.0, 1000.0)

    var best: Tuple2[VectorD, Double] = null

    def solve (lower: Array [Int], upper: Array [Int]): Tuple2 [VectorD, Double] = 
    {
        val x0 = findStart (lower, upper)
        println ("x0 = " + x0)
        best = (x0, f(x0))
        val n = x0.dim
        var done = false
        var x = new VectorD (x0)
        var k = 1
        while (! done || k == 50) {
            val dist = VectorD (5000000.0 / k, 5000000.0 / k, 1000000.0 / k)
            val delta = new VectorD (n)
            val x1    = new VectorD (x)
            val x2    = new VectorD (x)
            val grad  = new VectorD (n)
            for (i <- 0 until n) {
                val d = h(i) * rand.gen
                delta(i)  = d
                x1(i)    += d
                x2(i)    -= d
            }
            println ("x  = " + x)
            println ("x1 = " + x1)
            println ("x2 = " + x2)
            val top = f(x1) - f(x2)
            for (i <- 0 until n) grad(i) = top * 1000.0 / (2.0 * delta(i))
            println ("grad = " + grad)
            
            val xx = x - grad * dist
            val fx = f(xx)            
            println ("xx = " + xx + ", f(xx) = " + fx)
/*            if (fx < best._2) {
                best = (xx, fx)
                x = xx
                if (DEBUG) println ("solve: (k = " + k + ") move from " + x + " to " + xx
                                  + " where f(xx._1) = " + fx) 
            }
*/
            x = xx
            best = (x, fx)
            k += 1            
        } 
        best
    }   

    def findStart (lower: Array [Int], upper: Array [Int]): VectorD = 
    {
        val candidates = Array.ofDim [VectorD] (10)
        val rand = Array.ofDim [Variate] (3)
        for (i <- 0 until 3) rand(i) = Randi (lower(i), upper(i))
        for (i <- 0 until 10) {
            val member = new VectorD (3)            
            for (j <- 0 until 3) {
                member(j) = rand(j).igen * 1000.0
            }
            candidates(i) = member
        }
        val eval = Array.ofDim [Tuple2 [VectorD, Double]] (10)
        for (i <- 0 until 10) eval(i) = (candidates(i), f (candidates(i))) 
        stableSort (eval, (e1: Tuple2 [VectorD, Double], e2: Tuple2 [VectorD, Double]) => e1._2 < e2._2)
        eval(0)._1
    }
}       










