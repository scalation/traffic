
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman
 *  @version 1.1
 *  @date    Fri Feb 13 22:17:23 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import math.{abs, max, pow}

import scalation.calculus.Calculus.{FunctionV2S, gradient, gradientD}
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra.MatrixD.{eye, outer}
import scalation.util.Error
import scalation.random._
import scalation.math._

class SPSA (f: FunctionV2S)
      extends Minimizer with Error
{
    private val DEBUG = true

    type Pair = Tuple2 [VectorD, VectorD]

    private val rand = Bernoulli2 ()
 
    private val alpha = 0.602     // From Spall: http://www.jhuapl.edu/spsa/PDF-SPSA/Spall_Implementation_of_the_Simultaneous.PDF

    private val gamma = 0.101 
 
    private val A = 50

    private val a = 0.10665

    private val c = 1e-6
    
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double = 0.0  

    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): VectorD =
    {
        println ("x0 = " + x0) 
        var ak = pow (a / (A + 1), alpha)
        var ck = c 
        var grad = gradEst (f, x0, ck)
        if (DEBUG) println ("Gradient Estimation = " + grad)
        var x  = x0 - grad * ak  
        var xx = new VectorD (x0)       

        for (k <- 1 to 500 if !terminate (x, xx)) {
            x = xx
            ak = pow (a / (A + k + 1), alpha)
            ck = pow (c / (k + 1), gamma)
            grad = gradEst (f, x, ck)
            if (DEBUG) println ("Gradient Esimation  = " + grad)
            xx = x - grad * ak
            if (DEBUG) println ("solve: (k = " + k + ") move from " + x + " to " + xx
                              + " where f(xx._1) = " + f(xx))
        }
        x
    }

    def terminate (xnew: VectorD, xold: VectorD): Boolean =
    {
        var term = false
        var max = 0.0
        for (i <- 0 until xnew.dim) {
            val diff = abs (xnew(i) - xold(i))
//            println ("diff = " + diff)
            if (diff > max) max = diff
        }
//        println ("max = " + max)
        if (max < 1e-3) term = true
        term
    }  

    def gradEst (f: FunctionV2S, x: VectorD, ck: Double): VectorD =
    {
        val n = x.dim
        val delta = new VectorD (n)
        val y  = new VectorD (n)
        val x1 = new VectorD (x)
        val x2 = new VectorD (x)
        
        for (i <- 0 until n) { 
            val d = rand.gen
            delta(i) = d
            x1(i) += d * ck
            x2(i) -= d * ck
        }
        val top = f(x1) - f(x2)
        for (i <- 0 until n) y(i) = top / (2.0 * ck * delta(i))
        y
    }
}

/*
object SPSATest extends App
{
    def x0 = new VectorD (2)

    println ("\nMinimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    var solver = new SPSA (f)
    var x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value f(x) = " + f(x))

    println ("\nMinimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def g (x: VectorD): Double = pow (x(0), 4.0) + (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    solver = new SPSA (g)
    x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value g(x) = " + g(x))
}

object SPSATest2 extends App
{
    def f (x: VectorD): Double = (x(0) - 1.0) ~^ 2.0 + (x(1) - 2.0) ~^ 2.0 + (x(2) - 3.0) ~^ 2.0 + (x(3) - 4.0) ~^ 2.0 + 1.0
    var solver = new SPSA (f)
    var x = solver.solve (new VectorD (4))
    println ("optimal solution x = " + x + " with an objective value f(x) = " + f(x))
}
*/
