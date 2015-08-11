
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

// U N D E R   D E V E L O P M E N T 

package scalation.analytics

import math.{abs, sqrt}

import scalation.linalgebra.{Fac_Cholesky, Fac_QR, Factorization, MatriD, MatrixD, VectorD}
//import scalation.math.DoubleWithExp._
import scalation.math._
import scalation.plot.Plot
import scalation.util.Error
import scalation.util.Timer.time

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_WLS` class supports weighted multiple linear regression.
 *  In this case, 'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Weighted Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y   [ alternative: b  =  solve (y) ]
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.  Three techniques are provided:
 *  <p>
 *      Fac_QR         // QR Factorization: slower, more stable (default)
 *      Fac_Cholesky   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      Inverse        // Inverse/Gaussian Elimination, classical textbook technique (outdated)
 *  <p>
 *  @see www.markirwin.net/stat149/Lecture/Lecture3.pdf
 *  @param x          the input/design m-by-n matrix augmented with a first column of ones
 *  @param y          the response vector
 *  @param w          the weight vector
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class Regression_WLS (x: MatrixD, y: VectorD, private var w: VectorD = null, technique: RegTechnique = Fac_QR)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 <= x.dim2) flaw ("constructor", "not enough data rows in matrix to use regression")

    private val DEBUG      = true                              // debug flag
    private val k          = x.dim2 - 1                        // number of variables (k = n-1)
    private val m          = x.dim1.toDouble                   // number of data points (rows)
    private val r_df       = (m-1.0) / (m-k-1.0)               // ratio of degrees of freedom
    private var b: VectorD = null                              // parameter vector [b_0, b_1, ... b_k]
    private var e: VectorD = null                              // residual vector [e_0, e_1, ... e_m-1]
    private var rSquared   = -1.0                              // coefficient of determination (quality of fit)
    private var rBarSq     = -1.0                              // Adjusted R-squared
    private var fStat      = -1.0                              // F statistic (quality of fit)

    if (w == null) {
        val ols_y = new Regression (x, y, technique)           // run OLS on data
        ols_y.train ()
        val e = ols_y.residual                                 // deviations/errors
        val r = e.map ((a: Double) => sqrt (abs (a)))          // root absolute deviations (rad's)

        val ols_r = new Regression (x, r, technique)           // run OLS on rad
        ols_r.train ()
        val rp = ols_r.predict (x)                             // predicted rad
        w      = rp.map ((a: Double) => 1.0 / a)               // set weight vector for WLS to reciporcal of rp

        if (DEBUG) {
            println ("b_OLS      = " + ols_y.fit._1)           // Ordinary Least Squares (OLS)
            println ("residual e = " + e)
            println ("rad r      = " + r)
            println ("rad rp     = " + rp)
            println ("weights    = " + w)
        } // if
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  using the weighted least squares (WLS) method.
     */
    def train ()
    {
        val xw = new MatrixD (x)                                // x multiplied by weights
        for (i <- 0 until x.dim1) xw(i) *= w(i)

        b = (xw .t * x).inverse * x.t * (w * y)                 // perform WLS to fit parameters
        e = y - x * b                                           // compute errors/residuals

        val sse  = e dot e                                      // residual/error sum of squares
        val sst  = (y dot y) - y.sum~^2.0 / m                   // total sum of squares
        val ssr  = sst - sse                                    // regression sum of squares
        rSquared = ssr / sst                                    // coefficient of determination (R-squared)
        rBarSq   = 1.0 - (1.0-rSquared) * r_df                  // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k)                 // F statistic (msr / mse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  using the weighted least squares (WLS) method.
     *  @param yy  the new response vector
     */
    def train (yy: VectorD)
    {
        val xw = new MatrixD (x)                                // x multiplied by weights
        for (i <- 0 until x.dim1) xw(i) *= w(i)

        b = (xw .t * x).inverse * x.t * (w * yy)                // perform WLS to fit parameters
        e = yy - x * b                                          // compute errors/residuals

        val sse  = e dot e                                      // residual/error sum of squares
        val sst  = (yy dot yy) - yy.sum~^2.0 / m                // total sum of squares
        val ssr  = sst - sse                                    // regression sum of squares
        rSquared = ssr / sst                                    // coefficient of determination (R-squared)
        rBarSq   = 1.0 - (1.0-rSquared) * r_df                  // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k)                 // F statistic (msr / mse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals.
     */
    def residual: VectorD = e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit including rSquared).
     */
    def fit: Tuple4 [VectorD, Double, Double, Double] = (b, rSquared, rBarSq, fStat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectorD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatriD): VectorD = z * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    def backElim (): Tuple4 [Int, VectorD, Double, Double] =
    {
        var j_max   = -1                     // index of variable to eliminate
        var b_max: VectorD = null            // parameter values for best solution
        var rSq_max = -1.0                   // currently maximizing R squared
        var fS_max  = -1.0                   // could optimize on F statistic

        for (j <- 1 to k) {
            val keep = m.toInt               // i-value large enough to not exclude any rows in slice
            val rg_j = new Regression (x.sliceExclude (keep, j), y)       // regress with x_j removed
            rg_j.train ()
            val (b, rSq, fS, rBar) =  rg_j.fit
            if (rSq > rSq_max) { j_max = j; b_max = b; rSq_max = rSq; fS_max = fS}
        } // for
        (j_max, b_max, rSq_max, fS_max)
    } // backElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-colinearity by regressing xj against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the varaince of xj can be predicted
     *  from the other variables, so xj is a candidate for removal from the model.
     */
    def vif: VectorD =
    {
        val vifV = new VectorD (k)           // VIF vector
        for (j <- 1 to k) {
            val keep = m.toInt               // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                           // x_j is jth column in x
            val rg_j = new Regression (x.sliceExclude (keep, j), x_j)     // regress with x_j removed
            rg_j.train ()
            vifV(j-1) =  1.0 / (1.0 - rg_j.fit._2)                        // store vif for x_1 in vifV(0)
        } // for
        vifV
    } // vif

} // Regression_WLS class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_WLSTest` object tests `Regression_WLS` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  Test regression and backward elimination.
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 */
object Regression_WLSTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x)
    println ("y = " + y)

    val rg = new Regression_WLS (x, y)
    rg.train ()
    println ("fit = " + rg.fit)
    val yp = rg.predict (z)                              // predict y for one point
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                             // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

    println ("reduced model: fit = " + rg.backElim ())   // eliminate least predictive variable

} // Regression_WLSTest object

