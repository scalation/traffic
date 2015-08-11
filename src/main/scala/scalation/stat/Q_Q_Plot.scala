
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Mar  8 14:24:11 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import scalation.linalgebra.VectorD
import scalation.plot.Plot
import scalation.random.Quantile._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Q_Q_Plot` object produces Quantile-Quantile plots that are used to
 *  compare probability distributions.
 */
object Q_Q_Plot
{
    private val DEBUG = true               // debug flag

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a Q-Q plot for the two data vectors.
     *  @param fv  the first data vector
     *  @param gv  the second data vector
     */
    def plot (fv: VectorD, gv: VectorD)
    {
        val n = fv.dim
        if (gv.dim != n) flaw ("plot", "vectors must have the same size")
        val pv = new VectorD (n)
        for (i <- 1 until n) {
            val p   = i / n.toDouble
            pv(i-1) = p
            if (DEBUG) println ("pv = " + pv + ", fv = " + fv(i-1) + ", gv = " + gv(i-1))
        } // for
        new Plot (pv, fv, gv)
    } // plot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a Q-Q plot for the data vector and the distribution.
     *  @param fv    the data vector
     *  @param gInv  the inverse CDF
     *  @param g_df  the degrees of freedom for the distribution
     */
    def plot (fv: VectorD, gInv: ICDF, g_df: Array [Int])
    {
        val n = fv.dim
        val gv = new VectorD (n)          // to hold vector of values for gInv
        for (i <- 1 until n) {
            val p   = i / n.toDouble
            gv(i-1) = gInv (p, g_df)
        } // for
        plot (fv, gv)
    } // plot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a Q-Q plot for the two distribution.
     *  @param fInv  the first inverse CDF
     *  @param f_df  the degrees of freedom for the first distribution
     *  @param gInv  the second inverse CDF
     *  @param g_df  the degrees of freedom for the second distribution
     *  @param n     the number of intervals
     */
    def plot (fInv: ICDF, f_df: Array [Int], gInv: ICDF, g_df: Array [Int], n: Int)
    {
        val fv = new VectorD (n)          // to hold vector of values for fInv
        val gv = new VectorD (n)          // to hold vector of values for gInv
        for (i <- 1 until n) {
            val p   = i / n.toDouble
            fv(i-1) = fInv (p, f_df)
            gv(i-1) = gInv (p, g_df)
        } // for
        plot (fv, gv)
    } // plot

} // Q_Q_Plot object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Q_Q_PlotTest` object is used to test the `Q_Q_Plot` object.
 */
object Q_Q_PlotTest extends App
{
     Q_Q_Plot.plot (normalInv, Array (), studentTInv, Array (10), 100)

} // Q_Q_PlotTest object

