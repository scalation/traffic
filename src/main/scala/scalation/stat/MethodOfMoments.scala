
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Mar 13 15:49:38 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.sqrt

//import scalation.math.DoubleWithExp._
import scalation.math._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MethodOfMoments` object provides methods for estimating parameters
 *  for popular probability distributions using the Method of Moments (MOM).
 *  The main alternative is to use Maximum Likelihood Estimators (MLE).
 *  @see http://www.math.uah.edu/stat/point/Moments.html
 */
object MethodOfMoments
{
    /** Standard functional form for parameter estimating functions
     */
    type ParamFunction = (StatVector) => Array [Double]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameter 'p' for the `Bernoulli` distribution.
     *  @param x  the statistical data vector
     */
    def bernoulli (x: StatVector) = Array (x.mean)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameters 'a' (alpha) and 'b' (beta) for the `Beta` distribution.
     *  @param x  the statistical data vector
     */
    def beta (x: StatVector) =
    {
        val (m, m2) = (x.mean, x.ms)                   // first 2 moments
        val f = (m - m2) / (m2 - m~^2)
        Array (m * f, (1.0 - m) * f)
    } // beta

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameter 'mu' for the `Exponential` distribution.
     *  @param x  the statistical data vector
     */
    def exponential (x: StatVector) = Array (x.mean)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameters 'a' (alpha) and 'b' (beta) for the `Gamma` distribution.
     *  @param x  the statistical data vector
     */
    def gamma (x: StatVector) = 
    {
        val (m, v) = (x.mean, x.variance)
        Array (m~^2 / v, v / m)
    } // gamma

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameter 'p' for the `Geometric` distribution.
     *  @param x  the statistical data vector
     */
    def geometric (x: StatVector) = Array (1.0 / x.mean)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameters 'mu' and 'sigma2' for the `Normal` distribution.
     *  @param x  the statistical data vector
     */
    def normal (x: StatVector) = Array (x.mean, x.variance)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameters 'a' and 'b' for the `Pareto` distribution.
     *  @param x  the statistical data vector
     */
    def pareto (x: StatVector) =
    {
        val (m, m2) = (x.mean, x.ms)                   // first 2 moments
        val f = m2 - m~^2
        Array (1.0 + sqrt (m2 / f), m2 * (1.0 - sqrt (f / m2)) / m)
    } // pareto

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameter 'mu' for the `Poisson` distribution.
     *  @param x  the statistical data vector
     */
    def poisson (x: StatVector) = Array (x.mean)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the parameters 'a' and 'b' for the `Uniform` distribution.
     *  @param x  the statistical data vector
     */
    def uniform (x: StatVector) =
    {
        val (m, v) = (x.mean, x.variance)
        val d  = sqrt (3.0 * v)
        Array (m - d, m + d)
    } // uniform

} // MethodOfMoments object

