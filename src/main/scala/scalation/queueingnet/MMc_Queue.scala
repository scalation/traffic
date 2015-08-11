
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Dec 29 21:28:40 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://irh.inf.unideb.hu/~jsztrik/education/16/SOR_Main_Angol.pdf
 */

package scalation.queueingnet

import scalation.math.Combinatorics.fac
//import scalation.math.DoubleWithExp._
import scalation.math._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MMc_Queue` class is used to solve single node Markovian Queueing problems.
 *  It models a service station consisting of one queue and 'c' servers, i.e.,
 *  an M/M/c queue.
 *  @see also `MMck_Queue` to model finite capacity queues.
 *  @param lambda  the arrival rate
 *  @param mu      the service rate
 *  @param c       the number of servers
 */
class MMc_Queue (lambda: Double, mu: Double, c: Int = 1)
      extends Error
{
     if (c < 1) flaw ("constructor", "must have at least on server")

     private val rho    = lambda / mu              // traffic intensity
     private val a      = rho / c.toDouble         // server utilization factor
     private val c_fac  = fac (c)                  // c! (factorial)
     private val rhoc   = rho~^c / c_fac           // all servers busy probability factor
     private val _1_a   = 1.0 - a                  // one minus a
     private val pr_0   = prob_0                   // probability system is empty

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Probability system is empty.
      */
     def prob_0: Double =
     {
         val sum = (for (i <- 0 until c) yield rho~^i / fac (i)).sum
         1.0 / (sum + rho~^c / (c_fac * _1_a))
     } // prob_0

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length of the waiting queue.
      */
     val l_q = pr_0 * rho * rhoc / _1_a~^2

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length/number in Service.
      */
     val l_s = lambda / mu

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length/number in sYstem.
      */
     val l_y = l_q + l_s

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected time in the waiting Queue (using Little's Law).
      */
     val t_q = l_q / lambda

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected time in Service.
      */
     val t_s = 1.0 / mu

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected time in the sYstem.
      */
     val t_y = t_q + t_s

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Check intermediate results.
      */
     def check
     {
         println ("Check queueing parameters:")
         println ("lambda = %g".format (lambda))                   // arrival rate
         println ("mu     = %g".format (mu))                       // service rate
         println ("c      = %d".format (c))                        // number of servers
         println ("rho    = %g".format (rho))                      // traffic intensity
         println ("a      = %g".format (a))                        // server utilization factor
     } // check

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Report the results.
      */
     def report
     {
         println ("Results for queue:")
         println ("---------------------------------------------------")
         println ("|  Queue    |  l_q = %8.4g".format (l_q) + "  |  t_q = %8.4g".format (t_q) + "  |")
         println ("|  Service  |  l_s = %8.4g".format (l_s) + "  |  t_s = %8.4g".format (t_s) + "  |")
         println ("|  sYstem   |  l_y = %8.4g".format (l_y) + "  |  t_y = %8.4g".format (t_y) + "  |")
         println ("---------------------------------------------------")
     } // report

} // MMc_Queue class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MMc_QueueTest` object is used to test the `MMc_Queue` class.
 */
object MMc_QueueTest extends App
{
    val lambda = 6.0                                   // customer arrival rate (per hour)
    val mu     = 7.5                                   // customer service rate (per hour)

    println("\nM/M/1 Queue Results:")
    val mm1 = new MMc_Queue (lambda, mu, 1)           // M/M/c Queue
    mm1.check
    mm1.report

    println("\nM/M/2 Queue Results:")
    val mm2 = new MMc_Queue (lambda, mu, 2)           // M/M/c Queue
    mm2.check
    mm2.report

} // MMc_QueueTest object

