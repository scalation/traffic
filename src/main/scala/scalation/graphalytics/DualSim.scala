
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller
 *  @version 1.2
 *  @date    Mon May  6 10:50:37 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  Graph Dual Simulation Using Immutable Sets
 */

package scalation.graphalytics

import collection.immutable.{Set => SET}

import scalation.util.Timer.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSim' classs provides an implementation for Dual Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = nisarDualSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def nisarDualSim (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                     // check for matching children and parents
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c

            for (u <- qRange; v <- phi(u); u_c <- q.adj(u)) {
                if ((g.adj(v) & phi(u_c)).isEmpty) {        // v must have a child in phi(u_c)
                    phi(u) -= v                             // remove v due to lack of child match 
                    alter  = true
                } // if
            } //for

            // loop over query vertices u, data vertices v in phi(u), and u's parents u_p

            for (u <- qRange; v <- phi(u); u_p <- q.par(u)) {
                if ((g.par(v) & phi(u_p)).isEmpty) {        // v must have a parent in phi(u_p)
                    phi(u) -= v                             // remove v due to lack of parent match
                    alter   = true
                } // if
            } //for

        } // while
        phi
    } // nisarDualSim

} // DualSim


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimTest` object is used to test the `DualSim` class.
 *  > run-main scalation.graphalytics.DigraphDualSimTest
 */
object DualSimTest extends App
{
    val q = Graph (Array (SET (1, 2),                   // ch(0)
                          SET (),                       // ch(1)
                          SET (1)),                     // ch(2)
                   Array (10, 11, 11), true)            // true, since parents 'par' is used

    val g = Graph (Array (SET (),                       // ch(0)
                          SET (0, 2, 3, 4),             // ch(1)
                          SET (0),                      // ch(2)
                          SET (4),                      // ch(3)
                          SET ()),                      // ch(4)
                   Array (11, 10, 11, 11, 11), true)    // true, since parents 'par' is used

    q.print ("q")
    g.print ("g")

    val matcher = new DualSim (g, q)                       // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    println ("DualSim -----------------------------------------------------")
    matcher.showMappings (phi)                             // display results

} // DualSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSimTest2' object is used to test the 'DualSim' class.
 */
object DualSimTest2 extends App
{  
    if (args.length != 4) {
        println ("usage: scala -cp classes scalation.graphalytics.DualSim [DataGraph] [QueryGraph] [Repeats] [DisplayResults(0/1)]")
    } else {
        val g = Graph (args(0), true)                         // data graph
        val q = Graph (args(1), true)                         // query graph
        val matcher = new DualSim (g, q)                      // Dual Graph Simulation Matcher

        for (i <- 0 until args(2).toInt) {                    // loop Repeats times
//          g.labelMap.clear ()
            System.gc ()                                      // run garbage collector (gc)
            Thread.sleep (2000)                               // give gc time to work
            val sim = time { matcher.mappings () }            // time the matcher
            if (args(3).toInt == 1) for (u <- 0 until q.size) println (u + " -> " + sim(u))
        } // for
    } // if

} // DualSimTest2 object

