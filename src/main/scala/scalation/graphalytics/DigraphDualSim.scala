
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Digraph Dual Simulation Using Mutable Sets
 */

package scalation.graphalytics

import collection.mutable.{Set => SET}

import scalation.util.Timer.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphDualSim` class provides a second implementation for Dual Graph Simulation.
 *  It differs from DualSim by not using inverse adjacency sets ('par') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DigraphDualSim (g: Digraph, q: Digraph)
      extends DigraphMatcher (g, q)
{
     private val DEBUG = true                                     // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = saltzDualSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def saltzDualSim (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                           // check for matching children/parents
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {                   // for each u in q and its children u_c
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                var newPhi = SET [Int] ()                         // subset of phi(u_c) having a parent in phi(u)

                for (v <- phi(u)) {                               // for each v in g image of u
                    val phiTemp = g.ch(v) & phi(u_c)              // children of v contained in phi(u_c)
                    if (phiTemp.isEmpty) {
                        phi(u) -= v                               // remove vertex v from phi(u)
                        if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                        alter = true
                    } // if
                    // build newPhi to contain only those vertices in phi(u_c) which also have a parent in phi(u)
                    newPhi ++= phiTemp
                } // for

                if (newPhi.isEmpty) return phi                    // empty newPhi => no match
                if (newPhi.size < phi(u_c).size) alter = true     // since newPhi is smaller than phi(u_c)

                if (SELF_LOOPS && u_c == u) phi(u_c) &= newPhi else phi(u_c) = newPhi
            } // for

        } // while
        phi
    } // saltzDualSim

} // DigraphDualSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphDualSimTest` object is used to test the `DigraphDualSim` class.
 *  > run-main scalation.graphalytics.DigraphDualSimTest
 */
object DigraphDualSimTest extends App
{
    val q = Digraph (Array (SET (1, 2),                   // ch(0)
                            SET (),                       // ch(1)
                            SET (1)),                     // ch(2)
                     Array (10, 11, 11))

    val g = Digraph (Array (SET (),                       // ch(0)
                            SET (0, 2, 3, 4),             // ch(1)
                            SET (0),                      // ch(2)
                            SET (4),                      // ch(3)
                            SET ()),                      // ch(4)
                     Array (11, 10, 11, 11, 11))

    q.print ("q")
    g.print ("g")

    val matcher = new DigraphDualSim (g, q)                // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    println ("DigraphDualSim ----------------------------------------------")
    matcher.showMappings (phi)                             // display results

} // DigraphDualSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphDualSimTest2` object is used to test the 'DigraphDualSim' class.
 *
object DigraphDualSimTest2 extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree)
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g)

    val matcher = new DigraphDualSim (g, q)                      // Dual Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    for (i <- phi.indices) println ("u_" + i + ": " + phi(i)) 

} // DigraphDualSimTest2 object
 */

