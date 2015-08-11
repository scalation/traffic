
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.2
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Dual Simulation Using Immutable Sets
 */

package scalation.graphalytics

import collection.immutable.{Set => SET}

import scalation.util.Timer.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2` class provides a second implementation for Dual Graph Simulation.
 *  It differs from DualSim by not using inverse adjacency sets ('par') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim2 (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    private val DEBUG = true                                      // debug flag

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

            for (u <- qRange; u_c <- q.adj(u)) {                  // for each u in q and its children u_c  
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                var newPhi = SET [Int] ()                         // subset of phi(u_c) having a parent in phi(u)

                for (v <- phi(u)) {                               // data vertex v matching u's label
                    val phiTemp = g.adj(v) & phi(u_c)             // children of v contained in phi(u_c)
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

} // DualSim2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test` object is used to test the `DualSim2` class.
 *  > run-main scalation.graphalytics.DigraphDualSimTest
 */
object DualSim2Test extends App
{
    val q = Graph (Array (SET (1, 2),                   // ch(0)
                          SET [Int] (),                 // ch(1)
                          SET (1)),                     // ch(2)
                   Array (10, 11, 11))

    val g = Graph (Array (SET [Int] (),                 // ch(0)
                          SET (0, 2, 3, 4),             // ch(1)
                          SET (0),                      // ch(2)
                          SET (4),                      // ch(3)
                          SET [Int] ()),                // ch(4)
                   Array (11, 10, 11, 11, 11))

    q.print ("q")
    g.print ("g")

    val matcher = new DualSim2 (g, q)                      // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    println ("DualSim2 ----------------------------------------------------")
    matcher.showMappings (phi)                             // display results

} // DualSim2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test2` object is used to test the 'DualSim2' class.
 */
object DualSim2Test2 extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree)
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g)

    val matcher = new DualSim2 (g, q)                      // Dual Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    for (i <- phi.indices) println ("u_" + i + ": " + phi(i)) 

} // DualSim2Test2 object

