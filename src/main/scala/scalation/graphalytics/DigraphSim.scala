
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Digraph Simulation Using Mutable Sets
 */

package scalation.graphalytics

import collection.mutable.{Set => SET}
//import collection.mutable.{HashSet => SET}

import scalation.util.Timer.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphSim` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from GraphSim in the looping order in the main for-loop
 *  and early termination when phi(u) is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DigraphSim (g: Digraph, q: Digraph)
      extends DigraphMatcher (g, q)
{
    private val DEBUG = true                                  // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = saltzGraphSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def saltzGraphSim (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                       // check for matching children
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {               // for each u in q and its children u_c
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val phi_u_c = phi(u_c)
                val v_rem   = SET [Int] ()                    // vertices to be removed

                for (v <- phi(u)) {                           // for each v in g image of u
                    if ((g.ch(v) & phi_u_c).isEmpty) {        // v must have a child in phi(u_c)
                        // phi(u) -= v                        // if not, remove vertex v from phi(u)
                        // if (phi(u).isEmpty) return phi     // no match for vertex u => no overall match
                        v_rem += v
                        alter = true
                    } // if
                    if (! v_rem.isEmpty) {
                        phi(u) --= v_rem                      // remove vertices in v_rem from phi(u)
                        if (phi(u).isEmpty) return phi        // no match for vertex u => no overall match
                    } // if
                } // for

            } // for

        } // while
        phi
    } // saltzGraphSim

} // DigraphSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphSimTest` object is used to test the `DigraphSim` class.
 *  > run-main scalation.graphalytics.DigraphSimTest
 */
object DigraphSimTest extends App
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

    val matcher = new DigraphSim (g, q)                    // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    println ("DigraphSim --------------------------------------------------")
    matcher.showMappings (phi)                             // display results

} // DigraphSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphSimTest2` object is used to test the `DigraphSim` class.
 */
object DigraphSimTest2 extends App
{
    val q = Digraph (Array (SET (1),
                            SET (0)),
                     Array (0, 1))

    val g = Digraph (Array (SET (1),
                            SET (0),
                            SET (0),
                            SET (2)),
                     Array (0, 1, 1, 0))

    q.print ("q")
    g.print ("g")

    val matcher = new DigraphSim (g, q)                    // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    matcher.showMappings (phi)                             // display results

} // DigraphSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphSimTest3` object is used to test the `DigraphSim` class.
 *
object DigraphSimTest3 extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree)
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g)

    val matcher = new DigraphSim (g, q)                  // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }           // time the matcher
    matcher.showMappings (phi)                           // show results

} // DigraphSimTest3 object
 */

