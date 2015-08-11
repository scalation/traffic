
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.2
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.immutable.{Set => SET}

import scalation.util.Timer.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from GraphSim in the looping order in the main for-loop
 *  and early termination when phi(u) is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim2 (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    private val DEGUG = true                    // debug flag

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
        while (alter) {                                          // check for matching children
            if (DEGUG) showMappings (phi)
            alter = false

            // loop over query vertices u, u's children u_c, and data vertices v in phi(u)

            for (u <- qRange; u_c <- q.adj(u); v <- phi(u)) {
                if ((g.adj(v) & phi(u_c)).isEmpty) {             // v must have a child in phi(u_c)
                    phi(u) -= v                                  // remove vertex v from phi(u)
                    if (phi(u).isEmpty) return phi               // no match for vertex u => no overall match
                    alter = true
                } // if
            } // for

        } // while
        phi
    } // saltzGraphSim

} // GraphSim2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test` object is used to test the `GraphSim2` class.
 *  > run-main scalation.graphalytics.GraphSim2Test
 */
object GraphSim2Test extends App
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

    val matcher = new GraphSim2 (g, q)                     // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    println ("GraphSim2 ---------------------------------------------------")
    matcher.showMappings (phi)                             // display results

} // GraphSim2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test2` object is used to test the `GraphSim2` class.
 */
object GraphSim2Test2 extends App
{
    val q = Graph (Array (SET (1),
                          SET (0)),
                   Array (0, 1))

    val g = Graph (Array (SET (1),
                          SET (0),
                          SET (0),
                          SET (2)),
                   Array (0, 1, 1, 0))

    q.print ("q")
    g.print ("g")

    val matcher = new GraphSim2 (g, q)                     // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    matcher.showMappings (phi)                             // display results

} // GraphSim2Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test3` object is used to test the `GraphSim2` class.
 */
object GraphSim2Test3 extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree)
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g)

    val matcher = new GraphSim2 (g, q)                     // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    matcher.showMappings (phi)                             // show results

} // GraphSim2Test3 object


