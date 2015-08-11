
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
/** The `GraphMatcherTest` object creates data graph and query graph that
 *  can be used to test the results produced by the pattern matching algorithms.
 *  @see MS Thesis, "A Comparison of Techniques for Graph Analytics on Big Data"
 */
object TestGraphContainer
{
    // Data Graph ------------------------------------------------------------

    /** The data graph adjacency sets, one for each vertex
     */
    val testGraphAdj = Array (SET (1, 2),
                              SET (2),
                              SET (7),
                              SET (7),
                              SET (7),
                              SET (6, 7),
                              SET (5),
                              SET [Int] (),
                              SET (11),
                              SET (7, 8),
                              SET (9),
                              SET (7, 10, 12),
                              SET [Int] (),
                              SET (10),
                              SET (7, 15),
                              SET (16),
                              SET (17, 20),
                              SET (18),
                              SET (19, 22),
                              SET (14),
                              SET [Int] (),
                              SET (20),
                              SET [Int] (),
                              SET (22))

    /** The vertex labels, one for each vertex in the data graph
     */
    val testGraphLabels = Array (0, 1, 2, 0, 0, 3, 4, 2, 4, 3, 4, 3, 0, 2, 3, 4, 3, 4, 3, 4, 2, 0, 2, 0)

    /** The test data graph
     */
    val testGraph = Graph (testGraphAdj, testGraphLabels)

    //------------------------------------------------------------------------
    // Query Graph
    //------------------------------------------------------------------------

    /** The query graph adjacency sets, one for each vertex
     */
    val testQueryAdj = Array (SET (3),           // PM
                              SET (3, 2),        // DB
                              SET (1),           // AI
                              SET [Int] ())      // SA

    /** The vertex labels, one for each vertex in the query graph
     */
    val testQueryLabels = Array (0, 3, 4, 2)

    /** The test query graph
     */
    val testQuery = Graph (testQueryAdj, testQueryLabels)

    //------------------------------------------------------------------------
    // Specified correct reults for Simple Graph Simulation, Dual Graph Simulation
    // and Subgraph Isomorphism
    //------------------------------------------------------------------------

    val correctSimpleResult = Array (SET (0, 3, 4, 21, 23), 
                                     SET (5, 9, 11, 14, 16, 18), 
                                     SET (6, 8, 10, 15, 17, 19),
                                     SET (2, 7, 13, 20, 22))
    val correctDualResult   = Array (SET (3, 4, 21, 23), 
                                     SET (5, 9, 11, 14, 16, 18), 
                                     SET (6, 8, 10, 15, 17, 19),
                                     SET (7, 20, 22))
    val correctIsoResult    = SET (Array (3, 5, 6, 7),
                                   Array (4, 5, 6, 7))
  
} // TestGraphContainer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphMatcherTest' object runs six pattern matching algorithms on
 *  the above test graph.  The algorithms tested are the following:
 *  GraphSim   - Simple Graph Simulation
 *  GraphSim2  - Simple Graph Simulation (with early termination)
 *  DualSim    - Dual Graph Simulation
 *  DualSim2   - Dual Graph Simulation (with reduced memory footprint) 
 *  UllmannIso - Adjacency List Version of Ullmann's Subgraph Isomorphism Algorithm
 *  DualIso    - Subgraph Isomorphism using Dual Simulation for Pruning
 */
object GraphMatcherTest extends App 
{
    import TestGraphContainer._

    val g = testGraph          // test query graph
    val q = testQuery          // test data graph

    def testBijections (matcher: GraphMatcher, name: String, answer: SET [Array [Int]])
    {
        println ("-------------------------------------------------------------------")
        println (name + " Bijections: ")
        val psi = time { matcher.bijections () }
        psi.foreach { b => println (b.mkString (", " )) }
        print (if (psi.map (_.deep) == answer.map (_.deep)) "Success: " else "Failure: ")
        println (name)
    } // test

    testBijections (new GraphSimIso (g, q), "GraphSimIso", correctIsoResult)
    testBijections (new DualIso (g, q),     "DualIso",     correctIsoResult)

    def testMappings (matcher: GraphMatcher, name: String, answer: Array [SET [Int]])
    {
        println ("-------------------------------------------------------------------")
        println (name + " Mappings: ")
        val phi = time { matcher.mappings () }
        for (i <- phi.indices) println (i + " ->  " + phi(i))
        print (if (phi.deep == answer.deep) "Success: " else "Failure: ")
        println (name)
    } // test

    testMappings (new GraphSim2 (g, q), "GraphSim2", correctSimpleResult)
    testMappings (new GraphSim (g, q),  "GraphSim",  correctSimpleResult)
    testMappings (new DualSim2 (g, q),  "DualSim2",  correctDualResult)

    // DualSim requires inverse adjacency ('par') for access to parent vertices
    g.addPar (); q.addPar ()
    testMappings (new DualSim (g, q),   "DualSim",   correctDualResult)

} // GraphMatcherTest

