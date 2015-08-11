
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller
 *  @version 1.2
 *  @date    Mon May  6 10:50:37 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 */

package scalation.graphalytics

import collection.immutable.{Set => SET}

import scalation.util.Timer.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = nisarGraphSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    private def nisarGraphSim (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                     // check for matching children
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c

            for (u <- qRange; v <- phi(u); u_c <- q.adj(u)) {
                if ((g.adj(v) & phi(u_c)).isEmpty) {        // v must have a child in phi(u_c)
                    phi(u) -= v                             // remove v due to lack of child match 
                    alter   = true
                } // if
            } //for           

        } // while
        phi
    } // nisarGraphSim

} // GraphSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.GraphSimTest
 */
object GraphSimTest extends App
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

    val matcher = new GraphSim (g, q)                      // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    println ("GraphSim ----------------------------------------------------")
    matcher.showMappings (phi)                             // display results

} // GraphSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest2` object is used to test the `GraphSim` class.
 */
object GraphSimTest2 extends App
{  
    if (args.length != 4) {
        println ("usage: scala -cp classes scalation.graphalytics.GraphSim [DataGraph] [QueryGraph] [Repeats] [DisplayResults(0/1)]")
    } else {
        val g = Graph (args(0), false)                        // data graph
        val q = Graph (args(1), false)                        // query graph
        val matcher = new GraphSim (g, q)                     // Simple Graph Simulation Pattern Matcher   
    
        for (i <- 0 until args(2).toInt) {                    // loop repeats times
//          g.labelMap.clear ()
            System.gc ()                                      // run garbage collector (gc)
            Thread.sleep (2000)                               // give gc time to work
            val sim = time { matcher.mappings () }            // time the matcher
            if (args(3).toInt == 1) for (u <- 0 until q.size) println (u + " -> " + sim(u))
        } // for
    } // if

} // GraphSimTest2 object

