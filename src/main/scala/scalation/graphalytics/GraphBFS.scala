
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Mon Nov 11 19:03:45 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.Queue

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphBFS` performs Breadth First Search on a Directed Graph.
 *  @param g  the directed graph to search
 */
class GraphBFS (g: Graph)
{
    private val qu = new Queue [Int] ()                 // vertex queue
    private val go = Array.ofDim [Boolean] (g.size)     // go flags

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Search for the label 'lab' in this graph.
     *  @param lab  the label to search for
     */
    def search (lab: TLabel): Int =
    {
        for (i <- 0 until g.size) go(i) = true
        for (i <- 0 until g.size if go(i)) {
            qu.enqueue (i)
            val res = visit (lab)
            if (res >= 0) return res
        } // for
        -1
    } // search

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit the next vertex and check its label.
     *  @param lab  the label to search for
     */
   private def visit (lab: TLabel): Int =
   {
       val j = qu.dequeue ()                          // take next vertex from queue
       go(j) = false                                  // mark as visited
       println ("label (" + j + ") = " + g.label (j))
       if (g.label(j) == lab) return j                // found label?
       for (c <- g.adj(j) if go(j)) qu.enqueue (c)    // put children in queue
       -1                                             // not found
   } // visit

} // GraphBFS class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphBFSTest` is used to test the `GraphBFS` class.
 */
object GraphBFSTest extends App
{
    val g = Graph (Array (Set (4, 5),                        // 0           // adj
                          Set (5),                           // 1
                          Set (6, 7),                        // 2
                          Set (7, 8),                        // 3
                          Set (0, 5, 9),                     // 4
                          Set (0, 1, 4, 6, 10),              // 5
                          Set (2, 5, 7, 10, 11),             // 6
                          Set (2, 3, 6, 8),                  // 7
                          Set (3, 7, 12),                    // 8
                          Set (4),                           // 9
                          Set (5, 6),                        // 10
                          Set (6),                           // 11
                          Set (8)),                          // 12
                   Array (1, 2, 3, 4 , 5, 6, 13, 12, 11, 10, 9, 8, 7))      // labels

    g.print ("g")

    val bfs = new GraphBFS (g)

    val lab = 12                                                                 // find label lab
    println ("search (" + lab + ") = " + bfs.search (lab))

} // GraphBFSTest object

