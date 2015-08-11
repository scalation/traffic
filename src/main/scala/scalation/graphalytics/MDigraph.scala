
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  MultiDigraph (MDigraph) Data Structure Using Mutable Sets
 */

package scalation.graphalytics

import collection.mutable.Map
import collection.mutable.{Set => SET}
//import collection.mutable.{HashSet => SET}

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDigraph` class stores vertex/edge-labeled multi-directed graphs using
 *  an adjacency set ('ch') representation, e.g., ch = { {1, 2}, {0}, {1} } means
 *  that the graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'pa' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
 *  @param label    the array of verter labels
 *  @param elabel   the map of edge labels
 *  @param inverse  whether to store inverse adjacency sets (parents)
 */
class MDigraph (ch:      Array [SET [Int]],
                label:   Array [TLabel] = Array.ofDim (0),
            val elabel:  Map [Tuple2 [Int, Int], TLabel] = Map (),
                inverse: Boolean = false)
      extends Digraph (ch, label, inverse) with Cloneable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of this graph.
     */
    override def clone: MDigraph = MDigraph (ch.clone, label.clone, elabel.clone, inverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the edges in the elabel map correspond to edges in the
     *  the adjacency list.
     */
    def checkElabels: Boolean =
    {
        for ((u, v) <- elabel.keys if ! (ch(u) contains v)) {
            flaw ("checkElabel", "no such edge from " + u + " to " + v)
            return false
        } // for
        true
    } // checkElabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this graph to Digraph.
     *  use print to show graph details.
     */
    def toDigraph: Digraph = Digraph (ch, label, inverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate basic information about this graph.  Due to its potential size,
     *  use print to show graph details.
     */
    override def toString: String = "MDigraph with " + size + " vertices"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prints the adjacency list and labelMap of the graph.
     *  @param name  the name of the graph
     */
    override def print (name: String = "g")
    {
        println ("MDigraph " + name + " ---------------------------------------")
        println ("ch: "); ch.foldLeft (0) { (i, u) => { println (i + " -> " + u); i+1 } }
        println ("labelMap: "); labelMap.foreach { case (k, v) => println (k + " -> " + v) }
        println ("elabel: "); elabel.foreach { case (k, v) => println (k + " -> " + v) }
    } // print

} // MDigraph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Digraph` companion object provides build methods for directed graphs.
 */
object MDigraph
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a directed graph.
     *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
     *  @param label    the array of verter labels
     *  @param elabel   the map of edge labels
     *  @param inverse  whether to store inverse adjacency sets (parents)
     */
    def apply (ch:      Array [SET [Int]],
               label:   Array [TLabel] = Array.ofDim (0),
               elabel:  Map [Tuple2 [Int, Int], TLabel] = Map (),
               inverse: Boolean = false) =
    {
        new MDigraph (ch, label, elabel, inverse)
    } // apply

} // MDigraph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDigraphTest` object is used to test the `MDigraph` class.
 *  run-main scalation.graphalytics.MDigraphTest
 */
object MDigraphTest extends App
{
    val g = MDigraph (Array (SET (4, 5),                // 0
                             SET (5),                   // 1
                             SET (6, 7),                // 2
                             SET (7, 8),                // 3
                             SET (0, 5, 9),             // 4
                             SET (0, 1, 4, 6, 10),      // 5
                             SET (2, 5, 7, 10, 11),     // 6
                             SET (2, 3, 6, 8),          // 7
                             SET (3, 7, 12),            // 8
                             SET (4),                   // 9
                             SET (5, 6),                // 10
                             SET (),                    // 11
                             SET (8)))                  // 12
    g.print ()

} // MDigraphTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDigraphTest2` object is used to test the `MDigraph` class.
 */
object MDigraphTest2 extends App
{
//    val g = GraphGen.genRandomGraph (10, 5, 2)
//    g.print ()

} // MDigraphTest2

