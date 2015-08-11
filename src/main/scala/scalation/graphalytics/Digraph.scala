
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Digraph Data Structure Using Mutable Sets
 */

package scalation.graphalytics

import collection.mutable.Map
import collection.mutable.{Set => SET}
//import collection.mutable.{HashSet => SET}
import scalation.util.Error

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Digraph` class stores vertex-labeled directed graphs using an adjacency
 *  set ('ch') representation, e.g., ch = { {1, 2}, {0}, {1} } means that the
 *  graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'pa' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
 *  @param label    the array of verter labels
 *  @param inverse  whether to store inverse adjacency sets (parents)
 */
class Digraph (val ch:      Array [SET [Int]],
               val label:   Array [TLabel] = Array.ofDim (0),
                   inverse: Boolean = false)
      extends Cloneable with Error
{
    /** the map from label to the set of vertices with the label
     */
    val labelMap = buildLabelMap (label)

    /** The optional array of vertex inverse (parent) adjacency sets (incoming edges)
     */
    val pa = Array.ofDim [SET [Int]] (ch.size)     // by default, don't use 'pa'

    if (inverse) addPa ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of this graph.
     */
    override def clone: Digraph = Digraph (ch.clone, label.clone, inverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the inverse adjacency sets for rapid accesses to parent vertices.
     */
    def addPa ()
    {
        for (j <- pa.indices) pa(j) = SET [Int] ()
        for (i <- ch.indices; j <- ch(i)) pa(j) += i
    } // addPa

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the number of vertices in the graph.
     */
    def size = ch.size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the number of edges in the graph.
     */
    def nEdges = ch.foldLeft (0) { (n, i) => n + i.size }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an array of labels, returns an index from labels to the sets of
     *  vertices containing those labels.
     *  @param label  the array of vertex labels of type TLabel
     */
    def buildLabelMap (label: Array [TLabel]): Map [TLabel, SET [Int]] =
    {
        val labelMap = Map [TLabel, SET [Int]] ()
        for (i <- label.indices) {                      // for each vertex i
            val lab  = label(i)                         // label for vertex i
            labelMap += lab -> (labelMap.getOrElse (lab, SET ()) + i)
        } // for
//      label.foldLeft(0) ( (i, lab) => {
//          labelMap = labelMap + (lab -> (labelMap.getOrElse (lab, SET ()) + i))
//          i + 1
//      })  // foldLeft
        labelMap
    } // buildLabelMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the maximum label value.
     */ 
    def nLabels = labelMap.keys.max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determines the number of vertices in the graph that have outgoing edges
     *  to themselves.
     */ 
    def nSelfLoops: Int =
    {
        ch.indices.foldLeft (0) { (sum, i) => if (ch(i) contains i) sum + 1 else sum }
    } // nSelfLoops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determines whether or not the graph is connected.
     */
    def isConnected: Boolean =
    {
        var connectedNodes = SET [Int] ()
        ch.foldLeft(0) { (i, set) =>
            if (! set.isEmpty) connectedNodes += i
            connectedNodes ++= set
            i + 1
        } // foldLeft
        connectedNodes.size == size
    } // isConnected

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the endpoint vertex id of each edge is within bounds: 0..maxId.
     */
    def checkEdges: Boolean =
    {
        val maxId = ch.size - 1
        for (u <- ch.indices; u_c <- ch(u) if u_c < 0 || u_c > maxId) {
            flaw ("checkEdges", s"child of $u, with vertex id $u_c not in bounds 0..$maxId")
            return false
        } // for
        true
    } // checkEdges

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the set of vertices in the graph with label l.
     */
//  def getVerticesWithLabel (l: Int) = labelMap.getOrElse (l, SET [Int] ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate basic information about this graph.  Due to its potential size,
     *  use print to show graph details.
     */
    override def toString: String = "Digraph with " + size + " vertices"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prints the adjacency list and labelMap of the graph.
     *  @param name  the name of the graph
     */
    def print (name: String = "g")
    {
        println ("Digraph " + name + " ---------------------------------------")
        println ("ch: "); ch.foldLeft (0) { (i, u) => { println (i + " -> " + u); i+1 } }
        println ("labelMap: "); labelMap.foreach { case (k, v) => println (k + " -> " + v) }
    } // print

} // Digraph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Digraph` companion object provides build methods for directed graphs.
 */
object Digraph
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a directed graph.
     *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
     *  @param label    the array of verter labels
     *  @param inverse  whether to store inverse adjacency sets (parents)
     */
    def apply (ch:      Array [SET [Int]],
               label:   Array [TLabel] = Array.ofDim (0),
               inverse: Boolean = false) =
    {
        new Digraph (ch, label, inverse)
    } // apply

} // Digraph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphTest` object is used to test the `Digraph` class.
 *  run-main scalation.graphalytics.DigraphTest
 */
object DigraphTest extends App
{
    val g = Digraph (Array (SET (4, 5),                // 0
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

} // DigraphTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphTest2` object is used to test the `Digraph` class.
 */
object DigraphTest2 extends App
{
//    val g = GraphGen.genRandomGraph (10, 5, 2)
//    g.print ()

} // DigraphTest2

