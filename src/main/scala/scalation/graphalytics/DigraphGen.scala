
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.2
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.Queue
import collection.mutable.{Set => SET}
import math.pow
import scala.util.Random

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphGen` object is used to build random graph with various
 *  characteristics.
 */
object DigraphGen
{
    /** Random number generator
     */
    private val rand = new Random

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex is uniformly distributed.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random graph with the specified size (number of vertices), 
     *  average degree and labels evenly distributed across vertices from 0 to
     *  nLabels - 1.  Not necessarily a connected graph.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     */
    def genRandomGraph (size: Int, nLabels: Int, avDegree: Int): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                          // each vertex i
            val degree = rand.nextInt (avDegree * 2 + 1)                  // out degree
            for (j <- 0 until degree if ! (ch contains j)) ch(i) += j   // add the edge i -> j
        } // for

        val label = randDistLabels (size, nLabels)
        Digraph (ch, label)
    } // genRandomGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random connected graph by using genRandomGraph and
     *  checking whether it is connected.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     */
    def genRandomConnectedGraph (size: Int, nLabels: Int, avDegree: Int): Digraph =
    {
        var g = genRandomGraph (size, nLabels, avDegree)
        while (! g.isConnected) g = genRandomGraph (size, nLabels, avDegree)
        g
    } // genRandomConnectedGraph
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random graph with labels distributed based on a power law
     *  distribution (currently with the magic number 2.1 for the power law exponent).
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed according to power law)
     *  @param avDegree  the average degree
     */
    def genRandomGraph_PowLabels (size: Int, nLabels: Int, avDegree: Int): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                          // each vertex i
            val degree = rand.nextInt (avDegree * 2 + 1)                  // out degree
            for (j <- 0 until degree if ! (ch contains j)) ch(i) += j   // add the edge i -> j
        } // for

        // 2.1 is used in WWW graph pg 72 of m&m graph data
        val label = powDistLabels (size, nLabels, 2.1)
        Digraph (ch, label)
    } // genRandomGraph_PowLabels

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex follows a power law distribution.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a graph with power law degree distribution with exponent 'distPow'
     *  and uniformly distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed uniformly)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     */
    def genPowerLawGraph (size: Int, nLabels: Int, maxDegree: Int, distPow: Double): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                          // each vertex i
            val degree = powInt (0, maxDegree, distPow)                   // out degree
            for (j <- 0 until degree if ! (ch contains j)) ch(i) += j   // add the edge i -> j
        } // for

        val label = randDistLabels (size, nLabels)
        Digraph (ch, label)
    } // genPowerLawGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a graph with power law degree distribution with exponent 'distPow'
     *  and power law distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed according to power law)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     */
    def genPowerLawGraph_PowLabels (size: Int, nLabels: Int, maxDegree: Int, distPow: Double): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                          // each vertex i
            val degree = powInt (0, maxDegree, distPow)                   // out degree
            for (j <- 0 until degree if ! (ch contains j)) ch(i) += j   // add the edge i -> j
        } // for

        val label = powDistLabels (size, nLabels, distPow)
        Digraph (ch, label)
    } // genPowerLawGraph_PowLabels

    //------------------------------------------------------------------------
    // Methods for generating/extracting query graphs from data graphs.
    // Ensures that matches will exist. 
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a graph 'g', performs a breadth first search starting at a random vertex
     *  until the breadth first tree contains 'size' vertices.  At each junction,
     *  it chooses a random number of children to traverse, with that random
     *  number averaging to 'avDegree'.
     *  @param size      the number of vertices to extract
     *  @param avDegree  the average out degree
     *  @param g         the data graph to extract from
     */
    def genBFSQuery (size: Int, avDegree: Int, g: Digraph): Digraph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var cycle       = false
        var nodes       = SET [Int] ()
        var chMap: Map [Int, SET [Int]] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            chMap     = Map [Int, SET [Int]] ()
            nodes      = SET [Int] ()
            val q      = Queue [Int] ()
            val start  = rand.nextInt (g.size)     // randomly pick a start node in ch 
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                var chs = SET [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.ch (newNode)
                if (! newNodeChildren.isEmpty) {
                    val nncArr = newNodeChildren.toArray
                    for (i <- 0 until rand.nextInt (avDegree * 2 + 1) if nodes.size < size) {
                        val newChild = nncArr (rand.nextInt (newNodeChildren.size)) 
                        if (!nodes.contains(newChild)) { nodes += newChild; q.enqueue (newChild) }
                        else cycle = true
                        if (newChild != newNode) chs += newChild 
                    } // for
                    chMap += (newNode -> (chMap.getOrElse (newNode, SET [Int] ()) ++ chs))
                } // if
            } // while

            if(nodes.size < size) nRestarts += 1
        } // while
    
        if (nRestarts == maxRestarts) { println ("genBFSQuery: could not find a good query"); return null }
    
        // gives the nodes new ids (FIX: refactor to renumber)
        var newLabelMap = Map [Int, Int] () 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val ch = Array.ofDim [SET [Int]] (size).map (x => SET [Int] ())
        for ((node, children) <- chMap) ch (newLabelMap (node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        if (cycle) println ("genBFSQuery: query has a cycle")
        Digraph (ch, label)
    } // genBFSQuery

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extracts a subgraph of 'size' vertices from graph 'g' by performing a
     *  breadth-first search from a random vertex.
     *  @param size  the number of vertices to extract
     *  @param g     the data graph to extract from
     */
    def extractSubgraph (size: Int, g: Digraph): Digraph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var chMap: Map [Int, SET [Int]] = null
        var nodes:  SET [Int] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            chMap    = Map [Int, SET [Int]] ()
            nodes     = SET [Int] ()
            val q     = Queue [Int] ()
            val start = rand.nextInt (g.size)         // randomly pick a start node in ch
            println ("extractSubgraph: start node: " + start)
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                var chs = SET [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.ch (newNode)
                if (! newNodeChildren.isEmpty) {
                    for (newChild <- newNodeChildren if nodes.size < size) {
                        if (! nodes.contains (newChild)) { nodes += newChild; q.enqueue (newChild) }
                    } // for
                } // if
            } // while

            for (n <- nodes) { val chs = g.ch(n) intersect nodes; chMap += (n -> chs ) }
            if (nodes.size < size) {
                nRestarts += 1
                println ("nodes.size only " + nodes.size)
            } // if
        } // while

        if (nRestarts == maxRestarts) { println ("extractSubgraph: could not find a good query"); return null }

        // gives the nodes new ids (FIX: refactor to renumber)
        var newLabelMap = Map [Int, Int] () 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val ch = Array.ofDim [SET [Int]] (size).map (x => SET [Int] ())
        for ((node, children) <- chMap) ch (newLabelMap(node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        Digraph (ch, label)
    } // extractSubgraph

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Renumber the selected nodes (give them new consecutive ids).
     *  @param node
     *  @param chMap
     *
    private def renumber (node: SET [Int], chMap: Map [Int, SET [Int]]): Array [SET [Int]] =
    {
        var oldId2newId = Map [Int, Int] ()
        var i = 0
        for (v <- node) { oldId2newId += (v -> i); i += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        for (
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        Array.ofDim [SET [Int]] (size).map (x => SET [Int] ())
    } // renumber
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a uniform distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def randDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => rand.nextInt (nLabels).asInstanceOf [TLabel])
    } // randDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a power law distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     *  @param pow      the power/exponent
     */
    private def powDistLabels (size: Int, nLabels: Int, pow: Double): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => powInt (0, nLabels, pow).asInstanceOf [TLabel])
    } // powDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a Gaussian/Normal distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def gaussianDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => gaussInt (nLabels / 2.0).asInstanceOf [TLabel])
    } // gaussianDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a random integer between min and max with a frequency determined
     *  by a power law distribution.
     *  @param min      the minimum value
     *  @param max      the maximum value
     *  @param distPow  the power distribution
     */
    private def powInt (min: Int, max: Int, distPow: Double): Int =
    {
        val exp = distPow + 1.0
        max - 1 - pow (( (pow (max, exp) - pow (min, exp)) * rand.nextDouble + pow (min, exp) ),
                       (1.0 / exp)).toInt
    } // powInt

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an integer with a probability based on a gaussian distribution
     *  centered at d
     *  FIX: may need to truncate with math.min(math.max((rand.nextGaussian()*d+d).toInt, 0), d*2).toInt
     *  @param d  the WHAT??
     */
    private def gaussInt (d: Double) = (rand.nextGaussian () * 2.0 * d).toInt

} // DigraphGen class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DigraphGenTest' object is used to test the 'DigraphGen' class
 *  for building random graphs where a vertex's degree is uniformly distributed.
 */
object DigraphGenTest extends App
{
    import DigraphGen._

    println ("DigraphGenTest: test genRandomGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomGraph (4, 100, 1)
        g.print ("g")
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("DigraphGenTest: test genRandomConnectedGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomConnectedGraph (4, 100, 1)
        g.print ("g")
    } // foreach

    println ("DigraphGenTest: test geneRandomGraph_PowLabels")
    val g1 = genRandomGraph_PowLabels (200, 50, 2)
    g1.print ("g1")
    g1.labelMap.toSeq.sortBy (_._1).foreach { println(_) }
 
} // DigraphGenTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DigraphGenTest2' object is used to test the 'DigraphGen' class
 *  for building power law graphs.
 */
object DigraphGenTest2 extends App
{
    import DigraphGen._

    println ("DigraphGenTest2: test genPowerLawGraph")
    val g2 = genPowerLawGraph (50, 10, 10, 2.1)
    g2.print ("g2")
    g2.ch.sortBy (_.size).foreach { println(_) }

    println ("DigraphGenTest2: test genPowerLawGraph_PowLabels")
    val g3 = genPowerLawGraph_PowLabels (50, 10, 10, 2.1)
    g3.print ("g3")
    g3.ch.sortBy (_.size).foreach { println(_) }
    g3.labelMap.toSeq.sortBy (_._1).foreach { println(_) }

} // DigraphGenTest2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphGenTest3` object is used to test the `DigraphGen` class
 *  for extracting query graphs from data graphs.
 */
object DigraphGenTest3 extends App
{
    import DigraphGen._

    var g = genRandomGraph (1000000, 10, 16)
    println ("done generating data graph")
    println ("g.size: " + g.size)
    println ("g.nEdges: " + g.nEdges)
    println ("DigraphGenTest3: test genBFSQuery")
    (2 until 10).foreach { i =>
        var q = genBFSQuery (25, 3, g)
        q.print ("q")
        println (q.size)
        println (q.nEdges)
        println (q.nEdges / q.size.toDouble)
    } // foreach
    println ("done")

} // DigraphGenTest3

