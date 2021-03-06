
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Arash Fard, Usman Nisar, Ayushi Jain, John Miller
 *  @version 1.2
 *  @date    Thu Nov 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import java.lang.System.nanoTime

import collection._
import collection.immutable.{Set => SET}
import collection.mutable.{ListBuffer, Map, HashMap, MutableList, Set, Stack}
import math.pow
import util.control.Breaks.{break, breakable}
import util.Random

import scalation.stat.Statistic
import scalation.util.Timer.{time, timer}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'TightSim' class provides an implementation for graph pattern matching.
 *  @see http://hipore.com/ijbd/2014/IJBD%20Vol%201%20No%201%202014.pdf
 *  @param printSetting  '0' for match and '1' for ball
 */
class TightSim (printSetting: String) 
{
    private val NS_PER_MS = 1000000.0                                         // nanosecond per milliseconds

    private val listOfDistinctReducedSet = new ListBuffer [SET [String]] ()   // contains total number of matches 
                                                                              // after post processing
    private val mapOfBallWithSize = Map [Int, Long] ()                        // contains balls left after
                                                                              // post processing with diameter.
    private val listOfMatchedBallVertices = MutableList [Int] ()              // contains list of center vertices 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs tight simulation.
     *  @param q          the query graph Q(U, D, k)
     *  @param g          the data graph  G(V, E, l)
     *  @param queryName  the query graph relative file name
     *  @param dataName   the data graph relative file name
     */
    def tightSim (q: Graph, g: Graph, queryName: String, dataName: String): Array [SET [Int]] = 
    {
        val dataSize  = g.adj.length                                      // size of the data graph
        val querySize = q.adj.length                                      // size of the query graph
        val qmet      = new GraphMetrics (q.clone, false)                 // creating graph metrics object of query graph
        val (sim, dualTime) = timer { new DualSim (g, q).mappings }       // performs dual simulation
        println ("DUAL RESULT SIZE: "+ sim.flatten.toSet.size)            // to print dual simulation result
        if (sim.size == 0) { println ("No dual match."); return null }    // exit if no match after dual simulation

        val (newGraph, gPruningTime) = timer { filterGraph (g, q, sim) }  // if doing strong sim more than once, must clone g
        val prunedSize = sim.flatten.toSet.size                           // size of feasible matches after strict simulation
        val (qDiameter, diameterTime) = timer { qmet.rad }                // get the query diameter
        val balls        = HashMap [Int, Ball] ()                    
        val matchCenters = Set [Int] ()
        var ballTime     = 0.0
        var filterTime   = 0.0
        var ballSum      = 0

        for (center <- sim (selectivityCriteria (qmet))) {                // picking center based on selectivity criteria
            var t0    = nanoTime ()
            val ball  = new Ball (newGraph, center, qDiameter)            // creating a new ball for the selected center vertex
            ballTime += (nanoTime () - t0) / NS_PER_MS                    // time elapsed for creating a ball
            ballSum  += ball.nodesInBall.size                             // calculate ball size

            t0          = nanoTime ()
            val mat     = dualFilter (q, sim.clone, ball)               // perform dual filter on the ball
            filterTime += (nanoTime () - t0) / NS_PER_MS                  // time elapsed for dual filter
            balls.put (center, ball)

            if (mat.size != 0) matchCenters += center
            else println ("No match for ball centered at " + center + "\n")
        } // for
        
        performPostProcessing (g, balls, matchCenters)                    // post processing after all the matches obtained
        println ("SEQUENTIAL:    Data Graph Name:  " + dataName +
                 "\n  Number of Data Graph Nodes:  " + dataSize +
                 "\n            Query Graph Name:  " + queryName +
                 "\n Number of Query Graph Nodes:  " + querySize +
                 "\n     Number of Tight Matches:  " + matchCenters.size +
                 "\nDualSim Time for Whole Graph:  " + dualTime + " ms" +
                 "\n          Graph Pruning Time:  " + gPruningTime + " ms" +
                 "\n    Graph Size after Pruning:  " + prunedSize + " nodes" +
                 "\n Finding Query Diameter Time:  " + diameterTime + " ms" +
                 "\n              Query Diameter:  " + qDiameter +
                 "\n          Ball Creation Time:  " + ballTime +
                 "\n           Average Ball Size:  " + (ballSum / prunedSize.toDouble) +
                 "\n                 Filter Time:  " + filterTime + 
                 "\nCreating and Filtering Balls:  " + (ballTime + filterTime) + " ms" +
                 "\n        Total Distinct Edges:  " + calculateTotalEdges (g, balls, matchCenters) +
                 "\n     Total Distinct Vertices:  " + calculateTotalVertices ())
        println ("Ball Diameter Metrics(Min, Max, Mean, StdDev): " + calculateBallDiameterMetrics (balls) )
        sim
    } // tightSim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prune the data graph by consider only those vertices and edges which
     *  are part of feasible matches after performing initial dual simulation.
     *  @param g    the data graph  G(V, E, l)
     *  @param q    the query graph Q(U, D, k)
     *  @param sim  mappings from a query vertex u_q to { graph vertices v_g }
     */ 
    def filterGraph (g: Graph, q: Graph,  sim: Array [SET [Int]]): Graph = 
    {
        val nodesInSimset = sim.flatten.toSet               // get all the vertices of feasible matches
        for (i <- 0 until g.adj.size) {                     // pruning all vertices which are not part of feasible matches
            g.adj(i) &= nodesInSimset                
            g.par(i) &= nodesInSimset
        } // for
        var newAdjSet  = Array.ofDim [SET [Int]] (g.adj.size)
        var newParList = Array.ofDim [SET [Int]] (g.par.size)

        for (i <- 0 until newAdjSet.size) {
            newAdjSet(i)  = SET [Int] ()
            newParList(i) = SET [Int] ()
        } // for

        for (u <- 0 until q.size; w <- sim (u)) {           // preparing new adj and parent set for data graph based upon feasible vertices
            for (v <- q.adj(u)) newAdjSet (w) |= (g.adj(w) & sim(v))
            for (v <- q.par(u)) newParList(w) |= (g.par(w) & sim(v))
        } // for
        Graph (newAdjSet, g.label, g.inverse)             // creating a new data graph
    } // filterGraph
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform dual simulation onto the ball.
     *  @param query  the query graph Q(U, D, k)
     *  @param sim    mappings from a query vertex u_q to { graph vertices v_g }
     *  @param ball   the Ball B(Graph, Center, Radius)
     */ 
    def dualFilter (query: Graph, sim: Array [SET [Int]], ball: Ball): Array [SET [Int]] = 
    {
        for (v <- sim.indices) sim(v) &= ball.nodesInBall            // project simset onto ball
        val filterSet = new Stack [(Int, Int)] ()
        var filtered  = false
        for (u <- sim.indices; v <- sim(u) if ball.borderNodes contains v) {
            filtered = false                                         // filtering ball based on child relationship
            breakable { for (u1 <- query.adj (u)) { 
                if ((ball.post (v) & sim (u1)).isEmpty) {
                    filterSet.push ((u, v))
                    filtered = true
                    break
                } // if
            }} //  breakable for
            if (! filtered) {                                        // filtering ball based on parent relationship,
              breakable { for (u2 <- query.par (u)) {                //  if no child has been filtered out
                   if ((ball.pre (v) & sim (u2)).isEmpty) {
                       filterSet.push ((u, v))
                       break
                   } // if
                }} // breakable for
            } // if
        } // for

        while (! filterSet.isEmpty) {                                // refine child and parent relationship for the vertex v,  
            val (u, v) = filterSet.pop ()                            // which is now not a feasible match  
            sim (u) -= v
            for (u2 <- query.par (u); v2 <- (ball.pre (v) & sim (u2)) if (ball.post (v2) & sim (u)).isEmpty) 
                filterSet.push ((u2, v2))
            for (u1 <- query.adj (u); v1 <- (ball.post (v) & sim (u1)) if (ball.pre (v1) & sim (u)).isEmpty)
                filterSet.push ((u1, v1))
        } // while

        val adjSet  = HashMap [Int, Set [Int]] ()
        val parList = HashMap [Int, Set [Int]] ()
        // create new adj and parent set for the ball after above pruning
        for (u <- sim.indices; v <- sim(u); uc <- query.adj(u); vc <- (ball.post (v) & sim (uc))) {                                 
            adjSet.getOrElseUpdate (v, Set [Int] ())   += vc
            parList.getOrElseUpdate (vc, Set [Int] ()) += v
        } // for

        // Finding max perfect subgraph
        val stack = new Stack [Int] ()
        val visited = Set (ball.ballcenter)
        stack.push (ball.ballcenter)
        while (! stack.isEmpty) {
            val v = stack.pop ()
            for (child <- (adjSet.getOrElse (v, Set ()) | parList.getOrElse (v, Set ()))) {
                if (! visited.contains (child)) {
                    stack.push (child)
                    visited += child
                } // if
            } // for
        } // while
        for ( v <- sim.indices) sim (v) = sim(v) & visited

        //fixes the edges in the ball
        //(note that it does not change the parent set; this is only used for printing)
        //uncomment if you want to see the ball after finding maximum perfect subgraph

        if (printSetting.trim == "0") {
            ball.adjSet = Map [Int, Set [Int]] ()
            val matchNodes = sim.flatten.toSet
            for ((n, nset) <- adjSet; nc <- nset) {
                if ((matchNodes contains n) && (matchNodes contains nc)) ball.adjSet.getOrElseUpdate (n, Set () ) += nc
            } // for
        } // if
  
        for ( v <- sim.indices if sim(v).isEmpty) return Array [SET [Int]] ()
        sim
    } //dualFilter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the dual sim match set.
     *  @param sim  mappings from a query vertex u_q to { graph vertices v_g }
     */
    def printDualSimMatch (sim: HashMap [Int, Set [Int] ] ) 
    {
        println("dualSim match:")
        for ( (u, v) <- sim) println (u + "  " + v)
        println ("-------------")
    } // printDualSimMatch
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform post processing after computing all the matches.
     *  @param g             the data graph  G(V, E, l)
     *  @param balls         mappings from a center vertex to the Ball B(Graph, Center, Radius)
     *  @param matchCenters  set of all vertices which are considered as center
     */
    def performPostProcessing (g: Graph, balls: HashMap [Int, Ball], matchCenters: Set [Int]) 
    {
        var postProcessingTime = 0l
        var t0                 = 0l

        for (vert_id <- 0 until g.adj.length) {                            // check for every vertex of a data graph
            var ballstring = ""
            var isMatch    = 0
            if (balls.keySet contains vert_id) {                           // if its a center vertex, then need to perform post processing
                ballstring = balls.get (vert_id).get.getBallAsString ()    // get the ball as a string
                t0 = nanoTime ()
                                                                           // get all the nodes in sorted order in string format
                val str = SET (ballstring.replaceAll ("[\\[\\]\\-\\>,]*", " ").replaceAll ("  ", ",").replaceAll (" ","").split (","):_*)
                if (checkInsertOfMatch (str)) {                            // check whether its already contained in the result
                    if (matchCenters contains vert_id) {
                        isMatch = 1
                        listOfMatchedBallVertices += vert_id
                    } // if
                } else {                                                   // remove the entry from matchCenters
                    ballstring = ""
                    matchCenters -= vert_id
                } // if
                postProcessingTime += nanoTime () - t0
            } // if
            println (vert_id + "\t " + g.label (vert_id) + "\t" + ballstring)
        } // for
        println ("Post Processing time is: " + (postProcessingTime / NS_PER_MS) + " ms")
    } // performPostProcessing

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether a ball is subset or superset of any other ball and add its
     *  entry accordingly in listofDistinctReduceSet and return true, if added in
     *  the list else false.
     *  @param str  set of vertices of a ball in string format
     */
    def checkInsertOfMatch (str: SET [String]): Boolean =
    {
        var isInsert = true
        if (str == null || str.isEmpty || str.size == 0) isInsert = false
        else {
            if (listOfDistinctReducedSet.isEmpty) listOfDistinctReducedSet += str
            else {
                breakable { for (i <- 0 until listOfDistinctReducedSet.size) {
                    if (str subsetOf listOfDistinctReducedSet(i)) {           // str is a subset
                        listOfDistinctReducedSet(i) = str                     // update listOfDistinctReducedSet with str
                        isInsert = false
                    } else if (listOfDistinctReducedSet(i) subsetOf str) {    // str is a superset
                        isInsert = false                  // listOfDistinctReducedSet already contain subset of str, so str not needed
                        break
                    } // if
                }} // breakable for
                if (isInsert) listOfDistinctReducedSet += str             // if str is not in listOfDistinctReducedSet, add it
            } // if
        } // if
        isInsert
    } // checkInsertOfMatch
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count distinct vertices left after post processing.
     */
    def calculateTotalVertices (): Int = 
    {
        val totalSet = Set [String] ()
        for (i <- 0 until listOfDistinctReducedSet.length) totalSet ++= listOfDistinctReducedSet(i)
        totalSet.size
    } // calculateTotalVertices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count distinct edges left after post processing.
     *  @param g               the data graph  G(V, E, l)
     *  @param balls           mappings from a center vertex to the Ball B(Graph, Center, Radius)
     *  @param matchCenters    set of all vertices which are considered as center
     */
    def calculateTotalEdges (g: Graph, balls: HashMap [Int, Ball], matchCenters: Set [Int]): Int = 
    {
        val distinctEdges = Set [String] ()
        for (vert_id <- 0 until g.adj.length; if balls.keySet.contains (vert_id)) { 
            balls.get (vert_id).get.adjSet.foreach (i => i._2.foreach (j => distinctEdges += (i._1.toString + "_" + j.toString)))
        } // for
        distinctEdges.size
    } // calculateTotalEdges

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate statistics (e.g., min, max, avg diameter and standard deviation)
     *  on the  balls left after postprocessing.
     *  @param balls  mappings from a center vertex to the Ball B(Graph, Center, Radius)
     */
    def calculateBallDiameterMetrics (balls: HashMap [Int, Ball]): Statistic =
    {
        val t0 = nanoTime ()
        val ballStats = new Statistic ()
        for (vert_id <- listOfMatchedBallVertices) ballStats.tally (balls.get (vert_id).get.getBallDiameter)
        ballStats
    } // calculateBallDiameterMetrics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vertex from an array of central vertices, those which have 
     *  highest adj set size and lowest frequency of label in the query graph, i.e.
     *  highest ratio.
     *  @param centr the array of vertices whose eccentricity is equal to the radius
     */
    def selectivityCriteria (qmet: GraphMetrics): Int =
    {
        var index = 0
        var max   = 0.0
        for (ctr <- qmet.central) {
            val ratio = qmet.g.adj(ctr).size.toDouble / qmet.g.labelMap (qmet.g.label(ctr)).size.toDouble
            if (max < ratio) { max = ratio; index = ctr }
        } // for
        index
    } // selectivityCriteria 

} // TightSim


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightSimTest` object test the `TightSim` class by passing
 *  data graph, query graph absolute file path, print match/ball (0/1)
 */
object TightSimTest extends App 
{
    if (args.length != 3) {
         println ("args must be in the format:  <data_graph> <query_graph> <print match/ball (0/1)> ")
    } else {
        val gfile = args (0)
        val qfile = args (1)
        val printSetting = args (2)
        val g = Graph.apply (gfile, true)
        val q = Graph.apply (qfile, true)
        time { new TightSim (printSetting).tightSim (q, g, qfile, gfile) }
    } // if

} // TightSimTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `TightSimTest2` object test the `TightSim` class by feeding
 *  data graph, query graph absolute file path, print match/ball (0/1).
 */
object TightSimTest2 extends App 
{
    val gfile = "/home/ayushi/experiment/check/todayData2"
    val qfile = "/home/ayushi/experiment/check/todayQuery2"
//  val gfile = "" // Data Graph File Path
//  val qfile = "" // Query Graph file path
    val printSetting = "0"
    val g = Graph.apply(gfile, true)
    val q = Graph.apply(qfile, true) 
    time { new TightSim (printSetting).tightSim (q, g, qfile, gfile) }

} // TightSimTest2

