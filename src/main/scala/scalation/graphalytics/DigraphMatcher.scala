
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Digraph Pattern Matching Using Mutable Sets
 */

package scalation.graphalytics

import collection.mutable.{Set => SET}
//import collection.mutable.{HashSet => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigraphMatcher` abstract class serves as a template for implementing
 *  specific algorithms for graph pattern matching.
 *  @param g  the data graph  G(V, E, l) with vertices v in V
 *  @param q  the query graph Q(U, D, k) with vertices u in U
 */
abstract class DigraphMatcher (g: Digraph, q: Digraph)
{
    protected val qRange     = 0 until q.size     // range for query vertices
    protected val CHECK      = 1024               // check progress after this many matches
    protected val LIMIT      = 1E7                // quit after too many matches
    protected val SELF_LOOPS = false              // whether the directed graph has self-loops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a graph pattern matching algorithm to find the mappings from the
     *  query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a graph pattern matching algorithm to find subgraphs of data graph
     *  'g' that isomorphically match query graph 'q'.  These are represented
     *  by a set of single-valued bijective functions {'psi'} where each 'psi'
     *  function maps each query graph vertex 'u' to a data graph vertices 'v'.
     */
    def bijections (): SET [Array [Int]] =
    {
        throw new UnsupportedOperationException ()
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an initial array of feasible mappings 'phi' from each query
     *  vertex 'u' to the corresponding set of data graph vertices '{v}' whose
     *  label matches 'u's.
     */
    def feasibleMates (): Array [SET [Int]] =
    {
        val phi = Array.ofDim [SET [Int]] (q.size)
        for (u <- qRange) phi(u) = g.labelMap (q.label(u)).clone
        phi
//        q.label.map (u_label => g.getVerticesWithLabel (u_label).clone)
    } // feasibleMates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the mappings between a query graph vertex u and a set of data
     *  graph vertices {v}.
     *  @param phi  the set-valued mapping function
     */
    def showMappings (phi: Array [SET [Int]])
    {
        println ("query u \t--> graph {v}")
        for (i <- phi.indices) println ("u_" + i + " \t--> " + phi(i))
    } // showMappings

} // DigraphMatcher abstract class

