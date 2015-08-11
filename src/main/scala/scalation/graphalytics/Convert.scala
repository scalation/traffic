
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Jul 31 13:54:40 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.immutable.{Set => SET}

import scalation.linalgebra.{MatriD, MatrixD, SparseMatrixD}

// FIX: Use MatrixI

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Convert` object is used to convert between an Adjacency Matrix
 *  representation to an Adjacency Sets representation.
 */
object Convert
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the graph from an Adjacency Matrix representation to an
     *  Adjacency Sets representation.
     *  @param mat  the Adjacency Matrix representation of a graph
     */
    def matrix2AdjacencySet (mat: MatriD): Graph =
    {
        val n = mat.dim1
        val adj = Array.ofDim [SET [Int]] (n)
        for (i <- 0 until n) {
            adj(i) = Set [Int] ()
            for (j <- 0 until n if mat(i, j) != 0.0)  adj(i) += j
        } // for
        Graph (adj)
    } // matrix2AdjacencySet

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the graph from an Adjacency Sets representation to an
     *  Adjacency Matrix representation.
     *  @param mat  the Adjacency Sets representation of a graph
     */
    def adjacencySet2Matrix (gr: Graph, sparse: Boolean = false): MatriD =
    {
        val n   = gr.adj.length
        val mat = if (sparse) new SparseMatrixD (n, n) else new MatrixD (n, n)
        for (i <- 0 until n; j <- 0 until n if gr.adj(i) contains j) mat(i, j) = 1.0
        mat
    } // adjacencySet2Matrix

} // Convert object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConvertTest` object is used to test the `Convert` object.
 */
object ConvertTest
{
    // FIX: test conversion in both directions

} // ConvertTest

