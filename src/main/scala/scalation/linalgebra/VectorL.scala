
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import collection.Traversable
import util.Sorting.quickSort

import math.{abs => ABS, sqrt}

import scalation.math._
import scalation.util.Error
import scalation.util.SortingL.qsort2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorL` class stores and operates on Numeric Vectors of base type `Long`.
 *  It follows the framework of VectorN [T] and is provided for performance.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D array used to store vector elements
 */
class VectorL (val dim: Int,
     protected var v:   Array [Long] = null)
      extends Traversable [Long] with PartiallyOrdered [VectorL] with Error with Serializable
{
    if (v == null) {
        v = Array.ofDim [Long] (dim)
    } else if (dim != v.length) {
        flaw ("constructor", "dimension is wrong")
    } // if

    /** Range for the storage array
     */
    private val range = 0 until dim

    /** Format String used for printing vector values (change using setFormat)
     *  Ex: "%d,\t", "%.6g,\t" or "%12.6g,\t"
     */
    private var fString = "%d,\t"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector from an array of values.
     *  @param u  the array of values
     */
    def this (u: Array [Long]) { this (u.length, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector u.
     *  @param u  the other vector
     */
    def this (u: VectorL)
    {
        this (u.dim)                               // invoke primary constructor
        for (i <- range) v(i) = u(i)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the range of all indices (0 to one less than dim).
     */
    def indices: Range = 0 until dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of this vector by 'more' elements.
     *  @param more  the number of new elements to add
     */
    def expand (more: Int = dim): VectorL =
    {
        if (more < 1) this       // no change
        else          new VectorL (dim + more, Array.concat (v, new Array [Long] (more)))
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): VectorL =
    {
        val c = new VectorL (size)
        c.v(j) = 1l
        c
    } // oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): VectorL =
    {
        val c = new VectorL (size)
        c.v(j) = -1l
        c
    } // _oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a VectorL into a VectorD.
     *  @param u  the vector to convert
     */
    def toDouble: VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c(i) = v(i).toDouble
        c
    } // toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's element at the i-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): Long = v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): VectorL = slice (r.start, r.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's entire array.
     */
    def apply (): Array [Long] = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's element at the i-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Long) { v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Long) { for (i <- r) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectorL) { for (i <- r) v(i) = u(i - r.start) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in this vector to x.
     *  @param x  the value to be assigned
     */
    def set (x: Long) { for (i <- range) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in this vector to the values in array u.
     *  @param u  the array of values to be assigned
     */
    def setAll (u: Array [Long]) { for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over this vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Long => U)
    {
        var i = 0    
        while (i < dim) { f (v(i)); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of this vector by applying the mapping function f.
     *  @param f  the function to apply
     */
    def map (f: Long => Long): VectorL = new VectorL (this ().map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this vector from to end.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): VectorL = new VectorL (till - from, v.slice (from, till))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of this vector corresponding to a basis.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): VectorL =
    {
        val c = new VectorL (basis.length)
        for (i <- c.range) c.v(i) = v(basis(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and vector b.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectorL): VectorL =
    {
        val c = new VectorL (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b.v(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and scalar s.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: Long): VectorL =
    {
        val c = new VectorL (dim + 1)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else s
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and vector b.
     *  @param b  the vector to add
     */
    def + (b: VectorL): VectorL = 
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) + b.v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: Long): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) + s
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar s._1 only at position s._2.
     *  @param s  the (scalar, position) to add
     */
    def + (s: Tuple2 [Long, Int]): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) + s._1 else v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def += (b: VectorL): VectorL = { for (i <- range) v(i) += b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def += (s: Long): VectorL = { for (i <- range) v(i) += s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of this vector (unary minus).
     */
    def unary_- (): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = -v(i)
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract vector b.
     *  @param b  the vector to subtract
     */
    def - (b: VectorL): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) - b.v(i)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar s.
     *  @param s  the scalar to subtract
     */
    def - (s: Long): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) - s
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar s._1 only at position s._2.
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: Tuple2 [Long, Int]): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) - s._1 else v(i)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place vector b.
     *  @param b  the vector to add
     */
    def -= (b: VectorL): VectorL = { for (i <- range) v(i) -= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place scalar s.
     *  @param s  the scalar to add
     */
    def -= (s: Long): VectorL = { for (i <- range) v(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by vector b.
     *  @param b  the vector to multiply by
     */
    def * (b: VectorL): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) * b.v(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: Long): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) * s
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this 'row' vector by matrix m.
     *  @param m  the matrix to multiply by
     */
    def * (m: MatriL): VectorL = m.t * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def *= (b: VectorL): VectorL = { for (i <- range) v(i) *= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def *= (s: Long): VectorL = { for (i <- range) v(i) *= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by vector b (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectorL): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) / b.v(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by scalar s.
     *  @param s  the scalar to divide by
     */
    def / (s: Long): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) / s
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def /= (b: VectorL): VectorL = { for (i <- range) v(i) /= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def /= (s: Long): VectorL = { for (i <- range) v(i) /= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of this vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Long): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = v(i) ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of this vector to the s-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Long) { for (i <- range) v(i) = v(i) ~^ s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the square of each element of this vector.
     */
    def sq: VectorL = this * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of this vector.
     */
    def abs: VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = ABS (v(i))
        c
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector.
     */
    def sum: Long = v.foldLeft (0l) (_ + _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector skipping the i-th element.
     *  @param i  the index of the element to skip
     */
    def sum_ne (i: Int): Long = sum - v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of this vector.
     */
    def sum_pos: Long =
    {
        var sum: Long = 0l
        for (i <- range if v(i) > 0l) sum += v(i)
        sum
    } // sum_pos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute mean the elements of this vector.
     */
    def mean: Long = sum / v.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of this vector from left to right (e.g., create a
     *  cdf from a pmf). Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: VectorL =
    {
        val c = new VectorL (dim)
        var sum: Long = 0l
        for (i <- range) { sum += v(i); c.v(i) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so that it sums to one (like a probability vector).
     */
    def normalize: VectorL = this * (1l / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so its length is one (unit vector).
     */
    def normalizeU: VectorL = this * (1l / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector to have a maximum of one.
     */
    def normalize1: VectorL = this * (1l / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of this vector with vector b.
     *  @param b  the other vector
     */
    def dot (b: VectorL): Long =
    {
        var sum: Long = 0l
        for (i <- range) sum += v(i) * b.v(i)
        sum
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) squared of this vector.
     */
    def normSq: Long = this dot this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) of this vector.
     */
    def norm: Long = sqrt (normSq).toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of this vector.
     */
    def norm1: Long =
    {
        var sum: Long = 0l
        for (i <- range) sum += ABS (v(i))
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Long =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) > x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectorL): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = if (b.v(i) > v(i)) b.v(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): Long =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) < x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectorL): VectorL =
    {
        val c = new VectorL (dim)
        for (i <- range) c.v(i) = if (b.v(i) < v(i)) b.v(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the element with the greatest magnitude in this vector.
     */
    def mag: Long = ABS (max ()) max ABS (min ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of this vector (index of maximum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (e: Int = dim): Int =
    {
        var j = 0
        for (i <- 1 until e if v(i) > v(j)) j = i
        j
    } // argmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of this vector (index of minimum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (e: Int = dim): Int =
    {
        var j = 0
        for (i <- 1 until e if v(i) < v(j)) j = i
        j
    } // argmin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument minimum of this vector (-1 if its not negative).
     *  @param e  the ending index (exclusive) for the search
     */
    def argminNeg (e: Int = dim): Int =
    {
        val j = argmin (e); if (v(j) < 0l) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of this vector (-1 if its not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int =
    {
        val j = argmax (e); if (v(j) > 0l) j else -1
    } // argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in this vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) < 0l) return i; -1
    } // firstNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in this vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) > 0l) return i; -1
    } // firstPos

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative entries in this vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) < 0l) count += 1
        count
    } // countNeg

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive entries in this vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) > 0l) count += 1
        count
    } // countPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the predicate pred holds for some element in this vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
//  def exists (pred: (Long) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether x is contained in this vector.
     *  @param x  the element to be checked
     */
    def contains (x: Long): Boolean = v contains x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort this vector in-place in ascending (non-decreasing) order.
     */
    def sort () { quickSort (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort this vector in-place in descending (non-increasing) order.
     */
    def sort2 () { qsort2 (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in this vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int)
    {
        val t = v(j); v(j) = v(i); v(i) = t
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector is at least as long as this vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: VectorL): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range if v(i) < 0l) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this vector with vector b.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: VectorL] (b: B)
        (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [VectorL] (i)
            if      (ge && (v(i) compare b_i) < 0) ge = false
            else if (le && (v(i) compare b_i) > 0) le = false
        } // for
        if (ge && le) Some (0) else if (le) Some (-1) else if (ge) Some (1) else None
    } // tryCompareTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether vector this equals vector b.
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean =
    {
        b.isInstanceOf [VectorL] && (v.deep equals b.asInstanceOf [VectorL].v.deep)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode to be be compatible with equals.
     */
    override def hashCode: Int = v.deep.hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the newFormat (e.g., "%.6g,\t" or "%12.6g,\t").
     *  @param  newFormat  the new format String
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this vector to a String.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("VectorL(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (v(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString
  
} // VectorL class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorL` object is the companion object for the `VectorL` class.
 */
object VectorL
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorL from one or more values (repeated values Long*).
     *  @param x   the first Long number
     *  @param xs  the rest of the Long numbers
     */
    def apply (x: Long, xs: Long*): VectorL =
    {
        val c = new VectorL (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.dim-1) c.v(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorL from one or more values (repeated values String*).
     *  @param x   the first String
     *  @param xs  the rest of the Strings
     */
    def apply (x: String, xs: String*): VectorL =
    {
        val c = new VectorL (1 + xs.length)
        c(0)  = x.toLong
        for (i <- 0 until c.dim-1) c.v(i+1) = xs(i).toLong
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorL from an array of Strings.
     *  @param xs  the array of the Strings
     */
    def apply (xs: Array [String]): VectorL =
    {
        val c = new VectorL (xs.length)
        for (i <- c.range) c.v(i) = xs(i).toLong
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the size of the vector
     */
    def one (size: Int): VectorL =
    {
        val c = new VectorL (size)
        c.set (1l)
        c
    } // one

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part
     */
    def ++ (b: Long, u: VectorL): VectorL =
    {
        val c = new VectorL (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i - 1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a VectorL containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): VectorL =
    {
        val c = new VectorL (end - start)
        for (i <- c.range) c.v(i) = (start + i).toLong
        c
    } // range

} // VectorL object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorLTest` object tests the operations provided by `VectorL`.
 *  > run-main scalation.linalgebra.VectorLTest
 */
object VectorLTest extends App
{
    var x: VectorL = null
    var y: VectorL = null

    for (l <- 1 to 4) {
        println ("\n\tTest VectorL on vectors of dim " + l)
        x = new VectorL (l)
        y = new VectorL (l)
        x.set (2)
        y.set (3)

        // test vector op scalar
        println ("x + 4    = " + (x + 4))
        println ("x - 4    = " + (x - 4))
        println ("x * 4    = " + (x * 4))
        println ("x / 4    = " + (x / 4))
        println ("x ~^ 4   = " + (x ~^ 4))

        // test vector op vector
        println ("x + y    = " + (x + y))
        println ("x - y    = " + (x - y))
        println ("x * y    = " + (x * y))
        println ("x / y    = " + (x / y))

        println ("x.min    = " + x.min ())
        println ("x.max    = " + x.max ())
        println ("x.sum    = " + x.sum)
        println ("x.sum_ne = " + x.sum_ne (0))
        println ("x dot y  = " + (x dot y))
        println ("x.normSq = " + x.normSq)
        println ("x.norm   = " + x.norm)
        println ("x < y    = " + (x < y))
    } // for

    println ("hashCode (" + x + ") = " + x.hashCode ())
    println ("hashCode (" + y + ") = " + y.hashCode ())

    val z = VectorL ("1", "2", "3", "4")
    println ("z = " + z)

} // VectorLTest

