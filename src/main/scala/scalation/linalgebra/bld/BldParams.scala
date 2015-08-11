
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat May 30 13:51:12 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldParams` trait defines common values to be used in code generation.
 */
trait BldParams
{
    val _l   = java.io.File.separator
    val DIR  = s"src${_l}main${_l}scala${_l}scalation${_l}linalgebra${_l}bld"

//                      VECTOR     BASE        VECTOR2    BASE2     FORMAT  MATRI    SORTING     ZERO   ONE
    val kind = Array (("VectorI", "Int",      "VectorD", "Double", "%d",   "MatriI", "SortingI", "0",   "1"),
                      ("VectorL", "Long",     "VectorD", "Double", "%d",   "MatriL", "SortingL", "0l",  "1l"),
                      ("VectorD", "Double",   "VectorI", "Int",    "%g",   "MatriD", "SortingD", "0.0", "1.0"),
                      ("VectorR", "Rational", "VectorD", "Double", "%s",   "MatriR", "SortingR", "_0",  "_1"),
                      ("VectorC", "Complex",  "VectorD", "Double", "%s",   "MatriC", "SortingC", "_0",  "_1"))

} // BldParams trait

