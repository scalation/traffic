
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LabelType` specifies (data) type for vertex and edge labels.
 */
object LabelType
{
    /** Type for label, e.g., Int, Double, String, etc. (not made generic for speed)
     */
    type TLabel = Int          // change and recompile (FIX: use Scala Macros)
//  type TLabel = Double
//  type TLabel = String

} // LabelType object

