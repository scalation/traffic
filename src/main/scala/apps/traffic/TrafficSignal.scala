//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman
 *  @version 1.1
 *  @date    Wed Aug 05 14:06:55 EDT 2015 
 *  @see     LICENSE (MIT style license file).
 */

package apps.traffic

import collection.mutable.ListBuffer
import java.io.PrintWriter

import scalation.animation.CommandType._
import scalation.process._
import scalation.random.Variate
import scalation.scala2d.{Ellipse, Rectangle}
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace
import util.control.Breaks.{breakable, break}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficSignal` class models the operation of a traffic signal that is
 *  either red or green.  When the signal is red, cars (SimActors) will be
 *  enqueued in WaitQueue.  When the signal is green, the cars can proceed.
 *  @param name       the name of the traffic signal
 *  @param line       the queue holding vehicles waiting for the signal to change
 *  @param units      number of units/phases of operation
 *  @param at         the location of the TrafficSignal (x, y, w, h)
 *  @param cap        the maximum number of vehicles that can proceed when the signal
 *                      changes
 *  @param _redirect  if the TrafficSignal represents a "turn light", then the 
 *                      redirect tells a vehicle where it needs to go.
 */
class TrafficSignal (name: String, line: WaitQueue, units: Int, 
            at: Array [Double], gTime: Double, cap: Int = 1000, _redirect: Component = null)
      extends Component
{    
    if (line == null) flaw ("constructor", "must have line for entities when gate is closed")

    initStats (name)
    at_= (at)

    var n2Release = cap

    var color = "red"

    var releaseCount = 0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The setColor method sets the color and, if the color
     *  has changed from red to green, calls the release method
     *  to release SimActors from line.
     *  @param c  the new color of the gate
     */
    def setColor (c: String) 
    {
        if (c == "green" && color == "red") {
            color = c
            release (gTime)
        } else color = c
    } // setColor
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the gate is shut (e.g., traffic light is red).
     */
    def shut: Boolean = if (color == "red") true else false
    
    def redirect = _redirect
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Gate.
     */
    def display ()
    {
        director.animate (this, CreateNode, signalColor, Ellipse (), at)
        director.animate (line, CreateNode, cyan, Rectangle (), line.at)
    } // display
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The release method dequeues SimActors from the line.
     *  It releases them one at a time with a half-second delay 
     *  between SimActors.
     */
    def release (durat: Double) 
    { 
        releaseCount += 1

        var n = (durat / 3000.0).toInt
        for (i <- 0 until n if ! line.isEmpty) {
            val waitingActor = line.dequeue ()
            trace (this, " / " + name + " releases", waitingActor, director.clock)            
            tally (5900.0)
            waitingActor.schedule (i * 1000.0 + 4900)
        }
    }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The gateColor method returns the actual color of the light
     *  for display purposes.
     */
    def signalColor: Color = if (shut) red else green

} // TrafficSignal


/*
object TrafficSignalTest extends App
{
    

    class TrafficSignalModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name, animating = false)
    {
        val src2 = new Source ("source_2", this, Car, 1, nArrivals, iArrivalRV, (490.0, 280.0))
        val snk2 = new Sink ("sink_2", (890.0, 280.0))    
        val wqu2 = new WaitQueue ("waitQ_2", Array (630.0, 280.0, 20.0, 20.0), 1000)
        val gat2 = new Gate2 ("gate_2", wqu2, 100, Array (705.0, 285.0, 10.0, 10.0), time (0))
        val in2  = new Transport ("in_2",  src2, wqu2, moveRV)
        val out2 = new Transport ("out_2", gat2, snk2, moveRV)    
*/











