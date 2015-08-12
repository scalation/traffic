
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author   Casey Bowman
 *  @version  1.1
 *  @date     Wed Aug 05 14:20:13 EDT 2015 
 *  @see      LICENSE (MIT style license file).
 */

package apps.traffic

import collection.mutable.ListBuffer

import scalation.process._
import scalation.animation.CommandType._
import scalation.random._
import scalation.scala2d.{Ellipse, Rectangle}
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace
import util.control.Breaks.{breakable, break}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GateController' class controls an array of Gate2s
 *  @param name      the name of this GateController  
 *  @param director  the director for the simulation
 *  @param gate      the array of Gate2s being controlled
 *  @param state     array of String arrays representing 
 *                   the states of the lights.
 *  @param time      the amount of time for each state
 *  @param at        the location of the GateController
 */
class SignalController (name: String, director: Model, gate: Array [TrafficSignal], state: Array [Array [String]], 
                      time: Array [Double], at: Array [Double] = Array (0.0, 0.0, 0.0, 0.0))
      extends SimActor (name, director) with Component 
{
    // each state should have a String for each individual gate
    if (gate.length != state(0).length) flaw ("constructor", "there must be the same number of gates as states.")

    // the GateController is not created by a source, so it must schedule itself to start.
    schedule (0.0)
    initStats (name)
    at_= (at)
    
    var stateCounter = 0   // a counter that will iterate through the states

    println ("hello")

    var stopped = false

//    for (g <- gate) subpart += g

    //::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Each GateController is a SimActor and must have an
     *  act method.  The method grabs the current state 
     *  and iterates through each gate, assigning the new
     *  individual states to each gate and setting the 
     *  color for each gate.  It then yields to the director.
     */
    def act ()
    {
        for (j <- 0 until 1000) {
//            println ("j = " + j)
            val curState = state(stateCounter)
            for (i <- 0 until gate.length) {
                val g = gate(i)
                g.setColor (curState(i))
                g.display ()
            }
            stateCounter += 1
            if (stateCounter == time.length) stateCounter = 0
            if (!stopped) schedule (time(stateCounter))
            yieldToDirector ()
        }
        yieldToDirector ()
    } // act
            
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The GateController is thought of as an abstract 
     *  component and therefore will not have a visualization.
     *  However, the display method is needed for all Components.
     */
    def display () {}

} // GateController         


/*            
object GateControllerTest extends App
{
    val gcm = new GateControllerModel ("gcm", 10, Sharp (500), Sharp (10000.0))
//    Coroutine.startup ()
    gcm.simulate ()
    gcm.complete ()        
    Coroutine.shutdown ()

    class GateControllerModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name, reps = 2, animating = false)
    {
        val state = Array (Array ("red"), Array("green"))

        val time  = Array (1000.0, 12000.0)

        val src = new Source    ("src", this, Car, 0, nArrivals, iArrivalRV, (100.0, 200.0))
        val snk = new Sink      ("snk", (900.0, 200.0))
        val que = new WaitQueue ("que", Array (750.0, 200.0, 20.0, 20.0), 1000)
        val gat = new Gate2     ("gat", que, 100, Array (800.0, 200.0, 20.0, 20.0), time(1))
        val tp1 = new Transport ("tp1", src, que, moveRV)
        val tp2 = new Transport ("tp2", gat, snk, moveRV)
 
        val gc  = new GateController ("gc", this, Array (gat), state, time)

        addComponent (src, snk, que, gat, tp1, tp2, gc)

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                tp1.move ()
                if (gat.shut) que.waitIn ()
//                tp2.increment ()
                tp2.move ()
//                tp2.decrement ()
                snk.leave ()
            }
        }
    }
}        

object GateControllerTest2 extends App
{
    val gcm = new GateControllerModel2 ("gcm", 300, Sharp (1000), Sharp (3000.0))
//    Coroutine.startup ()
    gcm.simulate ()
    gcm.complete ()        
    Coroutine.shutdown ()
    
    class GateControllerModel2 (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name, reps = 1)
    {
        val state = Array (Array ("red",   "red"  ), 
                           Array ("green", "red"  ),
                           Array ("green", "green"),
                           Array ("red",   "green"))

        val time  = Array (3000.0, 2000.0, 3000.0, 1000.0)

        val src = new Source    ("src", this, Car, 0, nArrivals, iArrivalRV, (100.0, 200.0))
        val snk = new Sink      ("snk", (800.0, 200.0))
        val qu1 = new WaitQueue ("qu1", Array (300.0, 200.0, 20.0, 20.0), 1000)
        val qu2 = new WaitQueue ("qu2", Array (500.0, 200.0, 20.0, 20.0), 1000)
        val gt1 = new Gate2     ("gt1", qu1, 100, Array (350.0, 200.0, 20.0, 20.0), time(1) + time(2))
        val gt2 = new Gate2     ("gt2", qu2, 100, Array (550.0, 200.0, 20.0, 20.0), time(2) + time(3))
        val tp1 = new Transport ("tp1", src, qu1, moveRV)
        val tp2 = new Transport ("tp2", gt1, qu2, moveRV)
        val tp3 = new Transport ("tp3", gt2, snk, moveRV)
 
        val gc  = new GateController ("gc", this, Array (gt1, gt2), state, time)

        addComponent (src, snk, qu1, qu2, gt1, gt2, tp1, tp2, tp3, gc)

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                tp1.move ()
                if (gt1.shut) qu1.waitIn ()
                tp2.move ()
                if (gt2.shut) qu2.waitIn ()
                tp3.move ()
                snk.leave ()
            }
        }
    }
}

object GateControllerTest3 extends App
{
    val gcm = new GateControllerModel3 ("gcm", 300, Sharp (3000), Sharp (3000.0))
//    Coroutine.startup ()
    gcm.simulate ()
    gcm.complete ()        
    Coroutine.shutdown ()
    
    class GateControllerModel3 (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name, reps = 2, animating = false)
    {
        val state = Array (Array ("red",   "green"  ), 
                           Array ("green", "red"  ))
                           

        val time  = Array (12000.0, 5000.0)

        val sc1 = new Source    ("sc1", this, Car, 0, nArrivals, iArrivalRV, (100.0, 200.0))
        val sc2 = new Source    ("sc2", this, Car, 1, nArrivals, iArrivalRV, (320.0, 50.0))
        val snk = new Sink      ("snk", (500.0, 200.0))
        val qu1 = new WaitQueue ("qu1", Array (300.0, 200.0, 20.0, 20.0), 1000)
        val qu2 = new WaitQueue ("qu2", Array (320.0, 180.0, 20.0, 20.0), 1000)
        val gt1 = new Gate2     ("gt1", qu1, 100, Array (350.0, 200.0, 20.0, 20.0), time(1))
        val gt2 = new Gate2     ("gt2", qu2, 100, Array (320.0, 220.0, 20.0, 20.0), time(0), _redirect = gt1)
        val tp1 = new Transport ("tp1", sc1, qu1, moveRV)
        val tp2 = new Transport ("tp2", sc2, qu2, moveRV)
        val tp3 = new Transport ("tp3", gt1, snk, moveRV)
 
        val gc  = new GateController ("gc", this, Array (gt1, gt2), state, time)

        addComponent (sc1, sc2, snk, qu1, qu2, gt1, gt2, tp1, tp2, tp3, gc)

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                if (subtype == 0) {
                    tp1.move () 
                    if (gt1.shut) qu1.waitIn ()
                    tp3.move ()
                    snk.leave ()
                } else {
                    tp2.move ()
                    if (gt2.shut) qu2.waitIn ()
                    tp3.move ()
                    snk.leave ()
                }
            }
        }
    }
}
*/












