
package apps.traffic

import scalation.random._
import scalation.process._

object TrafficTest extends App
{
    val tm = new TrafficModel ("tm", 100, Sharp (1000), Sharp (4000), true)

//    Coroutine.startup ()
    tm.simulate ()
    tm.complete ()
//    Coroutine.shutdown ()

    class TrafficModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate, ani: Boolean = false)
          extends Model (name, animating = ani)
    {
        val src = new Source    ("source", this, Car, 0, nArrivals, iArrivalRV, (400.0, 300.0))
        val wq  =     WaitQueue ("wq", (600, 300))
        val snk = new Sink      ("sink", (900.0, 300.0))
        val rd1 = new Transport ("road1", src, wq, moveRV)

        val sig = new TrafficSignal ("signal", wq, 1000, Array (700.0, 300.0, 20.0, 20.0), 5000.0)

        val rd2 = new Transport ("road2", sig, snk, moveRV)

        val sc = new SignalController ("control", this, Array (sig), Array (Array ("red"), Array ("green")), Array (5000.0, 5000.0))

        addComponent (src, snk, wq, rd1, sig, rd2, sc)

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                rd1. move ()
                wq.noWait ()
                rd2. move ()
                snk.leave ()
            }
        }
    }
}
