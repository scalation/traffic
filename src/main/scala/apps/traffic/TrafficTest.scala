
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
        val src = new Source ("source", this, Car, 0, nArrivals, iArrivalRV, (200.0, 300.0))
        val snk = new Sink   ("sink", (500.0, 300.0))
        val rd  = new Transport ("road", src, snk, moveRV)

        addComponent (src, snk, rd)

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                rd. move  ()
                snk.leave ()
            }
        }
    }
}
