
package apps.traffic

import scalation.random._
import scalation.process._

object TrafficTest extends App
{
    val tm = new TrafficModel ("tm", 100, Sharp (1000), Sharp (2000), true)

//    Coroutine.startup ()
    tm.simulate ()
    tm.complete ()
//    Coroutine.shutdown ()

    class TrafficModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate, ani: Boolean = false)
          extends Model (name, animating = ani)
    {
        val srcx = 100
        val srcy = 300
        val dx   = 75

        val src = new Source    ("source", this, Car, 0, nArrivals, iArrivalRV, (srcx.toDouble, srcy.toDouble))
        val wq1 =     WaitQueue ("wq1", (srcx + 3 * dx, srcy))
        val wq2 =     WaitQueue ("wq2", (srcx + 7 * dx, srcy))

        val j11 = new Junction ("junc11", this, Sharp (0), Array (srcx + 1 * dx, srcy, 20.0, 20.0))
        val j12 = new Junction ("junc12", this, Sharp (0), Array (srcx + 2 * dx, srcy, 20.0, 20.0))
        val j21 = new Junction ("junc21", this, Sharp (0), Array (srcx + 5 * dx, srcy, 20.0, 20.0))
        val j22 = new Junction ("junc22", this, Sharp (0), Array (srcx + 6 * dx, srcy, 20.0, 20.0))
        val j31 = new Junction ("junc31", this, Sharp (0), Array (srcx + 9 *  dx, srcy, 20.0, 20.0))
        val j32 = new Junction ("junc32", this, Sharp (0), Array (srcx + 10 * dx, srcy, 20.0, 20.0))

        val sig1 = new TrafficSignal ("signal1", wq1, 1000, Array (srcx + 4 * dx, srcy, 20.0, 20.0), 1000.0)       

        val sig2 = new TrafficSignal ("signal2", wq2, 1000, Array (srcx + 8 * dx, srcy, 20.0, 20.0), 1000.0)

        val snk  = new Sink      ("sink", (srcx.toDouble + 11 * dx, srcy.toDouble))

        val rd11 = new Transport ("road11", src,  j11, moveRV)
        val rd12 = new Transport ("road12", j11,  j12, moveRV)
        val rd13 = new Transport ("road13", j12,  wq1, moveRV)
        val rd21 = new Transport ("road21", sig1, j21, moveRV)
        val rd22 = new Transport ("road22", j21,  j22, moveRV)
        val rd23 = new Transport ("road23", j22,  wq2, moveRV)
        val rd31 = new Transport ("road31", sig2, j31, moveRV)
        val rd32 = new Transport ("road32", j31,  j32, moveRV)
        val rd33 = new Transport ("road33", j32,  snk, moveRV)

        val sc = new SignalController ("control", this, Array (sig1, sig2), Array (Array ("red", "green"), Array ("green", "red")), Array (1000.0, 1000.0))

        addComponent (src, snk, wq1, wq2, j11, j12, j21, j22, j31, j32, sig1, rd11, rd12, rd13, sig2, rd21, rd22, rd23, rd31, rd32, rd33, sc)

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                rd11.move ()
                j11.jump  ()
                rd12.move ()
                j12.jump  ()
                rd13.move ()
                if (sig1.shut || wq1.size > 0) wq1.waitIn () else wq1.noWait ()
                rd21.move ()
                j21.jump  ()
                rd22.move ()
                j22.jump  ()
                rd23.move ()
                if (sig2.shut || wq2.size > 0) wq2.waitIn () else wq2.noWait ()
                rd31.move ()
                j31.jump  ()
                rd32.move ()
                j32.jump  ()
                rd33.move ()
                snk.leave ()
            }
        }
    }
}
