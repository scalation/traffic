
package apps.traffic

import scalation.random._
import scalation.process._
import scalation.util.Monitor._

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
        traceOff ()

        val dx   = 75
        val dy   = 50
        val srcx  = 100
        val srcy  = 300
        val srcx2 = srcx + 4 * dx
        val srcy2 = 50

        val nameOn = false

        val names = if (nameOn) Array ("src1", "src2", 
                                       "wq1", "wq2", "wq3", 
                                       "jE11", "jE12", "jE21", "jE22", "jE31", "jE32", 
                                       "jS11", "jS12", "jS21", "jS22",
                                       "sig1", "sig2", "sig3", 
                                       "snk1", "snk2",
                                       "rdE11", "rdE12", "rdE13", "rdE21", "rdE22", "rdE23", "rdE31", "rdE32", "rdE33",
                                       "rdS11", "rdS12", "rdS13", "rdS21", "rdS22", "rdS23")
                    else Array ("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")



        val src  = new Source   (names(0), this, Car, 0, nArrivals, iArrivalRV, (srcx.toDouble, srcy.toDouble))
        val src2 = new Source   (names(1), this, Car, 1, nArrivals, iArrivalRV, (srcx2.toDouble, srcy2.toDouble))
        val wq1 =     WaitQueue (names(2), (srcx + 3 * dx, srcy))
        val wq2 =     WaitQueue (names(3), (srcx + 8 * dx, srcy))
        val wq3 =     WaitQueue (names(4), (srcx2, srcy2 + 3 * dy))
//        val wq4 =     WaitQueue ("wq4", (srcx2, srcy2 + 9 * dy))

        val jE11 = new Junction (names(5), this, Sharp (0), Array (srcx + 1  * dx, srcy, 8.0, 8.0))
        val jE12 = new Junction (names(6), this, Sharp (0), Array (srcx + 2  * dx, srcy, 8.0, 8.0))
        val jE21 = new Junction (names(7), this, Sharp (0), Array (srcx + 6  * dx, srcy, 8.0, 8.0))
        val jE22 = new Junction (names(8), this, Sharp (0), Array (srcx + 7  * dx, srcy, 8.0, 8.0))
        val jE31 = new Junction (names(9), this, Sharp (0), Array (srcx + 11  * dx, srcy, 8.0, 8.0))
        val jE32 = new Junction (names(10), this, Sharp (0), Array (srcx + 12 * dx, srcy, 8.0, 8.0))

        val jS11 = new Junction (names(11), this, Sharp (0), Array (srcx2, srcy2 + 1  * dy, 8.0, 8.0))
        val jS12 = new Junction (names(12), this, Sharp (0), Array (srcx2, srcy2 + 2  * dy, 8.0, 8.0))
        val jS21 = new Junction (names(13), this, Sharp (0), Array (srcx2, srcy2 + 7  * dy, 8.0, 8.0))
        val jS22 = new Junction (names(14), this, Sharp (0), Array (srcx2, srcy2 + 8  * dy, 8.0, 8.0))
//        val jS31 = new Junction ("juncS31", this, Sharp (0), Array (srcx2, srcy2 + 13  * dy, 20.0, 20.0))
//        val jS32 = new Junction ("juncS32", this, Sharp (0), Array (srcx2, srcy2 + 14 * dy, 20.0, 20.0))

        val time1 = 1000.0 * 1
        val time2 = 1000.0 * 1

        val sig1 = new TrafficSignal (names(15), wq1, 1000, Array (srcx + 5 * dx, srcy, 20.0, 20.0), time1)       

        val sig2 = new TrafficSignal (names(16), wq2, 1000, Array (srcx + 10 * dx, srcy, 20.0, 20.0), time2)

        val sig3 = new TrafficSignal (names(17), wq3, 1000, Array (srcx2, srcy2 + 6 * dy, 20.0, 20.0), time1)       

//        val sig4 = new TrafficSignal ("signal4", wq4, 1000, Array (srcx2, srcy2 + 12 * dy, 20.0, 20.0), time2)

        val snk   = new Sink      (names(18),  (srcx.toDouble + 13 * dx, srcy.toDouble))
        val snk2  = new Sink      (names(19), (srcx2.toDouble, srcy2.toDouble + 9 * dy))

        val rdE11 = new Transport (names(20), src,   jE11, moveRV)
        val rdE12 = new Transport (names(21), jE11,  jE12, moveRV)
        val rdE13 = new Transport (names(22), jE12,  wq1,  moveRV)
        val rdE21 = new Transport (names(23), sig1,  jE21, moveRV)
        val rdE22 = new Transport (names(24), jE21,  jE22, moveRV)
        val rdE23 = new Transport (names(25), jE22,  wq2,  moveRV)
        val rdE31 = new Transport (names(26), sig2,  jE31, moveRV)
        val rdE32 = new Transport (names(27), jE31,  jE32, moveRV)
        val rdE33 = new Transport (names(28), jE32,  snk,  moveRV)

        val rdS11 = new Transport (names(29), src2,  jS11, moveRV)
        val rdS12 = new Transport (names(30), jS11,  jS12, moveRV)
        val rdS13 = new Transport (names(31), jS12,  wq3,  moveRV)
        val rdS21 = new Transport (names(32), sig3,  jS21, moveRV)
        val rdS22 = new Transport (names(33), jS21,  jS22, moveRV)
        val rdS23 = new Transport (names(34), jS22,  snk2,  moveRV)
//        val rdS31 = new Transport ("roadS31", sig4,  jS31, moveRV)
//        val rdS32 = new Transport ("roadS32", jS31,  jS32, moveRV)
//        val rdS33 = new Transport ("roadS33", jS32,  snk2, moveRV)




        val sc = new SignalController ("control", this, Array (sig1, sig2, sig3  /*, sig4*/), 
                                                        Array (Array ("red", "green", "green", "red"), Array ("green", "red", "red", "green")), Array (time1, time2))

        addComponent (src, snk, src2, snk2, wq1,   wq2,   wq3,   /*wq4*/   
                                jE11,  jE12,  jE21,  jE22,  jE31,  jE32, 
                                jS11,  jS12,  jS21,  jS22,  /*jS31,  jS32*/  
                                sig1,  sig2,  sig3,  /*sig4*/ sc, 
                                rdE11, rdE12, rdE13, rdE21, rdE22, rdE23, rdE31, rdE32, rdE33,
                                rdS11, rdS12, rdS13, rdS21, rdS22, rdS23 /*rdS31, rdS32, rdS33*/)

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                if (subtype == 0) {
                    rdE11.move ()
                    jE11.jump  ()
                    rdE12.move ()
                    jE12.jump  ()
                    rdE13.move ()
                    if (sig1.shut || wq1.size > 0) wq1.waitIn () else wq1.noWait ()
                    rdE21.move ()
                    jE21.jump  ()
                    rdE22.move ()
                    jE22.jump  ()
                    rdE23.move ()
                    if (sig2.shut || wq2.size > 0) wq2.waitIn () else wq2.noWait ()
                    rdE31.move ()
                    jE31.jump  ()
                    rdE32.move ()
                    jE32.jump  ()
                    rdE33.move ()
                    snk.leave  ()
                } else if (subtype == 1) {
                    rdS11.move ()
                    jS11.jump  ()
                    rdS12.move ()
                    jS12.jump  ()
                    rdS13.move ()
                    if (sig3.shut || wq3.size > 0) wq3.waitIn () else wq3.noWait ()
                    rdS21.move ()
                    jS21.jump  ()
                    rdS22.move ()
                    jS22.jump  ()
                    rdS23.move ()
/*                    
                    if (sig4.shut || wq4.size > 0) wq4.waitIn () else wq4.noWait ()
                    rdS31.move ()
                    jS31.jump  ()
                    rdS32.move ()
                    jS32.jump  ()
                    rdS33.move ()
*/
                    snk2.leave ()
                }
            }
        }
    }
}
