
package apps.traffic

import scalation.linalgebra._
import scalation.metamodel.QuadraticFit
import scalation.minima._
import scalation.random._
import scalation.process._
import scalation.util.Monitor._

object TrafficTest extends App
{
    var count = 0

    val lower = Array [Double] (5000.0, 5000.0, 1000.0)
    val upper = Array [Double] (120000.0, 120000.0, 20000.0)

    val x0 = VectorD (30000.0, 30000.0, 5000.0)
//    val xs = VectorD (2000.0,  2000.0,  1000.0)

/*    val qf = new QuadraticFit (f, 3, 7)

    qf.formGrid (x0, xs)

    val (xx, yy) = qf.response ()
    qf.fit (xx, yy)

    def fp (x: VectorD): Double = qf.qFormsEval (x)
*/
    def fi (x: VectorI): Double = f (x.toDouble)

    def gi (x: VectorI): Double = g (x.toDouble)

    val opt = new QuasiNewton1 (f, g)
//    val opt = new ConjGradient (fp, g)
//    val opt = new IntegerLocalSearch (fi, gi, 5000)
//    val opt = new IntegerNLP (f, 3, g)
//    val opt = new IntegerTabuSearch (fi, gi, 5000)
//    val opt = new GeneticAlgorithm (fi, x0)

//    opt.setRIndiv (Randi (15000, 45000)

    val sol = opt.solve (x0)

    println ("sol = " + sol)    

/*  
    var count = 0
    for (i <- 20000 to 30000 by 2000) {
        for (j <- (i - 4000) to (i + 4000) by 2000) {
            for (k <- 10000 to 20000 by 5000) {
                System.err.println ("i = " + i + ", j = " + j + ", k = " + k + "\t" + count)
                count += 1
                println ("i = " + i + ", j = " + j + ", k = " + k + "\t" + fi (VectorI (i, j, k)))
            }
        }
    }
*/


//    for (i <- 3 to 30) f (VectorD (3000.0 * i, 3000.0 * i, 5000.0))


    def g (x: VectorD): Double = 
    {
        var sum = 0.0
        for (i <- 0 until x.dim) {
            if      (x(i) < lower(i)) sum += (x(i) - lower(i)) * (x(i) - lower(i))
            else if (x(i) > upper(i)) sum += (x(i) - upper(i)) * (x(i) - upper(i))
        } 
        sum
    }

//    f (VectorD (20000.0, 20000.0, 5000.0))


    def proj (x: VectorD): VectorD =
    {
        for (i <- 0 until x.dim) {
            if      (x(i) < lower(i)) x(i) = lower(i) + 5000.0
            else if (x(i) > upper(i)) x(i) = upper(i) - 5000.0
        }
        x
    }

    def f (x: VectorD): Double =
    {
        proj (x)
        var res = 0.0
        
//        else {
            count += 1
            val tm = new TrafficModel ("tm", 30, Uniform (1000.0, 2000.0), Uniform (1700.0, 2300.0), x, false)

//            Coroutine.startup ()
//            println ("before")
            tm.simulate ()
            tm.complete ()
//           println ("after")
//            Coroutine.shutdown ()
	    val sv      = tm.statV.filter{ case (key, value) => key contains "sk"}.map { case (key, value) => value(0) }
            val sinkN   = tm.statN.filter{ case (key, value) => key contains "sk"}.map { case (key, value) => value(0) }

//            println ("x = " + x + ", sv    = " + sv)
//            println ("sinkN = " + sinkN)
       
            res = sv.reduceLeft (_+_)
//        }
        println ("x = " + x + ", f(x) = " + res)
        res
    }

    class TrafficModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate, times: VectorD, ani: Boolean = false, aniR: Double = 1.0)
          extends Model (name, animating = ani, aniRatio = aniR)
    {
        traceOff ()

        val dx   = 75
        val dy   = 50
        val srcx  = 100
        val srcy  = 300
        val srcx2 = srcx + 4 * dx
        val srcy2 = 50
        val srcx3 = srcx + 9 * dx
        val srcy3 = srcy2

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



        val src  = new Source   ("sc1", this, Car, 0, nArrivals, iArrivalRV, (srcx.toDouble, srcy.toDouble))
        val src2 = new Source   ("sc2", this, Car, 1, nArrivals, iArrivalRV, (srcx2.toDouble, srcy2.toDouble))
        val src3 = new Source   ("sc3", this, Car, 2, nArrivals, iArrivalRV, (srcx3.toDouble, srcy3.toDouble))
        val wq1 =     WaitQueue ("wq1", (srcx + 3 * dx, srcy))
        val wq2 =     WaitQueue ("wq2", (srcx + 8 * dx, srcy))
        val wq3 =     WaitQueue ("wq3", (srcx2, srcy2 + 3 * dy))
        val wq4 =     WaitQueue ("wq4", (srcx3, srcy3 + 3 * dy))

//        val wq4 =     WaitQueue ("wq4", (srcx2, srcy2 + 9 * dy))

        val jE11 = new Junction ("j1",  this, Sharp (0), Array (srcx + 1  * dx, srcy, 8.0, 8.0))
        val jE12 = new Junction ("j2",  this, Sharp (0), Array (srcx + 2  * dx, srcy, 8.0, 8.0))
        val jE21 = new Junction ("j3",  this, Sharp (0), Array (srcx + 6  * dx, srcy, 8.0, 8.0))
        val jE22 = new Junction ("j4",  this, Sharp (0), Array (srcx + 7  * dx, srcy, 8.0, 8.0))
        val jE31 = new Junction ("j5",  this, Sharp (0), Array (srcx + 11  * dx, srcy, 8.0, 8.0))
        val jE32 = new Junction ("j6",  this, Sharp (0), Array (srcx + 12 * dx, srcy, 8.0, 8.0))

        val jS11 = new Junction ("j7",  this, Sharp (0), Array (srcx2, srcy2 + 1  * dy, 8.0, 8.0))
        val jS12 = new Junction ("j8",  this, Sharp (0), Array (srcx2, srcy2 + 2  * dy, 8.0, 8.0))
        val jS21 = new Junction ("j9",  this, Sharp (0), Array (srcx2, srcy2 + 7  * dy, 8.0, 8.0))
        val jS22 = new Junction ("j10", this, Sharp (0), Array (srcx2, srcy2 + 8  * dy, 8.0, 8.0))

        val jS31 = new Junction ("j11", this, Sharp (0), Array (srcx3, srcy3 + 1  * dy, 8.0, 8.0))
        val jS32 = new Junction ("j12", this, Sharp (0), Array (srcx3, srcy3 + 2  * dy, 8.0, 8.0))
        val jS41 = new Junction ("j13", this, Sharp (0), Array (srcx3, srcy3 + 7  * dy, 8.0, 8.0))
        val jS42 = new Junction ("j14", this, Sharp (0), Array (srcx3, srcy3 + 8  * dy, 8.0, 8.0))

//        val jS31 = new Junction ("juncS31", this, Sharp (0), Array (srcx2, srcy2 + 13  * dy, 20.0, 20.0))
//        val jS32 = new Junction ("juncS32", this, Sharp (0), Array (srcx2, srcy2 + 14 * dy, 20.0, 20.0))

        val sig1 = new TrafficSignal ("sg1", wq1, 1000, Array (srcx + 5 * dx, srcy, 20.0, 20.0), times(0))       

        val sig2 = new TrafficSignal ("sg2", wq2, 1000, Array (srcx + 10 * dx, srcy, 20.0, 20.0), times(0))

        val sig3 = new TrafficSignal ("sg3", wq3, 1000, Array (srcx2, srcy2 + 6 * dy, 20.0, 20.0), times(1))    

        val sig4 = new TrafficSignal ("sg4", wq4, 1000, Array (srcx3, srcy3 + 6 * dy, 20.0, 20.0), times(1))   

//        val sig4 = new TrafficSignal ("signal4", wq4, 1000, Array (srcx2, srcy2 + 12 * dy, 20.0, 20.0), time2)

        val snk   = new Sink ("sk1", (srcx.toDouble + 13 * dx, srcy.toDouble))
        val snk2  = new Sink ("sk2", (srcx2.toDouble, srcy2.toDouble + 9 * dy))
        val snk3  = new Sink ("sk3", (srcx3.toDouble, srcy3.toDouble + 9 * dy))     

        val rdE11 = new Transport ("rd1",  src,   jE11, moveRV)
        val rdE12 = new Transport ("rd2",  jE11,  jE12, moveRV)
        val rdE13 = new Transport ("rd3",  jE12,  wq1,  moveRV)
        val rdE21 = new Transport ("rd4",  sig1,  jE21, moveRV)
        val rdE22 = new Transport ("rd5",  jE21,  jE22, moveRV)
        val rdE23 = new Transport ("rd6",  jE22,  wq2,  moveRV)
        val rdE31 = new Transport ("rd7",  sig2,  jE31, moveRV)
        val rdE32 = new Transport ("rd8",  jE31,  jE32, moveRV)
        val rdE33 = new Transport ("rd9",  jE32,  snk,  moveRV)
        val rdS11 = new Transport ("rd10", src2,  jS11, moveRV)
        val rdS12 = new Transport ("rd11", jS11,  jS12, moveRV)
        val rdS13 = new Transport ("rd12", jS12,  wq3,  moveRV)
        val rdS21 = new Transport ("rd13", sig3,  jS21, moveRV)
        val rdS22 = new Transport ("rd14", jS21,  jS22, moveRV)
        val rdS23 = new Transport ("rd15", jS22,  snk2, moveRV)
        val rdS31 = new Transport ("rd16", src3,  jS31, moveRV)
        val rdS32 = new Transport ("rd17", jS31,  jS32, moveRV)
        val rdS33 = new Transport ("rd18", jS32,  wq4,  moveRV)
        val rdS41 = new Transport ("rd19", sig4,  jS41, moveRV)
        val rdS42 = new Transport ("rd20", jS41,  jS42, moveRV)
        val rdS43 = new Transport ("rd21", jS42,  snk3, moveRV)

//        val rdS31 = new Transport ("roadS31", sig4,  jS31, moveRV)
//        val rdS32 = new Transport ("roadS32", jS31,  jS32, moveRV)
//        val rdS33 = new Transport ("roadS33", jS32,  snk2, moveRV)

        val state1 = times(2)
        val state2 = times(0) - times(2)
        val state3 = times(2) 
        val state4 = times(1) - times(2)


        val sc = new SignalController ("control", this, Array (sig1, sig2, sig3, sig4), 
                                                        Array (Array ("green", "red", "red", "green"), 
                                                               Array ("green", "green", "red", "red"),
                                                               Array ("red", "green", "green", "red"),
                                                               Array ("red", "red", "green", "green")), 
                                                        Array (state1, state2, state3, state4))

        addComponent (src, snk, src2, snk2, src3, snk3, wq1,   wq2,   wq3, wq4,   
                                jE11,  jE12,  jE21,  jE22,  jE31,  jE32, 
                                jS11,  jS12,  jS21,  jS22, jS31,  jS32, jS41, jS42,  
                                sig1,  sig2,  sig3,  sig4, sc, 
                                rdE11, rdE12, rdE13, rdE21, rdE22, rdE23, rdE31, rdE32, rdE33,
                                rdS11, rdS12, rdS13, rdS21, rdS22, rdS23, rdS31, rdS32, rdS33, rdS41, rdS42, rdS43)

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
                    snk2.leave ()
                } else if (subtype == 2) {
                    rdS31.move ()
                    jS31.jump  ()
                    rdS32.move ()
                    jS32.jump  ()
                    rdS33.move ()
                    if (sig4.shut || wq4.size > 0) wq4.waitIn () else wq4.noWait ()
                    rdS41.move ()
                    jS41.jump  ()
                    rdS42.move ()
                    jS42.jump  ()
                    rdS43.move ()
                    snk3.leave ()
                }
               
            }
        }
    }
}

object TrafficModelTest extends App
{
    import TrafficTest.TrafficModel

    val x   = VectorD (30000.0, 30000.0, 5000.0)
    val fac = 2.5

    val tm = new TrafficModel ("tm", 12, Sharp (5000), Sharp (4000), x, true, 1.0 / fac)

    tm.simulate ()
    tm.complete ()

    val sv      = tm.statV.filter{ case (key, value) => key contains "sk"}.map { case (key, value) => value(0) }
    val sinkN   = tm.statN.filter{ case (key, value) => key contains "sk"}.map { case (key, value) => value(0) }
       
    println (sv.reduceLeft (_+_))    
}





