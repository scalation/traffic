
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman
 *  @version 1.1
 *  @date    Mon Sep 28 11:24:31 EDT 2015 
 *  @see     LICENSE (MIT style license file).
 */

package apps.traffic

import math.abs

import scala.collection.mutable.HashMap
import scala.util.Sorting.stableSort

import scalation.linalgebra.{VectorD, VectorI}
import scalation.math._
import scalation.random._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Genetic Algorithm for Traffic light optimization
 *  @param f  function to be optimized
 *  @param n  dimensionality of the domain
 */
class TrafficGA (f: VectorI => Double, n: Int)
{
    private val POP_SIZE = 20

    private val MAX_GEN = 100

    private var population = Array.ofDim [VectorI] (POP_SIZE)

    private val popMap = new HashMap [VectorI, Double] ()

    private var best: VectorI = null

    private var noChange = 0

    private var generation = 0

    def initPop (lower: Array [Int], upper: Array [Int]) 
    {
        val rand = Array.ofDim [Variate] (n)
        for (i <- 0 until n) rand(i) = Randi (lower(i), upper(i))
        for (i <- 0 until POP_SIZE) {
            val member = new VectorI (n)            
            for (j <- 0 until n) {
                member(j) = rand(j).igen * 1000
            }
            population(i) = member
        }
        best = population(0)
    }

    def fitness ()
    {       
        val fit = Array.ofDim [Tuple2 [VectorI, Double]] (POP_SIZE)
        for (i <- 0 until POP_SIZE) {
            val m = population(i)
            var v = 0.0
            if (! popMap.contains (m)) { v = f(m);  popMap += m -> v }
            else                         v = popMap(m)
            fit (i) = (m, v)
        }
        stableSort (fit, (e1: Tuple2 [VectorI, Double], e2: Tuple2 [VectorI, Double]) => e1._2 < e2._2)
        for (i <- 0 until POP_SIZE) population(i) = fit(i)._1
        if (! population(0).equals (best)) best = population(0)        
        else noChange += 1
    }

    def printPop () { println ("\npopulation:"); for (m <- population) println ("x = " + m + ", f(x) = " + popMap (m)) }

    def crossOver () 
    {
        var done = false
        val rand_choice = Bernoulli ()
        val rand_parent = Discrete (probs (1.5))
        for (k <- POP_SIZE / 2 until POP_SIZE) {
            val i = rand_parent.igen
            val m1 = population (i)
            val m2 = population (i + 1)
            val mnew = new VectorI (n)
            for (j <- 0 until n) {
                val r = rand_choice.igen
                if (r == 0) mnew(j) = m1(j) else mnew(j) = m2(j)
            }          
            population(k) = mnew
        }
    }

    def mutate (lower: Array [Int], upper: Array [Int])
    {
        val rand_direction = Discrete (VectorD (0.4, 0.2, 0.4), VectorD (-1.0, 0.0, 1.0))
        val rand_distance  = Randi (-30 + generation / 5, 30 - generation / 5)
        for (i <- 1 until POP_SIZE) {
            for (j <- 0 until n) {
//                val t = rand_direction.igen * rand_distance.igen * 1000
                val t = rand_distance.igen * 1000  
                var m = population (i)(j)
                m += t
                if (m < lower(j) * 1000) m = lower(j) * 1000
                if (m > upper(j) * 1000) m = upper(j) * 1000
                population(i)(j) = m
            }
        }
    }

    def probs (factor: Double): VectorD = 
    {
        var sum = 0.0
        val powers = new VectorD (POP_SIZE)
        for (i <- 0 until POP_SIZE) {
            val p = factor ~^ i
            sum += p
            powers (POP_SIZE - i - 1) = p
        }
        val p = new VectorD (POP_SIZE)
        for (i <- 0 until POP_SIZE) {
            p(i) = powers(i) / sum
        }
//        println ("p    = " + p)
//        println ("psum = " + p.sum) 
        p
    }

    def solve (lower: Array [Int], upper: Array [Int]): VectorI = 
    {
        initPop (lower, upper)
        fitness ()
        for (i <- 1 to MAX_GEN if noChange < 10) {
            println ("Generation " + i + ":")
            generation = i
            crossOver ()
            mutate (lower, upper)
            fitness ()
            printPop ()
            
        }
        population (0)
    }
}

object TrafficGATest extends App
{
    val lower = Array (10, 10, 5)
    val upper = Array (100, 100, 15)

    val opt = new TrafficGA (f, 3)
    opt.solve (lower, upper)

/*
    opt.initPop (lower, upper)

    opt.printPop ()

    opt.fitness ()

    opt.printPop ()

    opt.mutate ()

    opt.printPop ()

    opt.crossOver ()

    opt.printPop ()
*/
    def f (x: VectorI): Double = 
    {
        x.sum
    }
    
}

    








    
