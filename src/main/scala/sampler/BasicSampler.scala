package com.sinanspd.qure.circuit.sampler

import com.sinanspd.qure.circuit.sampler.Sampler
import probability_monad.Distribution
import scala.collection.immutable.Vector

class BasicSampler(dist: List[(Double, Vector[Boolean])]) extends Sampler{

    def sample(): Vector[Boolean] = 
        dist.sortBy(_._1).head._2

    def sample2() = {
        val remappedDistributions = dist.map(a => {
            val mapped = a._2.map{
                case true => 1
                case _ => 0
            }
            (mapped.mkString, a._1)
        })
        Distribution.discrete(remappedDistributions: _*)
    }
}