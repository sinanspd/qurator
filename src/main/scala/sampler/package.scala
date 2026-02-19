package com.sinanspd.qure.circuit

package object sampler{
    trait Sampler{
        def sample() : Vector[Boolean]
    }
}