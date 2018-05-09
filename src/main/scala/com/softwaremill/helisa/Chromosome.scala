package com.softwaremill.helisa

import com.softwaremill.helisa.gene.Gene
import org.{jgap => j}

import scala.annotation.implicitNotFound

// stripMargin cannot be used because annotation processor expects string constants (and ignores the directive otherwise)
// format: off
@implicitNotFound(msg =
"""Chromosome for ${A} not found.

For automatic generation, you need these things:
1. *All* fields of the case class you use *must* extend com.softwaremill.helisa.gene.Gene
2. import com.softwaremill.helisa._
""")
// format: on
trait Chromosome[A] {
  def genes(a: A): Vector[Gene[_]]

  def fromJ(jChromo: j.IChromosome): Either[String, A]

}
