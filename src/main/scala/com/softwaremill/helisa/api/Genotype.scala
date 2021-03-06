package com.softwaremill.helisa.api
import org.{jgap => j}

import scala.annotation.implicitNotFound

// stripMargin cannot be used because annotation processor expects string constants (and ignores the directive otherwise)
// format: off
@implicitNotFound(msg =
"""Chromosome for ${G} not found.

For automatic generation, you need these things:
1. *All* fields of the case class you use *must* extend com.softwaremill.helisa.api.Gene
2. import com.softwaremill.helisa._
""")
// format: on
trait Genotype[G] {
  def genes(a: G): Vector[Gene[_]]

  def fromJ(jChromo: j.IChromosome): Either[String, G]

}
