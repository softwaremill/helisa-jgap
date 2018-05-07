package com.softwaremill.helisa

import com.softwaremill.helisa.gene.Gene
import org.{jgap => j}

trait Chromosome[A] {
  def genes(a: A): Vector[Gene[_, _ <: j.Gene]]

  def fromJ(jChromo: j.IChromosome): Either[String, A]

}
