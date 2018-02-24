package com.softwaremill.sgap

import com.softwaremill.sgap.gene.Gene
import org.{jgap => j}

trait Chromosome[A] {
  def genes(a: A): Vector[Gene[_, _ <: j.Gene]]

  def fromJ(jChromo: j.IChromosome): A //ErrorEither?

}
