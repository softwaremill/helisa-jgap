package com.softwaremill.sgap

import org.{jgap => j}

abstract class GeneticOperator[A : Chromosome : Configuration] extends j.GeneticOperator {

  def apply(chromos: Seq[A]): Seq[A]

}
