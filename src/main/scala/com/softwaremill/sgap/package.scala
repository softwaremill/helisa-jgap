package com.softwaremill

import com.softwaremill.sgap.gene.Gene
import org.{jgap => j}
import shapeless.ops.hlist.ToTraversable
import shapeless.{Generic, HList, Poly1}

package object sgap {

  implicit class ChromosomeOps[A](a: A)(implicit chromoA: Chromosome[A]) {

    def genes: Vector[Gene[_, _ <: j.Gene]] = chromoA.genes(a)

    def toJ(config: Configuration[A]): j.IChromosome = {
      val genes       = a.genes.map(_.jGene)
      val jChromosome = new j.Chromosome(config.jConfig, genes.toArray)
      jChromosome
    }

  }

  implicit class JChromosomeOps[A](jChromo: j.IChromosome)(implicit chromoA: Chromosome[A]) {
    def fromJ: A = chromoA.fromJ(jChromo)
  }

  implicit def caseClassChromosome[A, Repr <: HList](implicit g: Generic.Aux[A, Repr],
                                                     tT: ToTraversable.Aux[Repr, Vector, Gene[_, _ <: j.Gene]]): Chromosome[A] = {
    import shapeless._
    import shapeless.ops.hlist._

    new Chromosome[A] {
      def genes(a: A): Vector[Gene[_, _ <: j.Gene]] = {
        val repr = g.to(a)
        tT(repr)
      }

      def fromJ(jChromo: j.IChromosome): A = ??? //TODO: add ab nihilo conversion
    }

  }
}
