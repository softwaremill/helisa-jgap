package com.softwaremill

import com.softwaremill.helisa.gene.Gene._
import com.softwaremill.helisa.gene._
import org.{jgap => j}
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.traversable.FromTraversable
import shapeless.{Generic, HList, Poly1}

package object helisa {

  val genes = Gene.genes

  val geneticOperators = GeneticOperator

  val naturalSelectors = NaturalSelector.selectors

  implicit class GenotypeOps[G](a: G)(implicit genotype: Genotype[G]) {

    def genes: Vector[Gene[_]] = genotype.genes(a)

    def toJ(implicit config: Evolver[G]): j.IChromosome = {
      val genes: Seq[j.Gene] = a.genes.map(_.jGene)
      val jChromosome        = new j.Chromosome(config.jConfig, genes.toArray)
      config.validator.map(_.toJ).foreach(jChromosome.setConstraintChecker)
      jChromosome
    }

  }

  implicit class JChromosomeOps[G](jChromo: j.IChromosome)(implicit genotype: Genotype[G]) {
    def fromJ: Either[String, G] = genotype.fromJ(jChromo)
  }

  implicit def defaultResolver(implicit c: Evolver[_]): JGeneResolver = {
    case g: j.impl.BooleanGene        => new BooleanGene(g)
    case g: j.impl.IntegerGene        => new IntGene(g)
    case g: j.impl.MutipleIntegerGene => new IntOfMultipleGene(g)
    case g: j.impl.DoubleGene         => new DoubleGene(g)
    case g: j.impl.StringGene         => new StringGene(g)
    case g: j.impl.FixedBinaryGene    => new BitGene(g)
    case g: j.impl.MapGene            => new DiscreteValueGene[Any](g)
    case u                            => throw new UnsupportedOperationException(s"Unsupported JGAP gene conversion for type: ${u.getClass}")
  }

  implicit def caseClassGenotype[G: Evolver, Repr <: HList](implicit g: Generic.Aux[G, Repr],
                                                            tT: ToTraversable.Aux[Repr, Vector, Gene[_]],
                                                            fT: FromTraversable[Repr]): Genotype[G] = {
    import shapeless.syntax.std.traversable._

    new Genotype[G] {
      def genes(a: G): Vector[Gene[_]] = {
        val repr = g.to(a)
        tT(repr)
      }

      def fromJ(jChromo: j.IChromosome): Either[String, G] = {
        val repr = jChromo.getGenes.toTraversable.map(Gene.fromJ).toHList[Repr]
        repr.map(g.from).toRight(s"Could not convert following IChromosome to case class: $jChromo")
      }
    }

  }

  implicit class PhenotypeOps[G](g: G) {
    def toPhenotype[A](implicit p: Phenotype[G, A]): A = p.convert(g)
  }
}
