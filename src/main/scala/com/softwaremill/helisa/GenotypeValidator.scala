package com.softwaremill.helisa
import com.softwaremill.helisa.gene.Gene
import org.{jgap => j}

abstract class GenotypeValidator[G: Genotype: Evolver] {

  def validate(gene: Gene[_], genotype: G, index: Int): Boolean

  def toJ: j.IGeneConstraintChecker = { (gene: j.Gene, value: Any, chromosome: j.IChromosome, index: Int) =>
    {
      if (value == null) true //workaround for j.Chromosome:1879
      else
        chromosome.fromJ.map(chromo => validate(Gene.fromJ(gene), chromo, index)).getOrElse(false)
    }
  }

}

object GenotypeValidator {

  //for some reason this is necessary (default conversion doesn't work - maybe because this is an AC not a trait?
  def apply[G: Genotype: Evolver](v: (Gene[_], G, Int) => Boolean): GenotypeValidator[G] = new GenotypeValidator[G] {
    override def validate(gene: Gene[_], chromosome: G, index: Int): Boolean = v(gene, chromosome, index)
  }
}
