/*
 * Copyright LIRIS-CNRS (2016)
 * Contributors: Vincent Primault <vincent.primault@liris.cnrs.fr>
 *
 * This software is a computer program whose purpose is to study location privacy.
 *
 * This software is governed by the CeCILL-B license under French law and
 * abiding by the rules of distribution of free software. You can use,
 * modify and/ or redistribute the software under the terms of the CeCILL-B
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty and the software's author, the holder of the
 * economic rights, and the successive licensors have only limited liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading, using, modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean that it is complicated to manipulate, and that also
 * therefore means that it is reserved for developers and experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and, more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL-B license and that you accept its terms.
 */

package fr.cnrs.liris.accio.core.framework

import java.util.NoSuchElementException

import fr.cnrs.liris.privamov.ops.analysis.{BasicAnalyzer, PoisAnalyzer}
import fr.cnrs.liris.privamov.ops.eval._
import fr.cnrs.liris.privamov.ops.lppm.SpeedSmoothing
import fr.cnrs.liris.privamov.ops.lppm.geoind.{GeoIndistinguishability, PredictiveMechanism}
import fr.cnrs.liris.privamov.ops.prepare._

/**
 * An operation registry keeps a trace of all registered operations inside Accio.
 */
class OpRegistry {
  private[this] val opsMap = {
    val ops = Seq[Class[_]](
      // Transformers
      classOf[CollapseTemporalGaps],
      classOf[GaussianKernelSmoothing],
      classOf[MaxDuration],
      classOf[MinDuration],
      classOf[MinSize],
      classOf[SpatialSampling],
      classOf[SplitByDuration],
      classOf[SplitBySize],
      classOf[SplitBySpatialGap],
      classOf[SplitByTemporalGap],
      classOf[SplitSequentially],
      classOf[TemporalSampling],
      classOf[UniformSampling],

      // Protection mechanisms
      classOf[GeoIndistinguishability],
      classOf[SpeedSmoothing],
      classOf[PredictiveMechanism],

      // Evaluators
      classOf[PoisRetrieval],
      classOf[SpatialDistortion],
      classOf[TemporalDistortion],
      classOf[AreaCoverage],
      classOf[DataCompleteness],
      classOf[TransmissionDelay],

      // Analyzers
      classOf[BasicAnalyzer],
      classOf[PoisAnalyzer]
    )
    ops.map { cls =>
      require(classOf[Operation].isAssignableFrom(cls), s"Class ${cls.getName} should be an operation")
      cls.getSimpleName -> cls.asInstanceOf[Class[Operation]]
    }.toMap
  }

  /**
   * Check whether the registry contains an [[Operation]] for the given name.
   *
   * @param name An operation name
   * @return True if there is an operation for the given name, false otherwise
   */
  def contains(name: String): Boolean = opsMap.contains(name)

  /**
   * Return the [[Operation]] class for the given name, if it exists.
   *
   * @param name An operation name
   */
  def get(name: String): Option[Class[Operation]] = opsMap.get(name)

  /**
   * Return the [[Operation]] class for the given name.
   *
   * @param name An operation name
   * @throws NoSuchElementException If there is no operation for the given name
   */
  @throws[NoSuchElementException]
  def apply(name: String): Class[Operation] = opsMap(name)
}