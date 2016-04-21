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

package fr.cnrs.liris.privamov.ops.analysis

import java.time.Duration

import fr.cnrs.liris.accio.core.framework.{Analyzer, Metric}
import fr.cnrs.liris.accio.lib.param.ParamMap
import fr.cnrs.liris.privamov.lib.clustering.DTClusterer
import fr.cnrs.liris.privamov.model.{Poi, Track}
import fr.cnrs.liris.util.Distance

/**
 * Analyzer output statistics about the POIs that can be extracted from a track, using a
 * classical DJ-clustering algorithm.
 */
class PoisAnalyzer extends Analyzer {
  val diameter = param.create[Distance]("diameter", "Clustering maximum diameter")
  val duration = param.create[Duration]("duration", "Clustering minimum duration")

  override def metrics: Seq[String] = Seq("count", "size", "duration", "size-ratio", "duration-ratio")

  override def analyze(paramMap: ParamMap, track: Track): Seq[Metric] = {
    val clusterer = new DTClusterer(paramMap(duration), paramMap(diameter))
    val pois = clusterer.cluster(track.records).map(c => Poi(c.records))
    val sizeInPoi = pois.map(_.size).sum
    val durationInPoi = pois.map(_.duration.getSeconds).sum
    Seq(
      Metric("count", pois.size),
      Metric("size", sizeInPoi),
      Metric("duration", durationInPoi),
      Metric("size-ratio", sizeInPoi.toDouble / track.size),
      Metric("duration-ratio", durationInPoi.toDouble / track.duration.getSeconds))
  }
}