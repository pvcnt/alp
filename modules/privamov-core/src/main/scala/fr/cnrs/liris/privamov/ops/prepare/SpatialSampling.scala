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

package fr.cnrs.liris.privamov.ops.prepare

import fr.cnrs.liris.privamov.model.Record
import fr.cnrs.liris.accio.lib.param.ParamMap
import fr.cnrs.liris.util.Distance

/**
 * Enforce a minimum distance between two consecutive records in a track. If the distance is
 * less than a given threshold, records will be discarded until the next point that fullfill
 * the minimum distance requirement.
 */
class SpatialSampling(minDistance: Distance) extends SlidingSampler {
  val distance = param.create[Distance]("distance", "Minimum distance between two consecutive records")

  override protected def sample(paramMap: ParamMap, prev: Record, curr: Record): Boolean = {
    prev.point.distance(curr.point) >= paramMap(distance)
  }
}