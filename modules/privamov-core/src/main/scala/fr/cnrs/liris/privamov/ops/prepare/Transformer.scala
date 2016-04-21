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

import fr.cnrs.liris.accio.core.framework.{Mapper, Transformer}
import fr.cnrs.liris.accio.lib.param.ParamMap
import fr.cnrs.liris.privamov.model.{Record, Track}

import scala.collection.mutable

private[privamov] trait SlidingSampler extends Mapper {
  override final def map(paramMap: ParamMap, track: Track): Track = {
    if (track.isEmpty) {
      track
    } else {
      val newRecords = mutable.ListBuffer.empty[Record]
      var maybePrev: Option[Record] = None
      for (record <- track.records) {
        val keep = maybePrev match {
          case Some(prev) => sample(paramMap, prev, record)
          case None => true
        }
        if (keep) {
          newRecords += record
          maybePrev = Some(record)
        }
      }
      track.copy(records = newRecords)
    }
  }

  protected def sample(paramMap: ParamMap, prev: Record, curr: Record): Boolean
}

/**
 * Base class for all functions using previous record and current one to take a decision.
 */
private[privamov] trait SlidingSplitter extends Transformer {
  override final def transform(paramMap: ParamMap, input: Track): Seq[Track] = {
    val output = mutable.ListBuffer[Track]()
    val buffer = mutable.ListBuffer[Record]()
    for (record <- input.records) {
      if (buffer.nonEmpty && split(paramMap, buffer, record)) {
        output += Track(buffer)
        buffer.clear()
      }
      buffer += record
    }
    if (buffer.nonEmpty) {
      output += Track(buffer)
    }
    output
  }

  protected def split(paramMap: ParamMap, buffer: Seq[Record], curr: Record): Boolean
}