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

package fr.cnrs.liris.privamov.lib.io.codec

import java.time.Instant

import com.typesafe.scalalogging.LazyLogging
import fr.cnrs.liris.privamov.model.{Record, Track}
import fr.cnrs.liris.privamov.lib.io.index.{DirectoryIndex, Index}
import fr.cnrs.liris.privamov.lib.io.reader.{EncodedRecord, RecordReader, TextLineReader, WholeFileReader}
import fr.cnrs.liris.privamov.geo.Point

import scala.reflect._

case class CsvSource(url: String) extends DataSource[Record] {
  override val index: Index = new DirectoryIndex(
    url = url,
    extractLabels = path => Set(path.getFileName.toString.dropRight(4)))

  override def reader: RecordReader = new TextLineReader

  override def decoder: Decoder[Record] = new CsvDecoder
}

class CsvDecoder extends Decoder[Record] with LazyLogging {
  override val elementClassTag = classTag[Record]

  override def decode(record: EncodedRecord): Option[Record] = {
    val line = new String(record.bytes).trim
    val parts = line.split(",")
    if (parts.length != 3) {
      logger.warn(s"Invalid line in ${record.url}: $line")
      None
    } else {
      val x = parts(0).toDouble
      val y = parts(1).toDouble
      val time = Instant.ofEpochSecond(parts(2).toLong)
      val user = record.labels.mkString(",")
      Some(Record(user, Point(x, y), time))
    }
  }
}

class CsvEncoder extends Encoder[Record] {
  override def encode(obj: Record): Array[Byte] =
    s"${obj.user},${obj.point.x},${obj.point.y},${obj.time.getEpochSecond}".getBytes
}

case class CsvTrackSource(url: String) extends DataSource[Track] {
  override val index: Index = new DirectoryIndex(
    url = url,
    extractLabels = path => Set(path.getFileName.toString.dropRight(4)))

  override def reader: RecordReader = new WholeFileReader

  override def decoder: Decoder[Track] = new CsvTrackDecoder
}

class CsvTrackDecoder extends Decoder[Track] with LazyLogging {
  override val elementClassTag = classTag[Track]

  override def decode(record: EncodedRecord): Option[Track] = {
    val user = record.labels.mkString(",")
    val records = new String(record.bytes).split("\n").flatMap(line => {
      val parts = line.trim.split(",")
      if (parts.length != 3) {
        logger.warn(s"Invalid line in ${record.url}: $line")
        None
      } else {
        val x = parts(0).toDouble
        val y = parts(1).toDouble
        val time = Instant.ofEpochSecond(parts(2).toLong)
        Some(Record(user, Point(x, y), time))
      }
    })
    Some(if (records.nonEmpty) Track(records) else Track.empty(user))
  }
}