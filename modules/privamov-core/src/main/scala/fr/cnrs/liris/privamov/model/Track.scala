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

package fr.cnrs.liris.privamov.model

import java.time.Duration

import com.google.common.base.MoreObjects
import fr.cnrs.liris.privamov.geo.FeatureCollection
import fr.cnrs.liris.util.{Distance, Speed}

/**
 * A track is a list of records belonging to a single user.
 *
 * @param user    A user name
 * @param records A list of temporally ordered records
 */
case class Track(user: String, records: Seq[Record]) {
  /**
   * Check if this track is empty, i.e., contains no record.
   */
  def isEmpty: Boolean = records.isEmpty

  /**
   * Check if this track is not empty, i.e., contains at least one record.
   */
  def nonEmpty: Boolean = records.nonEmpty

  /**
   * Return the size of the track, i.e., the number of records it contains.
   */
  def size: Int = records.size

  /**
   * Return the total duration of this track, i.e., the elapsed time between the first and
   * last record.
   */
  def duration: Duration =
    if (records.isEmpty) {
      Duration.ZERO
    } else {
      Duration.between(records.head.time, records.last.time)
    }

  /**
   * Return the total length of the track, i.e., the distance that has been travelled.
   */
  def length: Distance = distances.foldLeft(Distance.Zero)(_ + _)

  /**
   * Return the sequence of instantaneous speeds between consecutive points. The number of items
   * is the size of this track minus 1 (or 0 if the track is empty).
   */
  def speeds: Seq[Speed] =
    records.sliding(2).map(rs => {
      val dl = rs.head.point.distance(rs.last.point)
      val dt = Duration.between(rs.head.time, rs.last.time)
      Speed(dl, dt)
    }).toSeq

  /**
   * Return the sequence of distances between consecutive points. The number of items is the size
   * of this track minus 1 (or 0 if the track is empty).
   */
  def distances: Seq[Distance] =
    records.sliding(2).map(rs => rs.head.point.distance(rs.last.point)).toSeq

  /**
   * Return the sequence of durations between consecutive points. The number of durations is equal
   * to the size of this collection minus 1 (or 0 if the track is already empty).
   */
  def durations: Seq[Duration] =
    records.sliding(2).map(rs => Duration.between(rs.head.time, rs.last.time)).toSeq

  /**
   * Return a new track assigned to the same user created by applying a given function on records.
   *
   * @param fn A function modifying the records (it shouldn't modify the records' user)
   */
  def transform(fn: Seq[Record] => Seq[Record]): Track = copy(records = fn(records))

  /**
   * Convert this model to GeoJSON.
   */
  def toGeoJson: FeatureCollection = FeatureCollection(records.toSeq.map(_.toGeoJson))

  /**
   * Return an empty track with the same user and batch.
   */
  def empty: Track = new Track(user, Seq.empty)

  /**
   * Return a track with the same batch but with all records associated to another user name.
   *
   * @param user Another user name
   */
  def pseudonymise(user: String): Track = new Track(user, records.map(_.copy(user = user)))

  override def toString: String =
    MoreObjects.toStringHelper(this)
      .add("user", user)
      .add("size", records.size)
      .toString
}

/** Factory for [[Track]]. */
object Track {
  /**
   * Create an empty track associated with a given user.
   *
   * @param user A user name
   */
  def empty(user: String): Track = new Track(user, Seq.empty)

  /**
   * Create a track from a list of records. The user name will be automatically inferred from this data, and the
   * records will be temporally ordered.
   *
   * @param records A non-empty list of records
   */
  def apply(records: Iterable[Record]): Track = {
    val seq = records.toSeq.sortBy(_.time)
    require(seq.nonEmpty, "Cannot create a track from an empty list of records")
    new Track(seq.head.user, seq)
  }

  def apply(user: String, records: Iterable[Record]): Track = {
    val seq = records.toSeq.sortBy(_.time)
    new Track(user, seq)
  }
}