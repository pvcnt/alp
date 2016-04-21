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

package fr.cnrs.liris.accio.lib.param

import com.google.common.base.MoreObjects
import fr.cnrs.liris.util.random.RandomUtils

trait Domain[T] {
  /**
   * Return all values composing this domain.
   */
  def values: Seq[T]

  def neighbor(value: T): T

  /**
   * Return a random value drawn from this domain.
   */
  def random(): T = RandomUtils.randomElement(values)

  /**
   * Check if this domain is a singleton, i.e., it represents a single possible value.
   *
   * @return True if it is a singleton, false otherwise
   */
  def isSingleton: Boolean = values.size == 1

  def size: Int = values.size
}

object Domain {
  val DefaultContraction = 0.5

  def value[T](value: T): Domain[T] = new ListDomain(Seq(value), DefaultContraction)

  def apply[T](values: Iterable[T]): Domain[T] =
    new ListDomain(values.toSeq.distinct, DefaultContraction)

  def apply[T](value: T, contraction: Double): Domain[T] =
    new ListDomain(Seq(value), contraction)

  def apply[T](values: Iterable[T], contraction: Double): Domain[T] =
    new ListDomain(values.toSeq.distinct, contraction)
}

private class ListDomain[T](override val values: Seq[T], contraction: Double) extends Domain[T] {
  require(values.nonEmpty, "A domain cannot be empty")
  require(contraction > 0 && contraction <= 1, s"contraction must be in (0,1] (got $contraction)")

  override def neighbor(value: T): T = {
    val pos = values.indexOf(value)
    require(pos > -1, s"Value $value is not part of $this")
    if (values.size == 1) {
      value
    } else {
      var size = math.min(values.size, math.ceil(values.size * contraction).toInt)
      if ((size % 2) == 0) {
        size += 1
      }
      val middle = (size - 1) / 2
      //TODO: very very innefficient.
      val neighborhood = values.sliding(size).filter(_.contains(value)).toSeq.sortBy(w => math.abs(middle - w.indexOf(value))).head.filterNot(_ == value)
      val el = RandomUtils.randomElement(neighborhood)
      //println(s"--- restricted around $value to ${neighborhood.head}-${neighborhood.last}: $el")
      el
    }
  }

  override def toString: String =
    MoreObjects.toStringHelper(this)
        .add("type", values.head.getClass.getSimpleName)
        .add("size", values.size)
        .toString
}