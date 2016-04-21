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

/**
 * Hold a list of of values for some parameters. A [[ParamMap]] is immutable.
 *
 * @param map Mapping between parameters and values
 */
class ParamMap private(private val map: Map[Param[_], Any]) {
  /**
   * Return the number of parameters for which we have a value.
   */
  def size: Int = map.size

  /**
   * Check whether this map is empty.
   */
  def isEmpty: Boolean = map.isEmpty

  /**
   * Check whether this map is not empty.
   */
  def nonEmpty: Boolean = map.nonEmpty

  /**
   * Return the value of a given parameter if it has been explicitly defined, or its default value.
   *
   * @param param A parameter definition
   * @tparam T Parameter type
   * @throws NoSuchElementException If there is neither an explicit or default value
   */
  @throws[NoSuchElementException]
  def apply[T](param: Param[T]): T = getOrElse(param).get

  /**
   * Check whether an explicit value has been set for a given parameter.
   *
   * @param param A parameter definition
   */
  def contains(param: Param[_]): Boolean = map.contains(param)

  /**
   * Return the value of a given parameter if it has been explicitly defined.
   *
   * @param param A parameter defintion
   * @tparam T Parameter type
   */
  def get[T](param: Param[T]): Option[T] = map.get(param).map(_.asInstanceOf[T])

  /**
   * Return the value of a given parameter if it has been explicitly defined, or its default value,
   * if available.
   *
   * @param param A parameter defintion
   * @tparam T Parameter type
   */
  def getOrElse[T](param: Param[T]): Option[T] = get(param).orElse(param.defaultValue)

  /**
   * Return the value of a given parameter if it has been explicitly defined, or its default value,
   * or a provided default value.
   *
   * @param param   A parameter definition
   * @param default A default value
   * @tparam T Parameter type
   */
  def getOrElse[T](param: Param[T], default: T): T = getOrElse(param).getOrElse(default)

  /**
   * Return a map including only parameters for the given parent object.
   *
   * @param parent A parent object
   * @return A new map
   */
  def filter(parent: Identifiable): ParamMap =
    new ParamMap(map.filter(_._1.parent.uid == parent.uid))

  /**
   * Merge this map with another map, whose values overwrite those in the current map.
   *
   * @param other Another map
   * @return A new map
   */
  def ++(other: ParamMap): ParamMap = new ParamMap(map ++ other.map)

  /**
   * Define a value for a given parameter, overwriting an existing value.
   *
   * @param param A parameter definition
   * @param value A parameter value
   * @tparam T Parameter type
   * @return A new map
   */
  def set[T](param: Param[T], value: T): ParamMap = new ParamMap(map + (param -> value))

  /**
   * Define several parameter values, overwriting any existing value.
   *
   * @param paramPairs Parameter values
   * @return A new map
   */
  def set[T](paramPairs: ParamPair[T]*): ParamMap =
    new ParamMap(map ++ paramPairs.map(p => p.param -> p.value))

  /**
   * Return this map viewed as a sequence of parameter values.
   */
  def toSeq: Seq[ParamPair[_]] = {
    map.toSeq.map { case (param, value) =>
      new ParamPair(param.asInstanceOf[Param[Any]], value)
    }.sortBy(_.param.name)
  }

  /**
   * Return a string representation for a subset of this map including only parameters for a given
   * parent object. Here the parameters names won't be prefixed with their parent's uid, since all
   * of them belong to the same parent, resulting in a more compact representation.
   *
   * @param parent A parent object
   */
  def toString(parent: Identifiable): String = {
    filter(parent).toSeq
        .map(pair => s"${pair.param.name}=${pair.value}")
        .mkString(", ")
  }

  override def equals(other: Any): Boolean = other match {
    case m: ParamMap => m.map == map
    case _ => false
  }

  override def hashCode: Int = map.hashCode

  override def toString: String = {
    val inner = toSeq.map { pair =>
      s"${pair.param.parent.uid}.${pair.param.name}=${pair.value}"
    }.mkString(", ")
    s"${getClass.getSimpleName}($inner)"
  }
}

/**
 * Factory for [[ParamMap]].
 */
object ParamMap {
  /**
   * Create an empty parameters map.
   */
  def empty: ParamMap = new ParamMap(Map.empty)

  /**
   * Create a parameter map from a list of parameters and values.
   *
   * @param paramPairs Parameters/values
   */
  def apply(paramPairs: ParamPair[_]*): ParamMap =
    new ParamMap(paramPairs.map(p => p.param -> p.value).toMap)

  /**
   * Convenience method to create a parameter map composing parameters pairs on-the-fly for a
   * given parent object.
   *
   * @param obj          A parent object
   * @param mkParamPairs Functions to create parameters pairs
   * @tparam T Parent object type
   */
  def make[T <: Identifiable](obj: T, mkParamPairs: (T => ParamPair[_])*): ParamMap =
    apply(mkParamPairs.map(_.apply(obj)): _*)
}