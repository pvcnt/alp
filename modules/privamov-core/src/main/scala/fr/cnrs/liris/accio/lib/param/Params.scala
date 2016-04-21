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

import scala.collection.mutable
import scala.reflect.runtime.universe._

/**
 * A list of parameters definition bound to some parent object. This list *is* mutable and intended
 * to be embedded inside objects accepting to be parametrized. As with [[Param]], it does not hold
 * actual values parameters (see [[ParamMap]] for that).
 *
 * @param parent A parent object
 */
class Params(val parent: Identifiable) {
  private[this] val params = mutable.Map.empty[String, Param[_]]

  /**
   * Create a new parameter definition with a default value and register it.
   *
   * @param name        A name
   * @param description A description
   * @param default     A default value
   * @tparam T Parameter type
   * @return A new parameter
   */
  def create[T: TypeTag](name: String, description: String, default: T): Param[T] =
    add(new Param[T](parent, name, description, Some(default)))

  /**
   * Create a new parameter definition with a no default value and register it.
   *
   * @param name        A name
   * @param description A description
   * @tparam T Parameter type
   * @return A new parameter
   */
  def create[T: TypeTag](name: String, description: String): Param[T] =
    add(new Param[T](parent, name, description, None))

  /**
   * Return the parameter definition for the given name.
   *
   * @param name A parameter name
   * @throws NoSuchElementException If no parameter for the given name exists
   */
  @throws[NoSuchElementException]
  def apply(name: String): Param[_] = params(name)

  /**
   * Check whether a parameter definition exists for the given name.
   *
   * @param name A parameter name
   */
  def contains(name: String): Boolean = params.contains(name)

  /**
   * Check whether a given parameter belongs is defined in this instance.
   *
   * @param param A parameter definition
   */
  def contains(param: Param[_]): Boolean = params.get(param.name).exists(_.name == param)

  /**
   * Return this instance viewed as a sequence of parameter definitions, ordered by their name.
   */
  def toSeq: Seq[Param[_]] = params.toSeq.sortBy(_._1).map(_._2)

  private def add[T](param: Param[T]): Param[T] = {
    require(!params.contains(param.name), s"Duplicate parameter name '${param.name}'")
    // As this is a private method, we do not validate the uid which should be safe.
    params(param.name) = param
    param
  }
}