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

import java.util.Objects

import scala.reflect.runtime.universe._

/**
 * A parameter definition specifies a name, short description and optionally a default value for a
 * parameter. Each definition is bound to a particular instance of an [[Identifiable]] object.
 * Parameters definitions should only be created from [[Params]] methods.
 *
 * A [[Param]] is immutable and never holds its actual value (see [[ParamPair]] for that).
 *
 * @param parent       A parent object
 * @param name         A name
 * @param description  A short description
 * @param defaultValue A default value
 * @tparam T Parameter type
 * @throws IllegalArgumentException If `name` is not a valid parameter name
 */
class Param[T: TypeTag] private[param](
    val parent: Identifiable,
    val name: String,
    val description: String,
    val defaultValue: Option[T]) {
  Param.validateName(name)

  /**
   * Return the type tag associated with acceptable values for this parameter.
   */
  def valueTypeTag: TypeTag[T] = implicitly[TypeTag[T]]

  /**
   * Associate this parameter with a value to create a parameter pair.
   *
   * @param value A single value
   */
  def :=(value: T): ParamPair[T] = ParamPair(this, value)

  /**
   * Associate this parameter with a domain of values it can take.
   *
   * @param domain A domain of values
   */
  def ~=(domain: Domain[T]): ParamDomain[T] = ParamDomain(this, domain)

  override def equals(other: Any): Boolean = other match {
    case p: Param[_] => p.parent.uid == parent.uid && p.name == name
    case _ => false
  }

  override def hashCode: Int = Objects.hash(parent.uid, name)

  override def toString: String = s"${parent.uid}.$name"
}

/**
 * Helpers for [[Param]].
 */
object Param {
  /**
   * Pattern that parameters names must respect.
   */
  val NamePattern = "[a-zA-Z][a-zA-Z0-9_-]*"

  /**
   * Create the regex for [[NamePattern]] only once.
   */
  private[this] val nameRegex = s"^${Param.NamePattern}$$".r

  /**
   * Validate that a string is a valid parameter name.
   *
   * @param str A string
   * @throws IllegalArgumentException If the string is not a valid parameter name
   */
  @throws[IllegalArgumentException]
  def validateName(str: String): Unit = {
    require(nameRegex.findFirstIn(str).isDefined, s"Invalid parameter name (got '$str')")
  }
}

/**
 * A parameter definition associated with its value.
 *
 * @param param A parameter definition
 * @param value A value
 * @tparam T Parameter type
 */
case class ParamPair[T](param: Param[T], value: T)

/**
 * A parameter definition associated with a domain of values it can take.
 *
 * @param param  A parameter definition
 * @param domain A domain of values
 * @tparam T Parameter type
 */
case class ParamDomain[T](param: Param[T], domain: Domain[T])