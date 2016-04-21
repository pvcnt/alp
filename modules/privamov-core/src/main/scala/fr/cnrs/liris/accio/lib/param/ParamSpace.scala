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

/**
 *
 * @param grid
 */
class ParamSpace private(private val grid: Map[Param[_], Domain[_]]) {
  def contains(param: Param[_]): Boolean = grid.contains(param)

  def explore(param: Param[Boolean]): ParamSpace =
    explore(param ~= Domain(Seq(true, false)))

  def explore[T](param: Param[T], domain: Domain[T]): ParamSpace = explore(param ~= domain)

  def explore[T](paramDomains: ParamDomain[_]*): ParamSpace =
    new ParamSpace(grid ++ paramDomains.map(p => p.param -> p.domain))

  def baseOn(paramMap: ParamMap): ParamSpace = baseOn(paramMap.toSeq: _*)

  def baseOn(paramPairs: ParamPair[_]*): ParamSpace =
    new ParamSpace(grid ++ paramPairs.map(p => p.param -> Domain.value(p.value)))

  /**
   * Return a parameters space including only parameters for the given parent object.
   *
   * @param parent A parent object
   * @return A new parameters space
   */
  def filter(parent: Identifiable): ParamSpace =
    new ParamSpace(grid.filter { case (param, values) => param.parent.uid == parent.uid })

  def ++(other: ParamSpace): ParamSpace = new ParamSpace(grid ++ other.grid)

  def toSeq: Seq[ParamMap] = {
    var paramMaps = Seq(ParamMap.empty)
    grid.foreach { case (param, domain) =>
      paramMaps = domain.values.flatMap { v =>
        paramMaps.map(_.set(param.asInstanceOf[Param[Any]], v))
      }
    }
    paramMaps
  }

  /**
   * Draw a random parameters map from this parameters space.
   *
   * @return A new random parameters map
   */
  def random(): ParamMap = {
    val paramPairs = grid.map { case (param, domain) =>
      ParamPair(param.asInstanceOf[Param[Any]], domain.random())
    }.toSeq
    ParamMap(paramPairs: _*)
  }

  override def toString: String =
    MoreObjects.toStringHelper(this)
        .addValue(grid.map { case (param, domain) => s"${param.name}=$domain" }.mkString(", "))
        .toString

  /**
   * Compute a neighboring parameters map w.r.t. this parameters space. It randomly selects one
   * parameter from the parameters map defined into this parameters space and replace its value
   * with a neighboring one. It means that at most one parameter will be modified inside the
   * parameters map. Parameters defined over singleton domains are never eligible.
   *
   * @param paramMap A parameters map
   * @return A new neighboring parameters map
   */
  def neighbor(paramMap: ParamMap): ParamMap = {
    val eligibleParams = paramMap.toSeq.map(_.param).intersect(grid.keys.toSeq).filterNot(k => grid(k).isSingleton)
    if (eligibleParams.isEmpty) {
      paramMap
    } else {
      val mutatedParamPair = neighbor(RandomUtils.randomElement(eligibleParams), paramMap)
      paramMap.set(mutatedParamPair)
    }
  }

  private def neighbor[T](param: Param[T], paramMap: ParamMap) =
    ParamPair(param, grid(param).asInstanceOf[Domain[T]].neighbor(paramMap(param)))
}

/**
 * Factory for [[ParamSpace]].
 */
object ParamSpace {
  /**
   * Create an empty parameters space.
   */
  def empty: ParamSpace = new ParamSpace(Map.empty)

  /**
   * Create a parameters space from a list of parameters and domains.
   *
   * @param paramDomains Parameters/domains
   */
  def apply(paramDomains: ParamDomain[_]*): ParamSpace = empty.explore(paramDomains: _*)

  /**
   * Create a parameters space based on a parameters map.
   *
   * @param paramMap A parameters map
   */
  def apply(paramMap: ParamMap): ParamSpace = empty.baseOn(paramMap)
}