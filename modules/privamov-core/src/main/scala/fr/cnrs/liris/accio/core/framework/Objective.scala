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

package fr.cnrs.liris.accio.core.framework

/**
 * An optimization objective, working on a specific metric. It is translated into a cost by the
 * [[fr.cnrs.liris.accio.core.optimizer.OptimizeTool]].
 */
sealed trait Objective {
  /**
   * Metric this objective is concerned with.
   *
   * @return A metric name
   */
  def metric: String
}

/**
 * Factory for [[Objective]].
 */
object Objective {

  /**
   * Create an objective whose goal is to have a metric equal a target value.
   *
   * @param metric A metric name
   * @param value  Target value
   */
  case class EqualTo(metric: String, value: Double) extends Objective {
    override def toString: String = s"Compare($metric == $value)"
  }

  /**
   * Create an objective whose goal is to have a metric lower than a target value.
   *
   * @param metric A metric name
   * @param value  Maximum value
   */
  case class LessThan(metric: String, value: Double) extends Objective {
    override def toString: String = s"Compare($metric <= $value)"
  }

  /**
   * Create an objective whose goal is to have a metric greater than a target value.
   *
   * @param metric A metric name
   * @param value  Minimum value
   */
  case class MoreThan(metric: String, value: Double) extends Objective {
    override def toString: String = s"Compare($metric >= $value)"
  }

  /**
   * Create an objective whose goal is to minimize a metric.
   *
   * @param metric A metric name
   */
  case class Minimize(metric: String, maxValue: Option[Double]) extends Objective {
    override def toString: String = maxValue match {
      case Some(v) => s"Minimize($metric,$v)"
      case None => s"Minimize($metric)"
    }

    def scale(value: Double): Double = maxValue.map(v => math.min(value, v) / v).getOrElse(value)
  }

  /**
   * Create an objective whose goal is to maximize a metric.
   *
   * @param metric A metric name
   */
  case class Maximize(metric: String, maxValue: Option[Double]) extends Objective {
    override def toString: String = maxValue match {
      case Some(v) => s"Maximize($metric,$v)"
      case None => s"Maximize($metric)"
    }

    def scale(value: Double): Double = maxValue.map(v => math.min(value, v) / v).getOrElse(value)
  }

  /**
   * Parse a string, similar to one produced by [[Objective.toString]] into an objective.
   *
   * @param str A string
   * @throws IllegalArgumentException If the string is invalid
   */
  @throws[IllegalArgumentException]
  def parse(str: String): Objective = {
    val trimmed = str.trim
    val matched = recognizeFunction(trimmed, "Minimize")
        .orElse(recognizeFunction(trimmed, "Maximize"))
        .orElse(recognizeFunction(trimmed, "Compare"))
    matched match {
      case Some(("Minimize", arg)) =>
        val (metric, maxValue) = recognizeScale(arg)
        Minimize(metric, maxValue)
      case Some(("Maximize", arg)) =>
        val (metric, maxValue) = recognizeScale(arg)
        Maximize(metric, maxValue)
      case Some(("Compare", arg)) =>
        val comparison = trimmed.drop(8).dropRight(1)
        val it = "^(.+)\\s*(==|<=|>=)\\s*([0-9]+(?:\\.[0-9]*)?)\\s*$".r.findAllIn(comparison).matchData
        if (!it.hasNext) {
          throw new IllegalArgumentException(s"Invalid comparison format: $comparison")
        }
        val matches = it.next
        matches.group(2) match {
          case "==" => EqualTo(matches.group(1).trim, matches.group(3).toDouble)
          case "<=" => LessThan(matches.group(1).trim, matches.group(3).toDouble)
          case ">=" => MoreThan(matches.group(1).trim, matches.group(3).toDouble)
        }
      case _ => throw new IllegalArgumentException(s"Invalid objective format: $str")
    }
  }

  private def maxNormalizer(maxValue: Double): (Double => Double) = {
    (value: Double) => math.min(value, maxValue) / maxValue
  }

  private def recognizeScale(str: String) = {
    val pos = str.indexOf(",")
    if (pos > -1) {
      (str.substring(0, pos).trim, Some(str.substring(pos + 1).trim.toDouble))
    } else {
      (str.trim, None)
    }
  }

  private def recognizeFunction(str: String, fn: String) = {
    val prefix = s"$fn("
    if (str.startsWith(prefix) && str.endsWith(")")) {
      Some(fn -> str.drop(prefix.length).dropRight(1).trim)
    } else {
      None
    }
  }
}