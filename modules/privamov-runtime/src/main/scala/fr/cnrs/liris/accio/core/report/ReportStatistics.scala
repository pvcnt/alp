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

package fr.cnrs.liris.accio.core.report

import com.twitter.util.Time
import fr.cnrs.liris.accio.core.thrift
import fr.cnrs.liris.accio.lib.stats.{AggregatedStats, Distribution}
import fr.cnrs.liris.util.MathUtils.roundAt6
import fr.cnrs.liris.util.{Distance, TextUtils}

/**
 * This class exposes various statistics about serialized reports.
 *
 * @param serReports Serialized reports
 */
class ReportStatistics(serReports: Seq[thrift.Report]) {
  //println(serReports.map(r => r.name + ":" + r.users.mkString(",")))
  /*serReports.filter(_.name.startsWith("dynamic")).foreach { report =>
      report.items.foreach { item =>
        val eps = report.solutions(item.solutionIndex).transformation.params.find(_.name == "epsilon").get.value.doubleValue.get
        println(s"epsilon=$eps")
        item.evaluations.foreach { metric =>
          println("  " + metric.name + "=" + metric.value)
        }
        item.analyses.foreach { metric =>
          println("  " + metric.name + "=" + metric.value)
        }
        println()
      }
  }*/

  /**
   * Return the reports.
   */
  val reports: Seq[Report] = serReports.map(new Report(_))

  /**
   * Return all available evaluation metrics. They are not necessarily defined for all reports.
   */
  val evaluations: Seq[String] = serReports.flatMap(_.items).flatMap(_.evaluations).map(_.name).distinct.sorted

  /**
   * Return all available analyses metrics. They are not necessarily defined for all reports.
   */
  val analyses: Seq[String] = serReports.flatMap(_.items).flatMap(_.analyses).map(_.name).distinct.sorted

  /**
   * Return the available metrics, grouped by prefix (before the first "/").
   */
  def metricsByPrefix: Map[String, Seq[String]] = {
    evaluations.groupBy { name =>
      val pos = name.indexOf("/")
      if (pos > -1) name.substring(0, pos) else name
    }
  }

  def hasOptimization: Boolean = reports.exists(_.isOptimization)

  private def describe(trans: thrift.Transformation, truncate: Boolean): String = {
    require(trans.ops.nonEmpty)
    trans.ops.zipWithIndex.map { case (op, idx) =>
      val uid = trans.uids(idx)
      val inner = trans.params.filter(_.parent == uid).map { pval =>
        val name = if (truncate) TextUtils.truncate(pval.name, 5) else pval.name
        val value = describe(pval.value, truncate)
        s"$name=$value"
      }.mkString(", ")
      val name = if (truncate) TextUtils.truncate(op, 10) else op
      s"$name($inner)"
    }.mkString("|")
  }

  private def describe(value: thrift.Value, truncate: Boolean): String = {
    if (value.boolValue.isDefined) {
      value.boolValue.get.toString
    } else if (value.longValue.isDefined) {
      value.longValue.get.toString
    } else if (value.intValue.isDefined) {
      value.intValue.get.toString
    } else if (value.doubleValue.isDefined) {
      roundAt6(value.doubleValue.get).toString
    } else if (value.stringValue.isDefined) {
      if (truncate) TextUtils.truncate(value.stringValue.get, 15) else value.stringValue.get
    } else {
      "<?>"
    }
  }

  private def asDouble(value: thrift.Value): Option[Double] = {
    value.intValue.map(_.toDouble)
        .orElse(value.longValue.map(_.toDouble))
        .orElse(value.doubleValue)
        .orElse(value.stringValue.filter(_.endsWith(".meters")).map(Distance.parse(_).meters))
  }

  private def asString(value: thrift.Value): Option[String] = {
    value.boolValue.map(_.toString).orElse(value.stringValue)
  }

  class Report(ser: thrift.Report) {
    //println(ser.users.mkString(","))
    println(toString)
    val s = AggregatedStats(ser.solutions.flatMap(_.optimizationDuration.getOrElse(Seq.empty[Long])).map(_.toDouble))
    println(s.min)
    println(s.max)
    println(s.avg)
    println(s.p50)
    //println(ser.users.mkString(","))
    //println(ser.items.filter(_.userIndex == ser.users.indexOf("013")).map(_.evaluations.find(_.name == "privacy/fscore").map(_.value).getOrElse(-1)).mkString(","))
    //println(ser.items.filter(_.userIndex == ser.users.indexOf("013")).map(_.evaluations.find(_.name == "utility/fscore").map(_.value).getOrElse(-1)).mkString(","))
    /*println(
      ser.items.filter(_.userIndex == ser.users.indexOf("013"))
          .map(_.solutionIndex)
          .map(ser.solutions.apply)
          .map(_.transformation.params.find(_.name == "epsilon").map(_.value.stringValue.get.dropRight(7)).getOrElse("?"))
          .mkString(","))*/

    def wallTime: Time = Time.fromNanoseconds(ser.wallTime)

    def isOptimization: Boolean = ser.solutions.size > 1

    def evaluations: Seq[String] = ser.items.flatMap(_.evaluations).map(_.name).distinct.sorted

    def analyses: Seq[String] = ser.items.flatMap(_.analyses).map(_.name).distinct.sorted

    def stats(metric: String): Option[AggregatedStats] = {
      val values = ser.items.flatMap(_.evaluations.find(_.name == metric)).map(_.value)
      if (values.nonEmpty) Some(AggregatedStats(values)) else None
    }

    def params: Seq[Param] = {
      ser.solutions
          .flatMap(_.transformation.params)
          .map { p =>
            val isDouble = ser.solutions
                .flatMap(_.transformation.params.filter(p2 => p.parent == p2.parent && p.name == p2.name))
                .headOption
                .flatMap(v => asDouble(v.value))
                .isDefined
            if (isDouble) {
              new DoubleParam(p.parent, p.name)
            } else {
              new StringParam(p.parent, p.name)
            }
          }
          .distinct
    }

    /*private lazy val weakestItems = {
      val dynaReport = serReports.find(_.name == "dynamic-geoind").get
      val allEpsilons = dynaReport.solutions.flatMap(_.transformation.params.filter(_.name == "epsilon").map(_.value.doubleValue.get)).sorted
      //val minEpsilon = allEpsilons((0.9 * allEpsilons.size).floor.toInt)
      //println(s"min epsilon: $minEpsilon")
      val maxEpsilon = allEpsilons((0.1 * allEpsilons.size).floor.toInt)
      println(s"max epsilon: $maxEpsilon")
      val weakestSolutions = dynaReport.solutions.zipWithIndex.filter { case (sol, idx) =>
        sol.transformation.params.filter(_.name == "epsilon").exists(v => v.value.doubleValue.get <= maxEpsilon)
      }.map(_._2)
      dynaReport.items.zipWithIndex.filter { case (item, idx) => weakestSolutions.contains(item.solutionIndex) }.map(_._2)
    }*/

    def evalDistribution(metric: String): Distribution[Double] = {
      //val values = ser.items.zipWithIndex.filter { case (item, idx) => weakestItems.contains(idx) }
      //.map(_._1).flatMap(_.evaluations).filter(_.name == metric).map(_.value)
      val values = ser.items.flatMap(_.evaluations).filter(_.name == metric).map(_.value)
      Distribution(values)
    }

    def analysisDistribution(metric: String): Distribution[Double] = {
      val values = ser.items.flatMap(_.analyses).filter(_.name == metric).map(_.value)
      Distribution(values)
    }

    def distribution(param: DoubleParam): Distribution[Double] = {
      ser.items
          .groupBy(_.solutionIndex)
          .map { case (solIndex, items) =>
            getValues(param, ser.solutions(solIndex).transformation) * items.size
          }.foldLeft(Distribution.empty[Double])(_ ++ _)
    }

    def distribution(param: StringParam): Distribution[String] = {
      ser.items
          .groupBy(_.solutionIndex)
          .map { case (solIndex, items) =>
            getValues(param, ser.solutions(solIndex).transformation) * items.size
          }.foldLeft(Distribution.empty[String])(_ ++ _)
    }

    def distributionByUser(param: DoubleParam): Map[String, Distribution[Double]] = {
      ser.items
          .groupBy(_.userIndex)
          .map { case (userIndex, items) =>
            val dist = items
                .map(item => getValues(param, ser.solutions(item.solutionIndex).transformation))
                .foldLeft(Distribution.empty[Double])(_ ++ _)
            ser.users(userIndex) -> dist
          }
    }

    def correlation(eval1: String, eval2: String): Seq[(Double, Double)] = {
      ser.items.flatMap { item =>
        val x = item.evaluations.find(_.name == eval1)
        val y = item.evaluations.find(_.name == eval2)
        if (x.isDefined && y.isDefined) {
          Some((x.get.value, y.get.value))
        } else {
          None
        }
      }
    }

    def correlation2(param: String, analysis: String): Seq[(Double, Double)] = {
      ser.items.flatMap { item =>
        val x = ser.solutions(item.solutionIndex).transformation.params.find(_.name == param).map(_.value.doubleValue.get)
        val y = item.analyses.find(_.name == analysis).map(_.value)
        if (x.isDefined && y.isDefined) {
          Some((x.get, y.get))
        } else {
          None
        }
      }
    }

    private def getValues(param: DoubleParam, transformation: thrift.Transformation) = {
      val values = getParams(transformation, param).flatMap(p => asDouble(p.value))
      Distribution(values)
    }

    private def getValues(param: StringParam, transformation: thrift.Transformation) = {
      val values = getParams(transformation, param).flatMap(p => asString(p.value))
      Distribution(values)
    }

    private def getParams(transformation: thrift.Transformation, param: Param) =
      transformation.params.filter(p => p.parent == param.parent && p.name == param.name)

    def toString(truncate: Boolean): String = {
      if (ser.solutions.size == 1) {
        describe(ser.solutions.head.transformation, truncate)
      } else {
        ser.name
      }
    }

    override def toString: String = toString(truncate = false)
  }

}

sealed trait Param {
  def parent: String

  def name: String
}

case class StringParam(parent: String, name: String) extends Param

case class DoubleParam(parent: String, name: String) extends Param