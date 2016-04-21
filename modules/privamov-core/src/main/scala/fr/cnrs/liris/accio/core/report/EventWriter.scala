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

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}

import com.twitter.util.Time
import com.typesafe.scalalogging.StrictLogging
import fr.cnrs.liris.accio.core.eval.Report
import fr.cnrs.liris.accio.core.framework.{BoundTransformer, Metric}
import fr.cnrs.liris.accio.core.thrift
import fr.cnrs.liris.accio.lib.param.ParamMap
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TIOStreamTransport

/**
 * Save various information about the execution output in a serialized manner. This data can be
 * read after with an [[EventReader]].
 *
 * @param logDir A directory where to dump data
 */
class EventWriter(logDir: Path) extends StrictLogging {
  if (!logDir.toFile.exists()) {
    Files.createDirectories(logDir)
  }
  logger.info(s"Logging events to ${logDir.toAbsolutePath}")
  private[this] val protocolFactory = new TBinaryProtocol.Factory

  def write(name: String, reports: Seq[Report]): Unit = {
    val solutionsIndex = reports.map(_.transformer).distinct.zipWithIndex.toMap
    val usersIndex = reports.map(_.user).distinct.zipWithIndex.toMap
    val items = reports.map { report =>
      thrift.ReportItem(encode(report.insights), encode(report.metrics), solutionsIndex(report.transformer), usersIndex(report.user))
    }
    val groupedReports = reports.groupBy(report => solutionsIndex(report.transformer))
    val durations = groupedReports.map { case (idx, values) => idx -> values.flatMap(_.duration.map(_.inNanoseconds)) }
    val costs = groupedReports.map { case (idx, values) => idx -> values.flatMap(_.cost) }
    val solutions = solutionsIndex.toSeq.sortBy(_._2).map { case (sol, idx) =>
      thrift.Solution(
        encode(sol),
        if (costs.nonEmpty) Some(costs(idx)) else None,
        if (durations.nonEmpty) Some(durations(idx)) else None)
    }
    write(thrift.Report(
      Time.now.inNanoseconds,
      name,
      solutions,
      usersIndex.toSeq.sortBy(_._2).map(_._1),
      items))
  }

  private def encode(metrics: Seq[Metric]): Seq[thrift.Metric] = {
    val duplicateMetrics = metrics.groupBy(_.name).filter(_._2.size > 1)
    if (duplicateMetrics.nonEmpty) {
      throw new RuntimeException(s"Duplicate metrics: ${duplicateMetrics.keys.mkString(", ")}")
    }
    metrics.map(metric => thrift.Metric(metric.name, metric.value))
  }

  private def encode(transformation: BoundTransformer): thrift.Transformation = {
    val ops = transformation.ops.map(_.getClass.getSimpleName)
    val uids = transformation.ops.map(_.uid)
    thrift.Transformation(ops, uids, encode(transformation.paramMap))
  }

  private def encode(paramMap: ParamMap): Seq[thrift.ParamValue] = {
    paramMap.toSeq.map { paramPair =>
      val value = paramPair.value match {
        case i: Int => thrift.Value(intValue = Some(i))
        case s: Short => thrift.Value(intValue = Some(s.toInt))
        case b: Byte => thrift.Value(intValue = Some(b.toInt))
        case l: Long => thrift.Value(longValue = Some(l))
        case d: Double => thrift.Value(doubleValue = Some(d))
        case f: Float => thrift.Value(doubleValue = Some(f.toDouble))
        case b: Boolean => thrift.Value(boolValue = Some(b))
        case v => thrift.Value(stringValue = Some(v.toString))
      }
      thrift.ParamValue(paramPair.param.name, paramPair.param.parent.uid, value)
    }
  }

  /**
   * Write a single Thrift object to the log directory. Each object will be written in a new file,
   * whose name is incrementing. This method is thread-safe.
   *
   * @param report An encoded report
   */
  private def write(report: thrift.Report) = {
    val baos = new ByteArrayOutputStream
    report.write(protocolFactory.getProtocol(new TIOStreamTransport(baos)))
    val bytes = baos.toByteArray

    val file = synchronized {
      var suffix = 0
      var file = logDir.resolve(s"report-$suffix")
      while (file.toFile.exists()) {
        suffix += 1
        file = logDir.resolve(s"report-$suffix")
      }
      file
    }
    Files.write(file, bytes)
    logger.info(s"Written report to ${file.toAbsolutePath} (${bytes.length} bytes)")
  }
}