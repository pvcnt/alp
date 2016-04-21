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

package fr.cnrs.liris.accio.core.runtime

import java.nio.file.{Files, Paths}
import java.time.Duration

import com.typesafe.scalalogging.StrictLogging
import fr.cnrs.liris.accio.core.eval.EvalTool
import fr.cnrs.liris.accio.core.framework._
import fr.cnrs.liris.accio.core.optimizer.OptimizeTool
import fr.cnrs.liris.accio.core.report.EventWriter
import fr.cnrs.liris.privamov.ops.prepare.SplitByTemporalGap
import fr.cnrs.liris.util.flags.{Flag, FlagsProvider}
import fr.cnrs.liris.util.io.FileUtils

case class MakeCommandOpts(
    @Flag(name = "keys")
    keys: String = "",
    @Flag(name = "sample")
    proba: Double = 1,
    @Flag(name = "remote")
    remote: String = "",
    @Flag(name = "log_dir")
    logDir: String = "")

class MakeCommand extends Command[MakeCommandOpts] with StrictLogging {
  private[this] val registry = new OpRegistry
  private[this] val parser = new JsonExperimentParser(registry)

  def run(flags: FlagsProvider): Unit = {
    val opts = flags.as[MakeCommandOpts]
    val logDir = if (opts.logDir.nonEmpty) Paths.get(opts.logDir) else Files.createTempDirectory("accio-")
    val eventWriter = new EventWriter(logDir)
    flags.residue.foreach { url =>
      make(opts, url, eventWriter)
    }
    logger.info(s"Reports are available in ${logDir.toAbsolutePath}")
  }

  private def make(opts: MakeCommandOpts, url: String, eventWriter: EventWriter) = {
    logger.info(s"Make $url")

    // Load experiment definition.
    val experiment = parser.parse(FileUtils.replaceHome(url))
    logger.debug(s"Loaded experiment: $experiment")
    val evalTool = new EvalTool(experiment.evaluators, experiment.analyzers, experiment.runs)

    // Prepare source.
    var source = experiment.source
    if (experiment.preparator.isDefined) {
      source = source.transform(experiment.preparator.get)
    }
    if (opts.keys.nonEmpty) {
      logger.info(s"Restricting source dataset to ${opts.keys}")
      source = source.restrict(opts.keys.split(","))
    }
    if (opts.proba < 1) {
      logger.info(s"Sampling source dataset with probability ${opts.proba}")
      source = source.sample(opts.proba)
    }

    // First evaluate optimized treatments.
    experiment.optimizations.foreach { optim =>
      logger.info(s"Optimizing ${optim.name}")
      val optimizeTool = new OptimizeTool(experiment.evaluators, optim.objectives, optim.iters, optim.runs.getOrElse(experiment.runs))
      //val splitter = BoundTransformer(new SplitByTemporalGap)(_.duration := Duration.ofHours(4))
      /*val evaluations = source.traverse { track =>
        val tracks = splitter.transform(track).filter(_.size > 1)
        val transformer = optimizeTool.optimize(tracks, optim.treatment.explo).solution
        tracks.map(track => evalTool.evaluate(track, transformer))
      }.flatten*/
      val evaluations = evalTool.evaluate(source, track => {
        optimizeTool.optimize(track, optim.treatment.explo).solution
      })
      eventWriter.write(optim.name, evaluations)
    }

    // Then evaluate other treatments.
    experiment.treatments.foreach { treatment =>
      logger.info(s"Evaluating ${treatment.name}")
      treatment.explo.toSeq.foreach { trans =>
        val evaluations = evalTool.evaluate(source, _ => trans)
        eventWriter.write(treatment.name, evaluations)
      }
    }
  }
}