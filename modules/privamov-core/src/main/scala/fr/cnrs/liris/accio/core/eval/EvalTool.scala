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

package fr.cnrs.liris.accio.core.eval

import java.util.concurrent.atomic.AtomicInteger

import breeze.stats.DescriptiveStats._
import com.twitter.util.{Duration, Stopwatch}
import com.typesafe.scalalogging.StrictLogging
import fr.cnrs.liris.accio.core.dataset.Dataset
import fr.cnrs.liris.accio.core.framework._
import fr.cnrs.liris.accio.lib.profiler.AutoProfiler.profile
import fr.cnrs.liris.accio.lib.profiler.ProfilerTask
import fr.cnrs.liris.privamov.model.Track

case class Report(
    user: String,
    insights: Seq[Metric],
    metrics: Seq[Metric],
    transformer: BoundTransformer,
    cost: Option[Double] = None,
    duration: Option[Duration] = None)

/**
 * Evaluation tool, whose purpose is to evaluate a transformation by comparing the reference track
 * and the trace resulting from this transformation, w.r.t. to some evaluators.
 *
 * @param evaluators Set of evaluators to run on tracks
 * @param analyzers  Set of analyers to run on tracks
 * @param runs       Number of runs to aggregate
 */
class EvalTool(evaluators: Set[BoundEvaluator], analyzers: Set[BoundAnalyzer], runs: Int = 1) extends StrictLogging {
  /**
   * Evaluate a dataset against fixed transformation for all tracks. It evaluates each track
   * independently and return the concatenation of all metrics, in the order of the source tracks.
   *
   * @param source      Source dataset
   * @param transformer Transformer
   * @return A list of unaggregated metrics
   */
  def evaluate(source: Dataset[Track], transformer: BoundTransformer): Seq[Report] =
    evaluate(source, _ => transformer)

  /**
   * Evaluate a dataset against a transformation depending on the input track and computed
   * on-the-fly. It evaluates each track independently and return the concatenation of all metrics,
   * in the order of the source tracks.
   *
   * @param source        Source dataset
   * @param mkTransformer Transformer generator, called for each track in `source`
   * @return A list of unaggregated metrics
   */
  def evaluate(source: Dataset[Track], mkTransformer: Track => BoundTransformer): Seq[Report] = {
    val i = new AtomicInteger(0)
    val size = source.count()
    source.traverse { track =>
      val w = Stopwatch.start()
      val transformation = mkTransformer(track)
      val report = evaluate(track, transformation, Some(w()))
      logger.info(s"Evaluated track ${i.incrementAndGet()}/$size with ${transformation.describe}")
      report
    }
  }

  /**
   * Evaluate a single track against a transformation.
   *
   * @param reference   A track
   * @param transformer A transformer
   * @return A list of metrics
   */
  def evaluate(reference: Track, transformer: BoundTransformer): Report =
    evaluate(reference, transformer, None)

  private def evaluate(reference: Track, transformer: BoundTransformer, duration: Option[Duration]): Report = {
    val metrics = Seq.fill(runs) {
      val results = transformer.transform(reference)
      require(results.size == 1, s"Output of ${transformer.describe} being evaluated must be a singleton (got ${results.size})")
      evaluators.flatMap(eval => profile(eval, ProfilerTask.Evaluation) {
        eval.evaluate(reference, results.head)
      }).toSeq
    }.flatten
    val insights = analyzers.flatMap(analyzer => profile(analyzer, ProfilerTask.Analyze) {
      analyzer.analyze(reference)
    }).toSeq
    Report(reference.user, insights, aggregate(metrics), transformer, duration = duration)
  }

  private def aggregate(metrics: Seq[Metric]) = {
    metrics
        .groupBy(_.name)
        .map { case (name, group) => Metric(name, percentile(group.map(_.value), .5)) }
        .toSeq
  }
}