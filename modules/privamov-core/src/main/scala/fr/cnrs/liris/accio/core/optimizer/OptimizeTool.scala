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

package fr.cnrs.liris.accio.core.optimizer

import com.twitter.util.{Duration, Stopwatch}
import com.typesafe.scalalogging.LazyLogging
import fr.cnrs.liris.accio.core.eval.EvalTool
import fr.cnrs.liris.accio.core.framework._
import fr.cnrs.liris.accio.lib.param.ParamMap
import fr.cnrs.liris.privamov.model.Track

/**
 * The optimizer's goal is to generate an optimal treatment for input tracks w.r.t. some evaluations and objectives.
 *
 * @param evaluators A set of evaluators
 * @param objectives A set of objectives
 * @param iters      Number of iterations at each step
 * @param runs       Number of runs to aggregate when computing the cost
 */
class OptimizeTool(evaluators: Set[BoundEvaluator], objectives: Set[Objective], iters: Int = 1, runs: Int = 1) extends LazyLogging {
  // We only keep relevant evaluators, that are actually used by at least one objective.
  private[this] val evalTool = new EvalTool(
    evaluators.filter(ev => objectives.exists(_.metric.startsWith(s"${ev.name}/"))),
    Set.empty,
    runs)

  def optimize(reference: Track, op: Exploration): OptimizerResult = optimize(Seq(reference), op)

  /**
   * Run the optimizer on a single track for a given treatment and parameters space.
   *
   * @param reference A reference track
   * @param op        An exploration
   * @return An optimal solution
   */
  def optimize(reference: Seq[Track], op: Exploration): OptimizerResult = {
    val watch = Stopwatch.start()
    val system = new SystemInstance(reference, op)
    val res = if (objectives.isEmpty) {
      // If we have no objective, there is no cost to compute, we return directly a random solution.
      logger.warn("No objective has been set, this means no optimization to perform")
      AnnealingResult(system.initialSolution, 0)
    } else {
      // With some objective, it is worth running the simulated annealing.
      val annealing = new SimulatedAnnealing(system, CoolingSchedule(), iters)
      annealing.run()
    }
    OptimizerResult(BoundTransformer(op.ops, res.value), res.cost, watch())
  }

  /**
   * A simulated annealing system trying to find an optimal [[ParamMap]] given a transformer
   * and a reference dataset.
   *
   * @param reference Reference dataset
   * @param explo     Transformation exploration
   */
  private class SystemInstance(reference: Seq[Track], explo: Exploration) extends AnnealingSystem[ParamMap] {
    override val initialSolution = explo.paramSpace.random()

    override def neighbor(solution: ParamMap): ParamMap = explo.paramSpace.neighbor(solution)

    override def cost(solution: ParamMap): Double = {
      val transformer = BoundTransformer(explo.ops, solution)
      val costs = reference.map { track =>
        val metrics = evalTool.evaluate(track, transformer).metrics
        val indexedMetrics = metrics.map(metric => metric.name -> metric).toMap
        objectives.map(o => getCost(o, indexedMetrics(o.metric))).sum
      }
      costs.sum
    }

    override def acceptanceProbability(oldCost: Double, newCost: Double, temp: Double): Double = {
      if (oldCost == 0) {
        0
      } else if (newCost < oldCost) {
        1
      } else {
        1d / (1 + math.exp((newCost - oldCost) / (0.5 * temp * objectives.size)))
      }
    }

    private def getCost(objective: Objective, metric: Metric): Double = {
      val left = metric.value
      val cost = objective match {
        case Objective.EqualTo(_, right) => math.abs(left - right) / right
        case Objective.MoreThan(_, right) => if (left >= right) 0d else (right - left) / right
        case Objective.LessThan(_, right) => if (left <= right) 0d else (left - right) / right
        case o: Objective.Maximize =>
          // We should maximize, which means the lower the value, the higher the cost.
          1 - o.scale(left)
        case o: Objective.Minimize =>
          // We should minimize, which means the higher the value, the higher the cost.
          o.scale(left)
      }
      logger.debug(s"Cost of $objective: $cost (metric: $left)")
      cost
    }
  }

}

/**
 * A solution determined by the optimizer.
 *
 * @param solution An optimal transformation
 * @param cost     Cost of this solution
 * @param duration Time spent to find this solution
 */
case class OptimizerResult(solution: BoundTransformer, cost: Double, duration: Duration)