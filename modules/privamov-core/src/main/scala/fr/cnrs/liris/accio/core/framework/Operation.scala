package fr.cnrs.liris.accio.core.framework

import fr.cnrs.liris.accio.lib.param.{Identifiable, Param, ParamMap, Params}

/**
 * An operation is a common trait for all parametrizable objects. They share a common way to be
 * instantiated and used.
 *
 * All their constructor-dependencies are global dependencies that do no depend on their parameters.
 * Parameters will be defined in the constructor through their [[Params]] instance. Methods that
 * will be defined after will likely receive a [[ParamMap]] parameter containing the actual
 * parameters values, e.g., [[Operation.describe]]. This means that operations should be stateless.
 */
trait Operation extends Identifiable {
  protected[this] val param = new Params(this)

  override val uid = Identifiable.uniqid(getClass.getSimpleName)

  def params: Seq[Param[_]] = param.toSeq

  /*def inputs: Int

  def outputs: Int

  def metrics: Seq[String]

  def execute(ctx: OpCtx): OpResult*/

  def describe(paramMap: ParamMap): String = s"${getClass.getSimpleName}(${paramMap.toString(this)})"
}

/*class OpCtx(opName: String, val paramMap: ParamMap, inputs: Seq[Track]) {
  def numInputs: Int = inputs.size

  def input: Track = inputs.head

  def input(idx: Int): Track = inputs(idx)

  def apply[T](param: Param[T]): T = paramMap(param)
}

class OpResult(val outputs: Seq[Track], val metrics: Seq[Metric]) {
  def scope(name: String): OpResult = new OpResult(outputs, metrics.map(_.scope(name)))
}

object OpResult {
  def apply(output: Track, metrics: Metric*): OpResult = new OpResult(Seq(output), metrics)

  def apply(outputs: Seq[Track], metrics: Metric*): OpResult = new OpResult(outputs, metrics)
}

case class Node(name: String, op: Operation, paramMap: ParamMap) extends Named with Describable {
  def execute(inputs: Seq[Track]): OpResult = {
    val ctx = new OpCtx(name, paramMap, inputs)
    op.execute(ctx).scope(name)
  }

  override def describe: String = op.describe(paramMap)
}

case class NodeChain(name: String, ops: Seq[Operation], paramMap: ParamMap) extends Named with Describable {
  def execute(inputs: Seq[Track]): OpResult = {
    var outputs = inputs
    var metrics = Seq.empty[Metric]
    ops.foreach { op =>
      val ctx = new OpCtx(name, paramMap, outputs)
      val res = op.execute(ctx)
      metrics ++= res.metrics
      outputs = res.outputs
    }
    new OpResult(outputs, metrics.map(_.scope(name)))
  }

  override def describe: String = ops.map(_.describe(paramMap)).mkString("|")
}*/