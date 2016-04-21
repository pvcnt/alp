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

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import fr.cnrs.liris.accio.core.dataset.Dataset
import fr.cnrs.liris.accio.lib.param._
import fr.cnrs.liris.privamov.lib.io.codec.CsvTrackSource
import fr.cnrs.liris.util.Distance
import fr.cnrs.liris.util.flags.Converter
import fr.cnrs.liris.util.io.{FileUtils, Getter}

import scala.collection.JavaConverters._
import scala.reflect._
import scala.reflect.runtime.universe._

/**
 * Parse a JSON file into an experiment definition.
 *
 * @param registry Operations registry
 */
class JsonExperimentParser(registry: OpRegistry) extends ExperimentParser {
  override def parse(url: String): Experiment = {
    val om = new ObjectMapper
    val root = om.readTree(Getter(url).readContents())

    // Optional comment.
    val comment = if (root.has("comment")) Some(root.get("comment").asText) else None

    // Optional number of runs.
    val runs = if (root.has("runs")) root.get("runs").asInt else 1

    // Required data source.
    require(root.has("source"), "'source' must be defined")
    val source = parseSource(root.get("source"))

    // Optional preparator.
    val preparator = if (root.get("source").has("ops")) {
      Some(parseTransformation(root.get("source").get("ops")))
    } else {
      None
    }

    // Required treatments/optimizations. We differentiate between pure treatments and
    // optimizations by the presence (or not) of an 'objectives' key.
    require(root.has("treatments"), "'treatments' must be defined")
    val treatments = parseTreatments(root.get("treatments"))
    val optimizations = parseOptimizations(root.get("treatments"))

    // Required metrics.
    require(root.has("metrics"), "'metrics' must be defined")
    val evaluations = parseMetrics(root.get("metrics"))

    // optional Analyzers.
    val analyzers = if (root.has("analyzers")) {
      root.get("analyzers").elements.asScala
          .zipWithIndex
          .map { case (node, idx) => parseAnalyzer(idx, node) }
          .toSet
    } else {
      Set.empty[BoundAnalyzer]
    }

    Experiment(source, preparator, treatments, optimizations, evaluations, analyzers, comment, runs)
  }

  private def parseTreatments(node: JsonNode) = {
    node.elements.asScala
        .filterNot(_.has("objectives"))
        .zipWithIndex
        .map { case (n, idx) => parseTreatment(idx, n) }
        .toSet
  }

  private def parseOptimizations(node: JsonNode) = {
    node.elements.asScala
        .filter(_.has("objectives"))
        .zipWithIndex
        .map { case (n, idx) => parseOptimization(idx, n) }
        .toSet
  }

  private def parseMetrics(node: JsonNode) = {
    node.elements.asScala
        .zipWithIndex
        .map { case (n, idx) => parseMetric(idx, n) }
        .toSet
  }

  private def parseSource(node: JsonNode) = {
    require(node.has("url"), "A 'source.url' value must be defined")
    Dataset(new CsvTrackSource(FileUtils.replaceHome(node.get("url").asText)))
  }

  private def parseTreatment(idx: Int, node: JsonNode) = {
    require(node.has("name"), s"'treatments[$idx].name' must be defined")
    require(node.has("ops"), s"'treatments[$idx].ops' must be defined")

    val name = node.get("name").asText
    val exploration = parseExploration(node.get("ops"))
    Treatment(name, exploration)
  }

  private def parseMetric(idx: Int, node: JsonNode) = {
    require(node.has("name"), s"'metrics[$idx].name' must be defined")
    require(node.has("op"), s"'metrics[$idx].op' must be defined")

    val name = node.get("name").asText
    val op = getOpWithMap[Evaluator](node.get("op").asText)
    new BoundEvaluator(name, op._1, op._2)
  }

  private def parseAnalyzer(idx: Int, node: JsonNode) = {
    require(node.has("name"), s"'analyzers[$idx].name' must be defined")
    require(node.has("op"), s"'analyzers[$idx].op' must be defined")

    val name = node.get("name").asText
    val op = getOpWithMap[Analyzer](node.get("op").asText)
    new BoundAnalyzer(name, op._1, op._2)
  }

  private def parseTransformation(tree: JsonNode) = {
    val ops = tree.elements.asScala.map(n => getOpWithMap[Transformer](n.asText)).toSeq
    val paramMap = ops.map(_._2).foldLeft(ParamMap.empty)(_ ++ _)
    new BoundTransformer(ops.map(_._1), paramMap)
  }

  private def parseExploration(tree: JsonNode) = {
    val ops = tree.elements.asScala.map(n => getOpWithSpace[Transformer](n.asText)).toSeq
    val paramSpace = ops.map(_._2).foldLeft(ParamSpace.empty)(_ ++ _)
    Exploration(ops.map(_._1), paramSpace)
  }

  private def parseOptimization(idx: Int, node: JsonNode) = {
    require(node.has("name"), s"'treatments[$idx].name' must be defined")
    require(node.has("level"), s"'treatments[$idx].level' must be defined")
    require(node.has("ops"), s"'treatments[$idx].ops' must be defined")
    require(node.has("objectives"), s"'treatments[$idx].objectives' must be defined")

    val name = node.get("name").asText
    val level = node.get("level").asText
    val exploration = parseExploration(node.get("ops"))
    val objectives = node.get("objectives").elements.asScala
        .map(str => Objective.parse(str.asText))
        .toSet
    val itersPerStep = if (node.has("iters_per_step")) node.get("iters_per_step").asInt else 1
    val runs = if (node.has("runs")) Some(node.get("runs").asInt) else None
    Optimization(Treatment(name, exploration), level, itersPerStep, runs, objectives)
  }

  private def getOpWithMap[T <: Operation : ClassTag](str: String): (T, ParamMap) = {
    val (op, argsMap) = getOp[T](str)
    val pairs = op.params.map(param => getParamPair(param, argsMap.get(param.name)))
    (op, ParamMap(pairs: _*))
  }

  private def getOpWithSpace[T <: Operation : ClassTag](str: String): (T, ParamSpace) = {
    val (op, argsMap) = getOp[T](str)
    val domains = op.params.map(param => getParamDomain(param, argsMap.get(param.name)))
    (op, ParamSpace(domains: _*))
  }

  private def getOp[T <: Operation : ClassTag](str: String): (T, Map[String, String]) = {
    val pos = str.indexOf("(")
    val (opName, args) = if (pos > -1 && str.endsWith(")")) {
      (str.substring(0, pos), str.substring(pos + 1).dropRight(1))
    } else {
      (str, "")
    }
    val opClass = registry(opName)

    require(classTag[T].runtimeClass.isAssignableFrom(opClass),
      s"$opName is not a ${classTag[T].runtimeClass.getName} (got a ${opClass.getName})")

    val op = opClass.newInstance().asInstanceOf[T]
    val argsMap = args.split(",").map(_.split("=")).map { case s => s.head.trim -> s.tail.mkString("=").trim }.toMap
    (op, argsMap)
  }

  private def getParamDomain[T](param: Param[T], value: Option[String]): ParamDomain[T] = {
    val domain: Domain[T] = value match {
      case Some(str) => DomainParser.parse(str)(param.valueTypeTag)
      case None => Domain.value(param.defaultValue.get)
    }
    param ~= domain
  }

  private def getParamPair[T](param: Param[T], value: Option[String]): ParamPair[T] = {
    val parsedValue = value match {
      case Some(str) => Converter.of(param.valueTypeTag).convert(str)
      case None => param.defaultValue.get
    }
    param := parsedValue.asInstanceOf[T]
  }
}

private object DomainParser {
  def parse[T: TypeTag](str: String): Domain[T] = {
    val tpe = typeOf[T]
    val values = str.split("\\+").toSeq
        .map(_.trim)
        .map(str => parseRange(str, tpe).orElse(parseList(str, tpe)).getOrElse(parseValue(str, tpe)))
        .reduce(_ ++ _)
    Domain(values).asInstanceOf[Domain[T]]
  }

  private def parseRange(str: String, tpe: Type) = {
    if (str.startsWith("Range(") && str.endsWith(")")) {
      val bounds = str.drop(6).dropRight(1).split(";").map(_.trim)
      val values = tpe match {
        case _ if tpe <:< typeOf[Long] => parseLongRange(bounds)
        case _ if tpe <:< typeOf[Double] => parseDoubleRange(bounds)
        case _ if tpe <:< typeOf[Distance] => parseDistanceRange(bounds)
        case _ => throw new IllegalArgumentException(s"Range() works only with numeric values (got $tpe)")
      }
      Some(values)
    } else {
      None
    }
  }

  private def parseList(str: String, tpe: Type) = {
    if (str.startsWith("Discrete(") && str.endsWith(")")) {
      val converter = Converter.of(tpe)
      Some(str.drop(9).dropRight(1).split(";").map(s => converter.convert(s.trim)).toSeq)
    } else {
      None
    }
  }

  private def parseValue(str: String, tpe: Type) = Seq(Converter.of(tpe).convert(str))

  private def parseDoubleRange(bounds: Seq[String]) = {
    require(bounds.length == 3, s"Range() needs 3 parameters (got ${bounds.length})")
    val converter = Converter.of[Double]
    val doubles = bounds.map(converter.convert)
    if (doubles(0) < doubles(1)) {
      doubles(0) to doubles(1) by doubles(2)
    } else {
      (doubles(1) to doubles(0) by doubles(2)).reverse
    }
  }

  private def parseLongRange(bounds: Seq[String]) = {
    require(bounds.length == 2 || bounds.length == 3, s"Range() needs 2 or 3 parameters (got ${bounds.length})")
    val converter = Converter.of[Long]
    val longs = bounds.map(converter.convert)
    val step = if (longs.size > 2) longs(2) else 1L
    if (longs(0) < longs(1)) {
      longs(0) to longs(1) by step
    } else {
      (longs(1) to longs(0) by step).reverse
    }
  }

  private def parseDistanceRange(bounds: Seq[String]) = {
    require(bounds.length == 3, s"Range() needs 3 parameters (got ${bounds.length})")
    val converter = Converter.of[Distance]
    val distances = bounds.map(converter.convert)
    val meters = if (distances(0) < distances(1)) {
      distances(0).meters to distances(1).meters by distances(2).meters
    } else {
      (distances(1).meters to distances(0).meters by distances(2).meters).reverse
    }
    meters.map(Distance.meters)
  }
}