package fr.cnrs.liris.privamov.ops.eval

import breeze.linalg.{DenseVector, max, min}
import com.google.common.geometry.S1Angle
import fr.cnrs.liris.accio.lib.param.ParamMap
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.Distance
import fr.cnrs.liris.util.testing.UnitSpec

import scala.util.Random
import breeze.stats._
import fr.cnrs.liris.privamov.model.Track

/**
 * Unit tests for [[SpatialDistortion]].
 */
class SpatialDistortionSpec extends UnitSpec with WithTrackGenerator {
  val eps = 1e-9

  "SpatialDistortion" should "return zero for identical tracks" in {
    val t1 = randomTrack(Me, 120)
    val metrics = new SpatialDistortion().evaluate(ParamMap.empty, t1, t1)
    metrics.find(_.name == "avg").get.value shouldBe 0d
    metrics.find(_.name == "min").get.value shouldBe 0d
    metrics.find(_.name == "max").get.value shouldBe 0d
    metrics.find(_.name == "median").get.value shouldBe 0d
    metrics.find(_.name == "stddev").get.value shouldBe 0d
  }

  it should "return zero for temporally shifted tracks" in {
    val t1 = randomTrack(Me, 120)
    val t2 = t1.transform { records =>
      var prev = records.head.time
      records.zipWithIndex.map { case (rec, idx) =>
        val now = prev.plusSeconds(Random.nextInt(3600))
        prev = now
        rec.copy(time = now)
      }
    }
    val metrics = new SpatialDistortion().evaluate(ParamMap.empty, t1, t2)
    metrics.find(_.name == "avg").get.value shouldBe 0d
    metrics.find(_.name == "min").get.value shouldBe 0d
    metrics.find(_.name == "max").get.value shouldBe 0d
    metrics.find(_.name == "median").get.value shouldBe 0d
    metrics.find(_.name == "stddev").get.value shouldBe 0d
  }

  it should "compute the spatial distortion" in {
    val t1 = randomTrack(Me, 120)
    val distances = DenseVector(Seq.fill(120)(Random.nextInt(5000).toDouble): _*)
    val t2 = t1.transform { records =>
      records.zipWithIndex.map { case (rec, idx) =>
        rec.copy(point = rec.point.translate(S1Angle.degrees(Random.nextInt(360)), Distance.meters(distances(idx))))
      }
    }
    val metrics = evaluate(t1, t2, interpolate = true)
    metrics.find(_.name == "avg").get.value shouldBe (mean(distances) +- eps)
    metrics.find(_.name == "min").get.value shouldBe (min(distances) +- eps)
    metrics.find(_.name == "max").get.value shouldBe (max(distances) +- eps)
    metrics.find(_.name == "median").get.value shouldBe (median(distances) +- eps)
    metrics.find(_.name == "stddev").get.value shouldBe (stddev(distances) +- eps)
  }

  private def evaluate(reference: Track, result: Track, interpolate: Boolean) = {
    val ev = new SpatialDistortion
    ev.evaluate(ParamMap(ev.interpolate := interpolate), reference, result)
  }
}