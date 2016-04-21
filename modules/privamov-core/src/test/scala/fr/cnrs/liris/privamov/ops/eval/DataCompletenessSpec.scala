package fr.cnrs.liris.privamov.ops.eval

import fr.cnrs.liris.accio.lib.param.ParamMap
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[DataCompleteness]].
 */
class DataCompletenessSpec extends UnitSpec with WithTrackGenerator {
  "DataCompleteness" should "compute the data completeness" in {
    val t1 = randomTrack(Me, 120)
    val t2 = randomTrack(Him, 85)
    val metrics = new DataCompleteness().evaluate(ParamMap.empty, t1, t2)
    metrics.find(_.name == "completeness").get.value shouldBe (85d / 120)
  }

  it should "return one for identical tracks" in {
    val t1 = randomTrack(Me, 120)
    val metrics = new DataCompleteness().evaluate(ParamMap.empty, t1, t1)
    metrics.find(_.name == "completeness").get.value shouldBe 1d
  }

  it should "return nothing for an empty track" in {
    val t1 = randomTrack(Me, 85)
    val t2 = randomTrack(Me, 0)
    val metrics = new DataCompleteness().evaluate(ParamMap.empty, t1, t2)
    metrics.find(_.name == "completeness").get.value shouldBe 0d
  }
}