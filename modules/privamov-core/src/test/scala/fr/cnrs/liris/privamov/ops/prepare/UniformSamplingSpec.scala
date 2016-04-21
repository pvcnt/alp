package fr.cnrs.liris.privamov.ops.prepare

import java.time.Duration

import fr.cnrs.liris.accio.core.framework.BoundTransformer
import fr.cnrs.liris.privamov.model.Track
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.{Flacky, UnitSpec}

class UniformSamplingSpec extends UnitSpec with WithTrackGenerator {
  "UniformSampling" should "downsample tracks" in {
    val track = randomTrack(Me, 100, Duration.ofSeconds(10))
    val runs = 10
    Seq.fill(runs)(transform(track, 0.1).size).sum.toDouble shouldBe ((10d * runs) +- (2 * runs))
    Seq.fill(runs)(transform(track, 0.5).size).sum.toDouble shouldBe ((50d * runs) +- (2 * runs))
    Seq.fill(runs)(transform(track, 0.9).size).sum.toDouble shouldBe ((90d * runs) +- (2 * runs))
  }

  it should "handle null probability" in {
    val track = randomTrack(Me, 100, Duration.ofSeconds(10))
    transform(track, 0) shouldBe Track.empty(Me)
  }

  it should "handle certain probability" in {
    val track = randomTrack(Me, 100, Duration.ofSeconds(10))
    transform(track, 1) shouldBe track
  }

  private def transform(track: Track, probability: Double) = {
    val transformation = BoundTransformer(new UniformSampling)(_.probability := probability)
    val res = transformation.transform(track)
    res should have size 1
    res.head
  }
}
