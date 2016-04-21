package fr.cnrs.liris.privamov.ops.prepare

import java.time.Duration

import fr.cnrs.liris.accio.core.framework.BoundTransformer
import fr.cnrs.liris.privamov.model.Track
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[MinDuration]].
 */
class MinDurationSpec extends UnitSpec with WithTrackGenerator {
  "MinDuration" should "keep tracks with a duration equal to threshold" in {
    val track = randomTrack(Me, size = 15, rate = Duration.ofMinutes(1))
    transform(track, Duration.ofMinutes(14)) shouldBe Seq(track)
  }

  it should "keep tracks with a duration greater than threshold" in {
    val track = randomTrack(Me, size = 15, rate = Duration.ofMinutes(1))
    transform(track, Duration.ofMinutes(10)) shouldBe Seq(track)
  }

  it should "reject tracks with a duration lower than threshold" in {
    val track = randomTrack(Me, size = 15, rate = Duration.ofMinutes(1))
    transform(track, Duration.ofMinutes(15)) shouldBe Seq.empty[Track]
    transform(track, Duration.ofMinutes(20)) shouldBe Seq.empty[Track]
  }

  private def transform(track: Track, duration: Duration) = {
    val transformation = BoundTransformer(new MinDuration)(_.duration := duration)
    transformation.transform(track)
  }
}