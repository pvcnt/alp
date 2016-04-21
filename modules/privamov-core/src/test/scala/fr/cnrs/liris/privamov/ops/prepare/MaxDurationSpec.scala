package fr.cnrs.liris.privamov.ops.prepare

import java.time.Duration

import fr.cnrs.liris.accio.core.framework.BoundTransformer
import fr.cnrs.liris.privamov.model.Track
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[MaxDuration]].
 */
class MaxDurationSpec extends UnitSpec with WithTrackGenerator {
  "MaxDuration" should "shorten tracks with a duration greater than threshold" in {
    val track = randomTrack(Me, size = 15, rate = Duration.ofMinutes(1))
    val t1 = transform(track, Duration.ofSeconds(10 * 60 + 10))
    assertTrackIsShortened(track, t1, 11)
  }

  it should "keep tracks with a duration lower than threshold" in {
    val track = randomTrack(Me, size = 15, rate = Duration.ofMinutes(1))
    val t1 = transform(track, Duration.ofMinutes(14))
    val t2 = transform(track, Duration.ofMinutes(20))
    assertTrackIsShortened(track, t1, 15)
    assertTrackIsShortened(track, t2, 15)
  }

  private def transform(track: Track, duration: Duration) = {
    val transformation = BoundTransformer(new MaxDuration)(_.duration := duration)
    val res = transformation.transform(track)
    res should have size 1
    res.head
  }

  private def assertTrackIsShortened(t: Track, t1: Track, s1: Int) = {
    t1.user shouldBe t.user
    t1.records shouldBe t.records.take(s1)
  }
}