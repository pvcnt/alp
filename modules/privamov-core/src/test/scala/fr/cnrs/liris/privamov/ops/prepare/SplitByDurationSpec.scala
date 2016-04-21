package fr.cnrs.liris.privamov.ops.prepare

import java.time.Duration

import fr.cnrs.liris.accio.core.framework.BoundTransformer
import fr.cnrs.liris.privamov.model.Track
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

import scala.util.Random

class SplitByDurationSpec extends UnitSpec with WithTrackGenerator {
  "SplitByDuration" should "split by duration" in {
    val track = randomTrack(Me, 150, Duration.ofSeconds(Random.nextInt(10)))
    val res = transform(track, Duration.ofSeconds(10))
    res.map(_.user).foreach(user => user shouldBe Me)
    res.flatMap(_.records) shouldBe track.records
    res.foreach(_.duration.getSeconds should be <= (10L))
  }

  it should "handle a duration greater than track's duration" in {
    val track = randomTrack(Me, 60, Duration.ofSeconds(1))
    val res = transform(track, Duration.ofSeconds(100))
    res should have size 1
    res.head shouldBe track
  }

  private def transform(track: Track, duration: Duration) = {
    val transformation = BoundTransformer(new SplitByDuration)(_.duration := duration)
    transformation.transform(track)
  }
}