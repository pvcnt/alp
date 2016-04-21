package fr.cnrs.liris.privamov.ops.prepare

import java.time.Duration

import fr.cnrs.liris.accio.core.framework.BoundTransformer
import fr.cnrs.liris.privamov.model.{Record, Track}
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

class TemporalSamplingSpec extends UnitSpec with WithTrackGenerator {
  "TemporalSampling" should "downsample tracks" in {
    val track = Track(Seq(
      Record(Me, Here, Now),
      Record(Me, Here, Now.plusSeconds(10)),
      Record(Me, Here, Now.plusSeconds(19)),
      Record(Me, Here, Now.plusSeconds(25)),
      Record(Me, Here, Now.plusSeconds(34)),
      Record(Me, Here, Now.plusSeconds(44))))
    transform(track, Duration.ofSeconds(10)) shouldBe Track(Me, Seq(
      Record(Me, Here, Now),
      Record(Me, Here, Now.plusSeconds(10)),
      Record(Me, Here, Now.plusSeconds(25)),
      Record(Me, Here, Now.plusSeconds(44))))
  }

  it should "handle empty tracks" in {
    val track = Track.empty(Me)
    transform(track, Duration.ofSeconds(10)) shouldBe track
  }

  it should "handle singleton tracks" in {
    val track = Track(Seq(Record(Me, Here, Now)))
    transform(track, Duration.ofSeconds(10)) shouldBe track
  }

  private def transform(track: Track, duration: Duration) = {
    val transformation = BoundTransformer(new TemporalSampling)(_.duration := duration)
    val res = transformation.transform(track)
    res should have size 1
    res.head
  }
}