package fr.cnrs.liris.privamov.ops.prepare

import fr.cnrs.liris.accio.core.framework.BoundTransformer
import fr.cnrs.liris.privamov.model.Track
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[MinSize]].
 */
class MinSizeSpec extends UnitSpec with WithTrackGenerator {
  "MinSize" should "keep tracks with a length equal to threshold" in {
    val track = randomTrack(Me, 15)
    transform(track, 15) shouldBe Seq(track)
  }

  it should "keep tracks with a length greater than threshold" in {
    val track = randomTrack(Me, 15)
    transform(track, 10) shouldBe Seq(track)
  }

  it should "reject tracks with a length lower than threshold" in {
    val track = randomTrack(Me, 15)
    transform(track, 16) shouldBe Seq.empty[Track]
    transform(track, 20) shouldBe Seq.empty[Track]
  }

  private def transform(track: Track, size: Int) = {
    val transformation = BoundTransformer(new MinSize)(_.size := size)
    transformation.transform(track)
  }
}