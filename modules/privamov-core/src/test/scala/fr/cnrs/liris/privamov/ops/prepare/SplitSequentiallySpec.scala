package fr.cnrs.liris.privamov.ops.prepare

import fr.cnrs.liris.accio.core.framework.BoundTransformer
import fr.cnrs.liris.privamov.model.Track
import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[SplitSequentially]].
 */
class SplitSequentiallySpec extends UnitSpec with WithTrackGenerator {
  "SplitSequentially" should "split a track with an even number of records without losing any of them" in {
    val track = randomTrack(Me, 150)
    val (out1, out2) = split(percent = 50, track)
    assertTrackIsSplitted(track, out1.head, out2.head, 75)
  }

  it should "split a track with a odd number of records without losing any of them" in {
    val track = randomTrack(Me, 151)
    val (out1, out2) = split(percent = 50, track)
    assertTrackIsSplitted(track, out1.head, out2.head, 76)
  }

  it should "split several tracks" in {
    val track1 = randomTrack(Me, 150)
    val track2 = randomTrack(Him, 150)
    val (out1, out2) = split(percent = 50, track1, track2)
    assertTrackIsSplitted(track1, out1.head, out2.head, 75)
    assertTrackIsSplitted(track2, out1.last, out2.last, 75)
  }

  it should "split an empty track into two empty tracks" in {
    val track = Track.empty(Me)
    val (out1, out2) = split(percent = 50, track)
    assertTrackIsSplitted(track, out1.head, out2.head, 0)
  }

  it should "split an empty iterator into two empty iterators" in {
    split(.5)
  }

  it should "split a track at 0%" in {
    val track = randomTrack(Me, 150)
    val (out1, out2) = split(percent = 0, track)
    assertTrackIsSplitted(track, out1.head, out2.head, 0)
  }

  it should "split a track at 100%" in {
    val track = randomTrack(Me, 150)
    val (out1, out2) = split(percent = 100, track)
    assertTrackIsSplitted(track, out1.head, out2.head, 150)
  }

  private def split(percent: Double, tracks: Track*): (Seq[Track], Seq[Track]) = {
    val splitter1 = BoundTransformer(new SplitSequentially)(_.percentBegin := 0, _.percentEnd := percent, _.complement := false)
    val splitter2 = BoundTransformer(new SplitSequentially)(_.percentBegin := 0, _.percentEnd := percent, _.complement := true)
    val out1 = tracks.flatMap(splitter1.transform)
    val out2 = tracks.flatMap(splitter2.transform)
    out1 should have size tracks.size
    out2 should have size tracks.size
    (out1, out2)
  }

  private def assertTrackIsSplitted(t: Track, t1: Track, t2: Track, s1: Int): Unit = {
    t1.user shouldBe t.user
    t2.user shouldBe t.user
    t1.records shouldBe t.records.take(s1)
    t2.records shouldBe t.records.drop(s1)
  }
}