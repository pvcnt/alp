package fr.cnrs.liris.privamov.model

import java.time.Duration

import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

class TrackSpec extends UnitSpec with WithTrackGenerator {
  "Track" should "return its duration" in {
    val t1 = randomTrack(Me, 101, Duration.ofSeconds(10))
    t1.duration shouldBe Duration.ofSeconds(1000)

    val t2 = Track.empty(Me)
    t2.duration shouldBe Duration.ZERO
  }

  it should "return its size" in {
    val t1 = randomTrack(Me, 101, Duration.ofSeconds(10))
    t1.size shouldBe 101

    val t2 = Track.empty(Me)
    t2.size shouldBe 0
  }

  it should "return its user" in {
    val t1 = Track(Me, Seq(Record(Me, Here, Now)))
    t1.user shouldBe Me

    val t2 = Track.empty(Me)
    t2.user shouldBe Me
  }
}