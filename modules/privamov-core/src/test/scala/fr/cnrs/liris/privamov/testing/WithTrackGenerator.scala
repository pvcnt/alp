package fr.cnrs.liris.privamov.testing

import java.time.{Duration, Instant}

import fr.cnrs.liris.privamov.geo.LatLng
import fr.cnrs.liris.privamov.model.{Record, Track}

trait WithTrackGenerator {
  protected[this] val Here = LatLng.degrees(48.858222, 2.2945).toPoint
  protected[this] val Now = Instant.now
  protected[this] val Me = "me"
  protected[this] val Him = "him"

  protected def randomTrack(user: String, size: Int, rate: => Duration = Duration.ofSeconds(1)) = {
    if (size <= 0) {
      Track.empty(user)
    } else {
      Track(Seq.tabulate(size)(i => Record(user, Here, Now.plus(rate.multipliedBy(i)))))
    }
  }
}