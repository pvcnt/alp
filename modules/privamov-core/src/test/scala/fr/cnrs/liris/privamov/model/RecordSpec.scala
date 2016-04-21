package fr.cnrs.liris.privamov.model

import fr.cnrs.liris.privamov.testing.WithTrackGenerator
import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[Record]].
 */
class RecordSpec extends UnitSpec with WithTrackGenerator {
  "Record" should "export to GeoJSON" in {
    val json = Record(Me, Here, Now).toGeoJson
    json.geometry shouldBe Here.toGeoJson
    json.properties("time") shouldBe Now.toEpochMilli
    json.properties("user") shouldBe "me"
  }

  it should "include additional properties when exported to GeoJSON" in {
    val json = Record(Me, Here, Now, Map("foo" -> 1d, "bar" -> 2d)).toGeoJson
    json.properties("foo") shouldBe 1d
    json.properties("bar") shouldBe 2d
  }

  it should "set new properties" in {
    val rec1 = Record(Me, Here, Now, Map("foo" -> 1d, "bar" -> 1d))
    val rec2 = rec1.set("foo", 2d)
    val rec3 = rec2.set("foobar", 3d)
    rec1.props("foo") shouldBe 1d
    rec1.props("bar") shouldBe 1d
    rec2.props("foo") shouldBe 2d
    rec2.props("bar") shouldBe 1d
    rec2.props.get("foobar") shouldBe None
    rec3.props("foo") shouldBe 2d
    rec3.props("bar") shouldBe 1d
    rec3.props("foobar") shouldBe 3d
  }
}