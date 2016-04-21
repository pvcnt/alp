/*
 * Copyright LIRIS-CNRS (2016)
 * Contributors: Vincent Primault <vincent.primault@liris.cnrs.fr>
 *
 * This software is a computer program whose purpose is to study location privacy.
 *
 * This software is governed by the CeCILL-B license under French law and
 * abiding by the rules of distribution of free software. You can use,
 * modify and/ or redistribute the software under the terms of the CeCILL-B
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty and the software's author, the holder of the
 * economic rights, and the successive licensors have only limited liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading, using, modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean that it is complicated to manipulate, and that also
 * therefore means that it is reserved for developers and experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and, more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL-B license and that you accept its terms.
 */

package fr.cnrs.liris.privamov.geo

import com.google.common.geometry.{S1Angle, S2LatLng}
import fr.cnrs.liris.util.Distance

/**
 * A geographical location.
 */
class LatLng private(private val inner: S2LatLng) {
  def lat: S1Angle = inner.lat

  def lng: S1Angle = inner.lng

  /**
   * Compute the distance with another location on the surface of the Earth..
   *
   * @param other Another location
   * @return The distance in meters
   */
  def distance(other: LatLng): Distance = Distance.meters(inner.getEarthDistance(other.inner))

  def translate(angle: S1Angle, distance: Distance): LatLng = {
    // http://www.movable-type.co.uk/scripts/latlong.html
    val angDistance = distance / LatLng.EarthRadius
    val lat1 = inner.latRadians
    val lon1 = inner.lngRadians
    val lat2 = math.asin(
      math.sin(lat1) * math.cos(angDistance) +
          math.cos(lat1) * math.sin(angDistance) * math.cos(angle.radians))
    val lon2 = lon1 + math.atan2(
      math.sin(angle.radians) * Math.sin(angDistance) * Math.cos(lat1),
      Math.cos(angDistance) - Math.sin(lat1) * Math.sin(lat2))
    LatLng.radians(lat2, lon2)
  }

  def toPoint: Point = Point(Mercator.lng2x(lng), Mercator.lat2y(lat))

  def toGeoJson: GeoPoint = GeoPoint.ofCoords(lng.degrees, lat.degrees)

  def toS2: S2LatLng = inner
}

object LatLng {
  val EarthRadius = Distance.meters(S2LatLng.EARTH_RADIUS_METERS)

  def parse(str: String): LatLng = str.split(",") match {
    case Array(lat, lng) => LatLng.degrees(lat.trim.toDouble, lng.trim.toDouble)
    case _ => throw new IllegalArgumentException(s"Invalid lat/lng string: $str")
  }

  def apply(lat: S1Angle, lng: S1Angle): LatLng = new LatLng(new S2LatLng(lat, lng))

  /**
   * Create a location from a latitude and a longitude.
   *
   * @param lat
   * @param lng
   * @return
   */
  def degrees(lat: Double, lng: Double): LatLng = new LatLng(S2LatLng.fromDegrees(lat, lng))

  def radians(lat: Double, lng: Double): LatLng = {
    val normLat = math.atan(math.sin(lat) / math.abs(math.cos(lat)))
    val normLng = math.atan2(math.sin(lat), math.cos(lat))
    new LatLng(S2LatLng.fromRadians(normLat, normLng))
  }
}