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

import com.google.common.geometry.S1Angle
import fr.cnrs.liris.util.Distance

/**
 * A planar 2D point.
 *
 * @param x X coordinate
 * @param y Y coordinate
 */
case class Point(x: Double, y: Double) {
  /**
   * Compute the euclidian distance with another point.
   *
   * @param other Another point
   * @return A distance
   */
  def distance(other: Point): Distance =
    Distance.meters(Math.sqrt(Math.pow(other.x - x, 2) + Math.pow(other.y - y, 2)))

  def interpolate(dest: Point, ratio: Double): Point = {
    val dx = dest.x - x
    val dy = dest.y - y
    new Point(x + ratio * dx, y + ratio * dy)
  }

  /**
   * Move a point in a given direction from a given distance.
   *
   * @param direction A direction as an angle (e.g. 0° east, 90° north, 180° west, 270° south)
   * @param distance  A distance to move the point by
   * @return A new translated point
   */
  def translate(direction: S1Angle, distance: Distance): Point = {
    val dx = distance.meters * Math.cos(direction.radians)
    val dy = distance.meters * Math.sin(direction.radians)
    new Point(x + dx, y + dy)
  }

  def translate(dx: Double, dy: Double): Point = new Point(x + dx, y + dy)

  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def -(other: Point): Point = Point(x - other.x, y - other.y)

  def /(factor: Double): Point = Point(x / factor, y / factor)

  def *(factor: Double): Point = Point(x * factor, y * factor)

  def toLatLng: LatLng = LatLng(Mercator.y2lat(y), Mercator.x2lng(x))

  def toGeoJson: GeoPoint = toLatLng.toGeoJson
}

object Point {
  def tuple(tuple: (Double, Double)): Point = new Point(tuple._1, tuple._2)

  def nearest(ref: Point, points: Iterable[Point]): PointWithDistance =
    points.zipWithIndex.map { case (pt, idx) =>
      PointWithDistance(pt, idx, pt.distance(ref))
    }.minBy(_.distance)

  def centroid(points: Iterable[Point]): Point = points.reduce(_ + _) / points.size

  /**
   * Heuristic to fast compute the diameter of a set of points. The result is
   * still approximate and not guaranteed to be exact.
   *
   * @param locs A collection of points
   * @return The diameter
   * @see http://stackoverflow.com/questions/16865291/greatest-distance-between-set-of-longitude-latitude-points
   */
  def fastDiameter(locs: Iterable[Point]): Distance = {
    val c = Point.centroid(locs)
    val p0 = farthest(c, locs).point
    val p1 = farthest(p0, locs).point
    p0.distance(p1)
  }

  /**
   * Compute the farthest point in a collection from a reference point. If two
   * points are at the same distance, the first encountered point is kept.
   *
   * @param ref  Reference point
   * @param locs A collection of points
   * @return Farthest point with distance to reference point
   */
  def farthest(ref: Point, locs: Iterable[Point]): PointWithDistance =
    locs.zipWithIndex.map { case (pt, idx) =>
      new PointWithDistance(pt, idx, pt.distance(ref))
    }.maxBy(_.distance)

  /**
   * Exact computation of the diameter of a set of points. It computes
   * distances between each pair of points and takes the maximum.
   * (i.e., computing distance between each pair of points) is not suitable when
   * the number of points grows.
   *
   * @param locs A collection of points
   * @return The diameter
   */
  def exactDiameter(locs: Iterable[Point]): Distance = {
    var diameter = Distance.Zero
    var i = 0
    val size = locs.size
    for (l1 <- locs) {
      for (l2 <- locs.takeRight(size - i)) {
        if (l1 != l2) {
          val d = l1.distance(l2)
          if (d > diameter) {
            diameter = d
          }
        }
      }
      i += 1
    }
    diameter
  }
}

case class PointWithDistance(point: Point, idx: Int, distance: Distance)