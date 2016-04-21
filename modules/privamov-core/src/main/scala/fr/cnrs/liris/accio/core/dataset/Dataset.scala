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

package fr.cnrs.liris.accio.core.dataset

import java.util
import java.util.Collections

import fr.cnrs.liris.privamov.lib.io.codec.DataSource
import fr.cnrs.liris.privamov.model.Track
import fr.cnrs.liris.util.concurrent.ParallelExecutor
import fr.cnrs.liris.util.random.SamplingUtils

import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap

trait Dataset[T] {
  private[this] val parallelism = sys.env.get("CORES").map(_.toInt).getOrElse(math.max(1, sys.runtime.availableProcessors() - 1))

  def keys: Seq[String]

  def load(): Seq[T] = load(None)

  def load(label: Option[String]): Seq[T]

  def restrict(keys: Iterable[String]): Dataset[T] =
    new RestrictDataset(this, this.keys.intersect(keys.toSeq))

  def sample(proba: Double): Dataset[T] = new SampleDataset(this, proba)

  def count(): Int = submit(_.size).sum

  def traverse[U](fn: T => U): Seq[U] = submit(_.map(fn)).flatten

  def foreach(fn: T => Unit, keys: Seq[String] = this.keys): Unit = submit(_.foreach(fn))

  private def submit[U](fn: Seq[T] => U): Seq[U] = {
    if (keys.isEmpty) {
      Seq.empty
    } else if (keys.size == 1) {
      Seq(fn(load(Some(keys.head))))
    } else {
      val executor = new ParallelExecutor(parallelism)
      val results = Collections.synchronizedMap(new util.HashMap[String, U]).asScala
      keys.foreach { key =>
        executor.submit(() => {
          results(key) = fn(load(Some(key)))
        })
      }
      executor.shutdown()
      results synchronized {
        keys.map(key => results(key))
      }
    }
  }
}

object Dataset {
  def apply[T](source: DataSource[T]): Dataset[T] =
    new DataLoaderDataset(source.index, source.reader, source.decoder)

  def apply[T](key: String, values: Iterable[T]): Dataset[T] =
    new InMemoryDataset(TreeMap(key -> values.toSeq))

  def apply[T](data: (String, Iterable[T])*): Dataset[T] =
    new InMemoryDataset(TreeMap(data.map { case (key, value) => key -> value.toSeq }: _*))

  implicit def toTrackFunctions(dataset: Dataset[Track]): TrackFunctions =
    new TrackFunctions(dataset)
}

private class RestrictDataset[T](inner: Dataset[T], override val keys: Seq[String]) extends Dataset[T] {
  override def load(label: Option[String]): Seq[T] = {
    require(label.isEmpty || keys.contains(label.get))
    inner.load(label)
  }
}

private class SampleDataset[T](inner: Dataset[T], proba: Double) extends Dataset[T] {
  require(proba >= 0 && proba <= 1, s"proba must be in [0,1] (got $proba)")

  override val keys = SamplingUtils.sampleUniform(inner.keys, proba)

  override def load(label: Option[String]): Seq[T] = {
    require(label.isEmpty || keys.contains(label.get))
    inner.load(label)
  }
}