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

package fr.cnrs.liris.util.concurrent

import java.util.concurrent._

import com.google.common.base.MoreObjects
import com.twitter.util.{Future => TwitterFuture, Promise}
import com.typesafe.scalalogging.LazyLogging
import fr.cnrs.liris.util.conversions.thread._

/**
 * Lightweight wrapper around an executor running tasks over a multi-threaded
 * pool containing one thread per available processor.
 *
 * @param nbCores Number of threads to use, equal to number of processors if <= 0
 */
class ParallelExecutor(nbCores: Int) extends LazyLogging {
  private[this] val executor = if (1 == nbCores) {
    Executors.newSingleThreadExecutor
  } else {
    Executors.newFixedThreadPool(nbCores)
  }

  /**
   * Submit a task to execute. It will be run asynchronously using the thread
   * pool at its disposal.
   *
   * @param task A task
   */
  def submit[T](task: () => Unit): TwitterFuture[Unit] = {
    val p = Promise[Unit]()
    executor.submit(() => try {
      task()
      p.setDone()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        p.setException(ex)
    })
    p
  }

  /**
   * Close the pool, allowing no more task to be submitted. This method will
   * block until execution of all tasks is finished.
   */
  def shutdown(): Unit = {
    executor.shutdown()
    try {
      while (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
        Thread.`yield`()
      }
    } catch {
      case ex: InterruptedException => logger.error("Interruption while executing tasks", ex)
    }
  }

  override def toString: String = {
    MoreObjects.toStringHelper(this)
      .add("cores", nbCores)
      .add("status", if (executor.isTerminated) "TERMINATED" else if (executor.isShutdown) "SHUTDOWN" else "RUNNING")
      .toString
  }
}