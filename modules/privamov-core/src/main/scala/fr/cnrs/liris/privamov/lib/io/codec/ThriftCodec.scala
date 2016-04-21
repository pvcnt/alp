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

package fr.cnrs.liris.privamov.lib.io.codec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import fr.cnrs.liris.privamov.lib.io.reader.EncodedRecord
import org.apache.thrift.TBase
import org.apache.thrift.protocol.{TBinaryProtocol, TCompactProtocol, TProtocolFactory}
import org.apache.thrift.transport.TIOStreamTransport

import scala.reflect._

/**
 * Base class for codecs using Thrift to serialize data. It can handle objects extending TBase, hence associated with
 * a Thrift schema.
 *
 * @tparam T Type being handled by this codec
 * @tparam P Type of the associated protocol factory
 */
private[codec] class ThriftCodec[T <: TBase[_, _] : ClassTag, P <: TProtocolFactory : ClassTag]
  extends Decoder[T] with Encoder[T] {
  override val elementClassTag = classTag[T]
  private[this] lazy val prototype: T = elementClassTag.runtimeClass.asInstanceOf[Class[T]].newInstance
  private[this] lazy val protocolFactory: TProtocolFactory = classTag[P].runtimeClass.asInstanceOf[Class[P]].newInstance

  override def decode(record: EncodedRecord): Option[T] = {
    val obj = prototype.deepCopy
    val bais = new ByteArrayInputStream(record.bytes)
    obj.read(protocolFactory.getProtocol(new TIOStreamTransport(bais)))
    Some(obj.asInstanceOf[T])
  }

  override def encode(obj: T): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    obj.write(protocolFactory.getProtocol(new TIOStreamTransport(baos)))
    baos.toByteArray
  }
}

/**
 * Concrete Thrift codec using the binary protocol.
 *
 * @tparam T Type being handled by this codec
 */
class BinaryThriftCodec[T <: TBase[_, _] : ClassTag] extends ThriftCodec[T, TBinaryProtocol.Factory]

/**
 * Concrete Thrift codec using the compact protocol.
 *
 * @tparam T Type being handled by this codec
 */
class CompactThriftCodec[T <: TBase[_, _] : ClassTag] extends ThriftCodec[T, TCompactProtocol.Factory]