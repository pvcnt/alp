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

package fr.cnrs.liris.accio

import java.io.FileOutputStream
import java.nio.file.Paths

import com.typesafe.scalalogging.StrictLogging
import fr.cnrs.liris.accio.core._
import fr.cnrs.liris.accio.core.runtime.{SummarizeCommand, MakeCommand, Command, AnalyzeCommand}
import fr.cnrs.liris.accio.lib.profiler.{ProfilePhase, ProfiledTaskKinds, Profiler}
import fr.cnrs.liris.util.flags.{Flag, FlagsData, FlagsParser, FlagsProvider}
import fr.cnrs.liris.util.io.FileUtils
import fr.cnrs.liris.util.reflect.ReflectUtils

import scala.reflect.runtime.universe._

object AccioAppMain extends AccioApp

case class AccioAppLaunchFlags(
  @Flag(name = "profile")
  profile: String = "")

class AccioApp extends StrictLogging {
  def main(args: Array[String]): Unit = {
    Profiler.markPhase(ProfilePhase.Launch)

    Profiler.markPhase(ProfilePhase.Init)
    val commandType = getCommandType(args.headOption)
    val flags = parseFlagsForCommand(commandType, args.drop(1))
    val launchFlags = flags.as[AccioAppLaunchFlags]
    maybeStartProfiler(launchFlags)
    val command = ReflectUtils.typeToClass(commandType).newInstance.asInstanceOf[Command[_]]

    Profiler.markPhase(ProfilePhase.Exec)
    command.run(flags)

    Profiler.markPhase(ProfilePhase.Finish)
    maybeStopProfiler(launchFlags)
  }

  private def maybeStartProfiler(flags: AccioAppLaunchFlags) =
    if (flags.profile.nonEmpty) {
      val profileFile = Paths.get(FileUtils.replaceHome(flags.profile)).toFile
      if (profileFile.exists()) {
        profileFile.delete()
      }
      Profiler.start(ProfiledTaskKinds.All, Some(new FileOutputStream(profileFile)))
      logger.debug(s"Profiling application to ${profileFile.toPath.toAbsolutePath}")
    }

  private def maybeStopProfiler(flags: AccioAppLaunchFlags) =
    if (flags.profile.nonEmpty) {
      Profiler.stop()
    }

  private def getCommandType(name: Option[String]) =
    name match {
      case Some("analyze") => typeOf[AnalyzeCommand]
      case Some("summarize") => typeOf[SummarizeCommand]
      case Some("make") => typeOf[MakeCommand]
      case Some(str) => throw new IllegalArgumentException(s"Unknown command '$str'")
      case None => throw new IllegalArgumentException(s"You must specify a command to run")
    }

  private def parseFlagsForCommand(command: Type, args: Seq[String]): FlagsProvider = {
    val commandTypes = command.baseType(typeOf[Command[_]].typeSymbol).typeArgs.head
    val flagsTypes = if (commandTypes =:= typeOf[Unit]) {
      Seq.empty[Type]
    } else if (commandTypes <:< typeOf[Product] && commandTypes.typeArgs.nonEmpty) {
      commandTypes.typeArgs
    } else {
      Seq(commandTypes)
    }
    val flagsData = FlagsData.of(flagsTypes ++ Seq(typeOf[AccioAppLaunchFlags]))
    val parser = new FlagsParser(flagsData, allowResidue = true)
    parser.parse(args)
    parser
  }
}