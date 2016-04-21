import Dependencies._
import sbt.Keys._
import sbt._
import sbtassembly.AssemblyKeys._
import sbtassembly.{MergeStrategy, PathList}
import sbtdocker.DockerKeys._
import sbtdocker._

import scala.io.Source

object Commons {
  private val libVersion = "0.1"

  private val sharedSettings = Seq(
    version := libVersion,
    organization := "fr.cnrs.liris",
    scalaVersion := "2.11.7",
    updateOptions := updateOptions.value.withCachedResolution(true),

    //Common dependencies for all projects.
    libraryDependencies ++= guavaDeps ++ loggingDeps ++ scalacticDeps ++ testDeps.map(_ % Test),

    //Force Scala libraries to be at the same version than scala-library.
    dependencyOverrides ++= Set(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value
    ),

    //Force a single version of dependencies.
    dependencyOverrides ++= Set(
      "junit" % "junit" % V.Junit,
      "com.google.guava" % "guava" % V.Guava,
      "com.google.inject" % "guice" % V.Guice,
      "org.slf4j" % "slf4j-api" % V.Slf4j,
      "org.slf4j" % "jul-to-slf4j" % V.Slf4j,
      "org.slf4j" % "jcl-over-slf4j" % V.Slf4j,
      "org.apache.commons" % "commons-math3" % V.Commons,
      "com.google.code.findbugs" % "jsr305" % V.Findbugs,
      "org.apache.thrift" % "libthrift" % V.Thrift
    ),

    resolvers += Resolver.bintrayRepo("collectivemedia", "releases"),
    resolvers += "twitter" at "https://maven.twttr.com",

    javacOptions ++= Seq("-Xlint", "-source", "1.8", "-target", "1.8"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-Xlint",
      "-encoding", "utf8",
      "-language:implicitConversions",
      "-language:postfixOps"
    ),

    // Disable parallel execution of tests (does not play well with Spark).
    parallelExecution in Test := false,

    // Force the inclusion of "provided" dependencies when running the application through SBT.
    run in Compile <<= Defaults.runTask(fullClasspath in Compile, mainClass in(Compile, run), runner in(Compile, run)),
    runMain in Compile <<= Defaults.runMainTask(fullClasspath in Compile, runner in(Compile, run)),

    // Fork the JVM process when running the application through SBT (avoids a lot of classpath problems).
    fork in run := true,
    fork in runMain := true,

    // Do not run tests when creating a fat JAR.
    test in assembly := {},

    // Assembly shadowing rules.
    assemblyMergeStrategy in assembly := {
      case PathList(ps@_*) if ps.last == "BUILD" => MergeStrategy.rename // Pants build files
      case PathList(ps@_*) if ps.last endsWith ".conf" => MergeStrategy.concat // Configuration files
      case x => // Keep old strategy
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    }
  )

  def module(
    id: String,
    deps: Seq[ModuleID] = Seq.empty,
    excludes: Seq[SbtExclusionRule] = Seq.empty,
    main: Option[String] = None,
    dockerize: Boolean = false
  ): Project = {

    val home = sys.props("user.home")
    val env = parseEnvFile(new File(s"$home/.env")) ++
      parseEnvFile(new File(s"$home/.priva.env")) ++
      parseEnvFile(new File(s"$home/.priva.$id.env"))

    var project = Project(
      id = id,
      base = file(s"modules/$id"),
      settings = Defaults.coreDefaultSettings ++ sharedSettings
    ).settings(
      name := id,
      mainClass in run := main,
      libraryDependencies ++= deps,
      excludeDependencies ++= excludes,
      assemblyJarName in assembly := s"$id-bin.jar",
      envVars := env
    )
    if (dockerize) {
      project = addDockerSupport(project)
    }
    project
  }

  private def addDockerSupport(project: Project) = {
    project
      .enablePlugins(DockerPlugin)
      .settings(
        // Make the docker task depend on the assembly task, which generates a fat JAR file
        docker <<= (docker dependsOn assembly),

        // Generate a very basic Dockerfile running the JAR with Java 8.
        dockerfile in docker := {
          val artifact = (assemblyOutputPath in assembly).value
          val artifactTargetPath = s"/app/${artifact.name}"
          new Dockerfile {
            from("java:8")
            add(artifact, artifactTargetPath)
            volume("/data")
            expose(8888, 9990) // Finatra HTTP and admin ports
            entryPoint("java", "-jar", artifactTargetPath)
          }
        },

        imageNames in docker := Seq(
          // Sets the latest tag
          ImageName(s"${organization.value}/${name.value}:latest")
        )
      )
  }

  /**
   * Parse provided file in .env format.
   *
   * @param file .env file to read
   */
  private def parseEnvFile(file: File): Map[String, String] =
    if (!file.exists) {
      Map.empty
    } else {
      val source = Source.fromFile(file)
      val env = source.getLines()
        .filter(_.matches("^[a-zA-Z_]+[a-zA-Z0-9_]*=.*"))
        .map(line => line.split("=", 2)(0) -> line.split("=", 2)(1))
        .toMap
      source.close
      env
    }
}