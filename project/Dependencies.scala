import sbt._

object Dependencies {

  object V {
    val Guava = "18.0"
    val Findbugs = "2.0.3"
    val Guice = "4.0"
    val Finatra = "2.1.4"
    val Util = "6.30.0"
    val Commons = "3.3"
    val Jackson = "2.4.4"
    val Slf4j = "1.7.16"
    val Breeze = "0.12"
    val Junit = "4.12"
    val AwsSdk = "1.10.66"
    val Thrift = "0.8.0"
    val Scrooge = "4.6.0"
  }

  lazy val guavaDeps = Seq(
    //Guava has a `provided` dep on jsr/javax packages.
    "com.google.code.findbugs" % "jsr305" % V.Findbugs,
    "com.google.guava" % "guava" % V.Guava
  )
  lazy val guiceDeps = Seq(
    "com.google.inject" % "guice" % V.Guice,
    "com.google.inject.extensions" % "guice-multibindings" % V.Guice,
    "com.google.inject.extensions" % "guice-assistedinject" % V.Guice
  )
  lazy val scalaGuiceDeps = Seq("net.codingwell" %% "scala-guice" % "4.0.0")
  lazy val scalacticDeps = Seq("org.scalactic" %% "scalactic" % "2.2.6")
  lazy val awsSdkDeps = Seq("com.amazonaws" % "aws-java-sdk" % V.AwsSdk)
  lazy val sshJDeps = Seq("com.hierynomus" % "sshj" % "0.15.0", "com.jcraft" % "jzlib" % "1.1.3")
  lazy val utilCoreDeps = Seq("com.twitter" %% "util-core" % V.Util)
  lazy val utilAppDeps = Seq("com.twitter" %% "util-app" % V.Util)
  lazy val utilEvalDeps = Seq("com.twitter" %% "util-eval" % V.Util)
  lazy val utilCacheDeps = Seq("com.twitter" %% "util-cache" % V.Util)
  lazy val utilReflectDeps = Seq("com.twitter" %% "util-reflect" % V.Util)
  lazy val utilCollectionDeps = Seq("com.twitter" %% "util-collection" % V.Util)
  lazy val commonsMathDeps = Seq("org.apache.commons" % "commons-math3" % V.Commons)
  lazy val commonsLangDeps = Seq("org.apache.commons" % "commons-lang3" % V.Commons)
  lazy val commonsIoDeps = Seq("commons-io" % "commons-io" % "2.4")
  lazy val commonsDbcpDeps = Seq("org.apache.commons" % "commons-dbcp2" % "2.1.1")
  lazy val affinityDeps = Seq("com.higherfrequencytrading" % "affinity" % "1.7")
  lazy val mysqlDeps = Seq("mysql" % "mysql-connector-java" % "5.1.24")
  lazy val pgsqlDeps = Seq("org.postgresql" % "postgresql" % "9.4-1204-jdbc42")
  lazy val configDeps = Seq("com.typesafe" % "config" % "1.3.0")
  lazy val loggingDeps = Seq(
    "org.slf4j" % "slf4j-api" % V.Slf4j,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
  )
  lazy val daemonDeps = Seq("org.kohsuke" % "akuma" % "1.10")
  lazy val breezeDeps = Seq("org.scalanlp" %% "breeze" % V.Breeze)
  lazy val breezeNativeDeps = Seq("org.scalanlp" %% "breeze" % V.Breeze)
  lazy val jacksonDeps = Seq(
    "com.fasterxml.jackson.core" % "jackson-core" % V.Jackson,
    "com.fasterxml.jackson.core" % "jackson-annotations" % V.Jackson,
    "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % V.Jackson,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % V.Jackson
  )
  lazy val jodaTimeDeps = Seq(
    //Joda refers to joda-convert as optional, but mandatory in Scala.
    "joda-time" % "joda-time" % "2.5",
    "org.joda" % "joda-convert" % "1.2"
  )
  lazy val finatraDeps = Seq(
    "com.twitter.finatra" %% "finatra-http" % V.Finatra,
    "com.twitter.finatra" %% "finatra-httpclient" % V.Finatra,
    "com.twitter.finatra" %% "finatra-slf4j" % V.Finatra
  ).map(_.exclude("com.twitter.finatra", "finatra-scalap-compiler-deps_2.11"))
  // Conflict with scala-compiler. Maybe we should instead exclude the original scala-compiler dependency?
  // Cf. https://github.com/twitter/finatra/blob/1376bfdbba0ab/build.sbt#L398
  lazy val julBridgeDeps = Seq("org.slf4j" % "jul-to-slf4j" % V.Slf4j)
  lazy val logbackDeps = Seq("ch.qos.logback" % "logback-classic" % "1.1.5")
  lazy val thriftDeps = Seq(
    "org.apache.thrift" % "libthrift" % V.Thrift,
    "com.twitter" %% "scrooge-core" % V.Scrooge
  )
  lazy val testDeps = Seq(
    "junit" % "junit" % V.Junit,
    "org.scalatest" %% "scalatest" % "2.2.6",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2"
  )
}
