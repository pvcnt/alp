import Commons._
import Dependencies._

lazy val privamovCore = module(
  "privamov-core",
  deps = breezeDeps ++ jacksonDeps ++ thriftDeps
).dependsOn(utilCore, s2, utilTesting % Test)

lazy val privamovRuntime = module(
  "privamov-runtime",
  deps = jacksonDeps ++ logbackDeps
).dependsOn(privamovCore, utilFlags, utilTesting % Test)

lazy val utilCore = module("util-core", deps = utilCoreDeps).dependsOn(utilTesting % Test)
lazy val utilFlags = module("util-flags").dependsOn(utilCore, utilTesting % Test)
lazy val utilTesting = module("util-testing", deps = testDeps)
lazy val s2 = module("s2").dependsOn(utilTesting % Test)