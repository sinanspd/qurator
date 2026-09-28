import sbt._

val scala3Version = "3.3.8"

val catsV          = "2.10.0"
val catsEffectV    = "3.5.4"
val catsRetryV     = "3.1.3"
val catsDerivedV   = "3.5.0"
val circeV         = "0.14.6"
val cirisV         = "3.5.0"
val fs2V           = "3.10.2"
val http4sV        = "0.23.26"
val log4catsV      = "2.6.0"
val monocleV       = "3.2.0"
val refinedV       = "0.11.1"
val redis4catsV    = "1.6.0"
val skunkV         = "0.6.3"
val squantsV       = "1.8.3"
val logbackV       = "1.5.6"

def circe(artifact: String): ModuleID  = "io.circe"   %% s"circe-$artifact"  % circeV
def ciris(artifact: String): ModuleID  = "is.cir"     %% artifact            % cirisV
def http4s(artifact: String): ModuleID = "org.http4s" %% s"http4s-$artifact" % http4sV

val cats       = "org.typelevel"    %% "cats-core"   % catsV
val catsEffect = "org.typelevel"    %% "cats-effect" % catsEffectV
val catsRetry  = "com.github.cb372" %% "cats-retry"  % catsRetryV
val catsDerived = "org.typelevel"   %% "kittens"     % catsDerivedV
val squants    = "org.typelevel"    %% "squants"     % squantsV
val fs2        = "co.fs2"           %% "fs2-core"    % fs2V

val circeCore    = circe("core")
val circeGeneric = circe("generic")
val circeParser  = circe("parser")
val circeRefined = circe("refined")

val cirisCore    = ciris("ciris")
val cirisEnum    = ciris("ciris-enumeratum")
val cirisRefined = ciris("ciris-refined")

val http4sDsl    = http4s("dsl")
val http4sServer = http4s("ember-server")
val http4sClient = http4s("ember-client")
val http4sCirce  = http4s("circe")

val monocleCore = "dev.optics" %% "monocle-core" % monocleV

val refinedCore = "eu.timepit" %% "refined"      % refinedV
val refinedCats = "eu.timepit" %% "refined-cats" % refinedV

val redis4catsEffects  = "dev.profunktor" %% "redis4cats-effects"  % redis4catsV
val redis4catsLog4cats = "dev.profunktor" %% "redis4cats-log4cats" % redis4catsV

val skunkCore  = "org.tpolecat" %% "skunk-core"  % skunkV
val skunkCirce = "org.tpolecat" %% "skunk-circe" % skunkV

val logback = "ch.qos.logback" % "logback-classic" % logbackV

ThisBuild / evictionErrorLevel := Level.Warn

ThisBuild / assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}

lazy val root = project
  .in(file("."))
  .settings(
    assembly / mainClass := Some("qurator.DataPersitance"),
    name := "qure",
    organization := "com.sinanspd",
    scalaVersion := scala3Version,
    version := "0.1.20-SNAPSHOT",
    resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.18.0",
      "org.typelevel" %% "cats-mtl" % "1.7.0",
      catsEffect,
      cats,
      catsRetry,
      catsDerived,
      squants,
      fs2,
      circeCore,
      circeGeneric,
      circeParser,
      circeRefined,
      logback,
      cirisCore,
      cirisEnum,
      cirisRefined,
      http4sDsl,
      http4sServer,
      http4sClient,
      http4sCirce,
      monocleCore,
      refinedCore,
      refinedCats,
      redis4catsEffects,
      redis4catsLog4cats,
      skunkCore,
      skunkCirce,
      "org.typelevel" %% "weaver-cats" % "0.13.0" % Test,
      "org.typelevel" %% "log4cats-noop" % "2.8.0" % Test
    )
  )

addCommandAlias("runLinter", ";scalafixAll --rules OrganizeImports")