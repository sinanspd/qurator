import sbt._

val scala3Version = "2.13.10"

val catsV          = "2.7.0"
val catsEffectV    = "3.3.12"
val catsRetryV    = "3.1.0"
val circeV        = "0.14.6"
val cirisV         = "2.3.2"
val derevoV       = "0.14.0"
val fs2V           = "3.7.0"
val http4sV        = "0.23.10"
val log4catsV      = "2.3.1"
val monocleV       = "3.1.0"
val newtypeV       = "0.4.4"
val refinedV       = "0.11.0"
val redis4catsV    = "1.1.1"
val skunkV         = "0.5.1"
val squantsV       = "1.8.3"
val logbackV          = "1.2.11"
//https://github.com/maginepro/http4s-aws

def circe(artifact: String): ModuleID  = "io.circe"   %% s"circe-$artifact"  % circeV
def ciris(artifact: String): ModuleID  = "is.cir"     %% artifact            % cirisV
def derevo(artifact: String): ModuleID = "tf.tofu"    %% s"derevo-$artifact" % derevoV
def http4s(artifact: String): ModuleID = "org.http4s" %% s"http4s-$artifact" % http4sV

val cats       = "org.typelevel"    %% "cats-core"   % catsV
val catsEffect = "org.typelevel"    %% "cats-effect" % catsEffectV
val catsRetry  = "com.github.cb372" %% "cats-retry"  % catsRetryV
val squants    = "org.typelevel"    %% "squants"     % squantsV
val fs2        = "co.fs2"           %% "fs2-core"    % fs2V
val newtype  = "io.estatico"   %% "newtype"        % newtypeV

val circeCore    = circe("core")
val circeGeneric = circe("generic")
val circeParser  = circe("parser")
val circeRefined = circe("refined")

val cirisCore    = ciris("ciris")
val cirisEnum    = ciris("ciris-enumeratum")
val cirisRefined = ciris("ciris-refined")

val derevoCore  = derevo("core")
val derevoCats  = derevo("cats")
val derevoCirce = derevo("circe-magnolia")

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

val betterMonadicForV = "0.3.1"
val kindProjectorV    = "0.13.2"
val organizeImportsV  = "0.6.0"
val semanticDBV       = "4.4.31"


val betterMonadicFor = compilerPlugin(
  "com.olegpy" %% "better-monadic-for" % betterMonadicForV
)
val kindProjector = compilerPlugin(
  "org.typelevel" % "kind-projector" % kindProjectorV cross CrossVersion.full
)
val semanticDB = compilerPlugin(
  "org.scalameta" % "semanticdb-scalac" % semanticDBV cross CrossVersion.full
)

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
    scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info"),
    version := "0.1.20-SNAPSHOT",
    crossScalaVersions := Seq("2.12.10"),
    scalaVersion := scala3Version,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= Seq(
     kindProjector,
     betterMonadicFor,
     //semanticDB,
     "org.typelevel" %% "spire" % "0.17.0",
     "org.jliszka" %% "probability-monad" % "1.0.4",
     catsEffect,
     cats,
     catsRetry,
     squants,
     fs2,
     circeCore,
     circeGeneric,
     circeParser,
     logback,
     circeRefined,
     cirisCore,
     cirisEnum,
     cirisRefined,
     derevoCore,
     derevoCats,
     derevoCirce,
     http4sDsl,
     http4sServer,
     newtype,
     http4sClient,
     http4sCirce,
     monocleCore,
     refinedCore,
     refinedCats,
     redis4catsEffects,
     redis4catsEffects,
     skunkCore,
     skunkCirce,
     "io.circe" %% "circe-generic-extras" % "0.14.3",
     "com.magine" %% "http4s-aws" % "6.2.1",
     //"com.sinanspd" %% "qure" % "0.1.20-SNAPSHOT"
    )
  )

addCommandAlias("runLinter", ";scalafixAll --rules OrganizeImports")