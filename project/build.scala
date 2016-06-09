import sbt._
import Keys._
import org.scalatra.sbt._
import org.scalatra.sbt.PluginKeys._
import com.mojolly.scalate.ScalatePlugin._
import ScalateKeys._

object YuzuBuild extends Build {
  val Organization = "org.insightcentre"
  val Name = "ColloqWN Editor"
  val Version = "0.0.1-SNAPSHOT"
  val ScalaVersion = "2.10.6"
  val ScalatraVersion = "2.4.0"

  lazy val project = Project (
    "cwneditor",
    file("."),
    settings = ScalatraPlugin.scalatraSettings ++ scalateSettings ++ Seq(
      organization := Organization,
      name := Name,
      version := Version,
      scalaVersion := ScalaVersion,
      resolvers += Classpaths.typesafeReleases,
      resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
      libraryDependencies ++= Seq(
        "org.scalatra" %% "scalatra" % ScalatraVersion
          excludeAll(ExclusionRule(organization="org.slf4j")),
        "org.scalatra" %% "scalatra-scalate" % ScalatraVersion
          excludeAll(ExclusionRule(organization="org.slf4j")),
        "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test"
          excludeAll(ExclusionRule(organization="org.slf4j")),
//        "ch.qos.logback" % "logback-classic" % "1.1.1" % "runtime",
        "org.eclipse.jetty" % "jetty-webapp" % "9.2.10.v20150310" % "container"
          excludeAll(ExclusionRule(organization="org.slf4j")),
        "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
        "io.spray" %% "spray-json" % "1.3.2",
        "org.slf4j" % "slf4j-nop" % "1.7.13"
      ),
      scalateTemplateConfig in Compile <<= (sourceDirectory in Compile){ base =>
        Seq(
          TemplateConfig(
            base / "webapp" / "WEB-INF" / "templates",
            Seq.empty,  /* default imports should be added here */
            Seq(
              Binding("context", "_root_.org.scalatra.scalate.ScalatraRenderContext", importMembers = true, isImplicit = true)
            ),  /* add extra bindings here */
            Some("templates")
          )
        )
      }
    )
  )
}
