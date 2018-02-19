import mill._
import mill.scalalib._, publish._
import ammonite.ops._

trait DgraphPublishModule extends PublishModule{
  def scalaVersion = "2.12.4"
  def artifactName = "mill-dgraph"
  def publishVersion = "0.1"

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.ajr",
    url = "https://github.com/ajrnz/mill-dgraph",
    licenses = Seq(
      License("MIT license", "http://www.opensource.org/licenses/mit-license.php")
    ),
    scm = SCM(
      "git://github.com/ajrnz/mill-dgraph.git",
      "scm:git://github.com/ajrnz/mill-dgraph.git"
    ),
    developers = Seq(
      Developer("ajrnz", "Andrew Richards", "https://github.com/ajrnz")
    )
  )
}

object dgraph extends DgraphPublishModule {
  def scalaVersion = "2.12.4"

  def dagre_d3 = T{ "https://cdnjs.cloudflare.com/ajax/libs/dagre-d3/0.6.1/dagre-d3.min.js" }
  def d3 = T{ "https://cdnjs.cloudflare.com/ajax/libs/d3/4.13.0/d3.min.js" }

  // disable docs generation
  def docsJar() = T{
    PathRef(T.ctx().dest)
  }

  def generatedResources = T{
    mill.modules.Util.download(dagre_d3(), "dagre-d3.js")
    mill.modules.Util.download(d3(), "d3.js")
    PathRef(T.ctx().dest)
  }

  def resources = T.sources{ super.resources() :+ generatedResources() }

  val millVersion = "0.1.0"

  def compileIvyDeps = Agg(
    ivy"com.lihaoyi::mill-main:$millVersion",
    ivy"com.lihaoyi::geny:0.1.2",
  )

  def ivyDeps = Agg(
  	ivy"com.lihaoyi::scalatags:0.6.7",
  )

}
