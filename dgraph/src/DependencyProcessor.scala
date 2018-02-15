package ajr.mill.dgraph

import coursier._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import DependencyProcessor._
import mill.scalalib.Lib.scalaBinaryVersion
import scalatags.Text.all._
import mill.util.Loose.Agg
import mill.scalalib.{ScalaModule,Dep}
import ajr.util.Utils._




object DependencyProcessor {
  val validType = Set("jar", "bundle", "src")

  sealed trait DepWrapper {
    def isProject: Boolean = false
    def depKey: String
  }
  case class GenericDependency(dep: Dependency) extends DepWrapper {
    override def depKey: String = s"${dep.module.organization}:${dep.module.name}:${dep.version}"
  }

  case class ProjectDep(module: ScalaModule) extends DepWrapper {
    override def isProject: Boolean = true
    override def depKey: String = module.toString // XXX .name
  }

  class RefProjectDep(module: ScalaModule) extends ProjectDep(module) {
  }


  def projectDep(module: ScalaModule): Dependency = Dependency(Module("", module.toString), "")
  def isProject(dep: Dependency) = dep.module.organization == "" && dep.version == ""

  def depKey(dep: Dependency) = {
    if (isProject(dep))
      dep.module.name
    else
      s"${dep.module.organization}:${dep.module.name}:${dep.version}"
  }

  def toDependency(dep: Dep, scalaVersion: String, platformSuffix: String): Dependency =
    dep match {
      case Dep.Java(dep, cross) =>
        dep.copy(
          module = dep.module.copy(
            name =
              dep.module.name +
                (if (!cross) "" else platformSuffix)
          )
        )
      case Dep.Scala(dep, cross) =>
        dep.copy(
          module = dep.module.copy(
            name =
              dep.module.name +
                (if (!cross) "" else platformSuffix) +
                "_" + scalaBinaryVersion(scalaVersion)
          )
        )
      case Dep.Point(dep, cross) =>
        dep.copy(
          module = dep.module.copy(
            name =
              dep.module.name +
                (if (!cross) "" else platformSuffix) +
                "_" + scalaVersion
          )
        )
    }
}





import upickle.default.{ReadWriter, macroRW}

case class ArtifactInfo(link: String, file: String, size: Long, sizeStr: String)
sealed trait GraphElement
case class DepNode(key: String, org: String, name: String, version: String,
                   artifacts: Map[String, ArtifactInfo],
                   isProject: Boolean, info: String, evicted: Boolean) extends GraphElement
case class DepEdge(from: String, to: String, eviction: Boolean) extends GraphElement
case class Results(nodes: Map[String, DepNode], edges: Seq[DepEdge], jarTotal: Long, srcTotal: Long)


object ArtifactInfo { implicit def rw: ReadWriter[ArtifactInfo] = macroRW }
object DepNode {      implicit def rw: ReadWriter[DepNode] = macroRW }
object DepEdge {      implicit def rw: ReadWriter[DepEdge] = macroRW }
object GraphElement { implicit def rw: ReadWriter[GraphElement] = macroRW }
object Results {      implicit def rw: ReadWriter[Results] = macroRW }


class DependencyProcessor(moduleGraph: Seq[(ScalaModule,ScalaModule)],
                          depMap: Map[ScalaModule,Agg[Dep]],
                          repos: Seq[Repository], scalaVersion: String, platformSuffix: String)
{
  val allModules = moduleGraph.flatMap(x => Seq(x._1, x._2)).distinct
  val allDependencies = depMap.values.flatten.map(toDependency(_, scalaVersion, platformSuffix)).toSet
  //allDependencies.foreach(println)

  val allCompileDependencies = allDependencies.map(_.copy(configuration = "compile"))
  val start = Resolution(allCompileDependencies)
  val fetch = Fetch.from(repos, Cache.fetch())
  val resolution = start.process.run(fetch).unsafePerformSync

  def calculate(includeEvictions: Boolean): Results = {
    val sources = true
    val artifactList =
      ((if (sources) resolution.dependencyClassifiersArtifacts(Seq("sources")) else Seq()) ++
        resolution.dependencyArtifacts)
        .filter(da => validType(da._2.attributes.`type`))

    implicit val sg = new Semigroup[ArtifactInfo] {
      def append(a: ArtifactInfo, b: => ArtifactInfo) = b
    }

    def simpleType(typ: String): String = if (typ == "bundle") "jar" else typ

    val artifacts =
      Task.gatherUnordered(artifactList.map { case (dep, art) =>
        Cache.file(art).map(file =>
          Map(depKey(dep) ->
            Map(simpleType(art.attributes.`type`) ->
              ArtifactInfo(file.getAbsolutePath, file.getName, file.length, dataSize(file.length))))
        ).run
      })
        .unsafePerformSync
        .flatMap(_.toOption)

    val artifactMap = artifacts.foldLeft(Map[String, Map[String, ArtifactInfo]]())(_ |+| _)
    val sizes = artifacts
      .flatMap { a => a.map { case (key, m) => m.map { case (typ, ai) => (typ, ai.size) } } }
      .foldLeft(Map[String, Long]("jar" -> 0L, "src" -> 0L))(_ |+| _)

    val jarTotal = sizes("jar")
    val srcTotal = sizes("src")

//    println("artifactMap")
//    artifactMap.foreach(println)


    def artifactInfo(dep: Dependency, artInfo: Map[String, ArtifactInfo], evicted: Boolean): String = {
      val arts = artInfo.keys.toList.sorted.map { classifier =>
        val art = artInfo(classifier)
        tr(td(classifier.capitalize), td(a(href := art.link, s"${art.file} (${art.sizeStr})")))
      }

      if (isProject(dep)) {
        List(
          table(
            tr(td("Project"), td(dep.module.name))
          )
        ).render
      }
      else {
        List(
          table(
            tr(td("Name"), td(dep.module.name + (if (evicted) " (evicted)" else ""))),
            tr(td("Org"), td(dep.module.organization)),
            tr(td("Version"), td(dep.version)),
            arts
          )
        ).render
      }
    }

    def toDepNode(dep: Dependency, evicted: Boolean = false) = {
      val arts = artifactMap.getOrElse(depKey(dep), Map.empty)
      val info = artifactInfo(dep, arts, evicted)
      DepNode(depKey(dep), dep.module.organization, dep.module.name, dep.version, arts, isProject(dep), info, evicted = evicted)
    }

    def calcDependencies0(srcDep: Dependency, dstDeps: List[Dependency]): List[GraphElement] = {
      //println(s"calc0: srcDep=$srcDep dstDeps: $dstDeps")

      def updateDependency(dep: Dependency): Dependency = {
        val version = resolution.reconciledVersions(dep.module)
        dep.copy(version = version)
      }

      val srcNode = toDepNode(srcDep)

      srcNode :: dstDeps.flatMap { dep =>
        val updDep = updateDependency(dep)
        val updNode = toDepNode(updDep)

        val evicted = dep != updDep
        val depNode = toDepNode(dep, evicted)

        val elements = if (evicted && includeEvictions) {
          val depEdge = DepEdge(srcNode.key, depNode.key, eviction = false)
          val evictionEdge = DepEdge(depNode.key, updNode.key, eviction = true)
          List(depNode, updNode, depEdge, evictionEdge)
        }
        else {
          val resolvedEdge = DepEdge(srcNode.key, updNode.key, eviction = false)
          List(updNode, resolvedEdge)
        }

        val subDeps = resolution.finalDependenciesCache.getOrElse(updDep, Nil).toList
        srcNode :: elements ::: calcDependencies0(updDep, subDeps)
      }
    }


    val initialDependencies = resolution.rootDependencies.toList.map(_.copy(configuration = "compile"))
    val elements = depMap.flatMap{case(module, deps) =>
      val compileDeps =
        deps.map(toDependency(_, scalaVersion, platformSuffix))
          .map(_.copy(configuration = "compile"))
          .toList
      calcDependencies0(projectDep(module), compileDeps)
    }
    val moduleEdges = moduleGraph.map{case(from,to) =>
      DepEdge(depKey(projectDep(from)), depKey(projectDep(to)), eviction = false)
    }
    val nodes = elements.collect { case n: DepNode => n }.toSeq.distinct.map(n => n.key -> n).toMap
    val edges = elements.collect { case e: DepEdge => e }.toSeq.distinct.sortBy(_.from)

    Results(nodes, edges ++ moduleEdges, jarTotal, srcTotal)
  }
}
