package plugin

import ammonite.ops._
import mill._
import mill.define._
import fansi.{Bold, Color, Str}
import ajr.util.Utils
import ajr.util.CollectionExtensions._
import mill.scalalib.{Dep, ScalaModule}
import mill.util.Ctx
import ajr.mill.dgraph.{DependencyProcessor, Results}
import ajr.mill.dgraph.DependencyProcessor._
import mill.eval.Evaluator
import upickle.default.{ReadWriter, macroRW}


object dgraph extends ExternalModule {
  case class DepData(title: String,
                     depsWithEvictions: Results,
                     depsWithoutEvictions: Results,
                     jarTotal: String, srcTotal: String)

  object DepData { implicit def rw: ReadWriter[DepData] = macroRW }


  def browseDeps(module: ScalaModule, overrideDeps: Dep*) = T{
    def transitiveModuleDeps(start: ScalaModule, parent: ScalaModule): Seq[(ScalaModule, ScalaModule)] = {
      (parent, start) +: start.moduleDeps.flatMap(mod => transitiveModuleDeps(mod, start))
    }

    val isOverride = overrideDeps.nonEmpty
    val (moduleGraph, allModules) =
      if (isOverride) {
        (Seq.empty[(ScalaModule, ScalaModule)], Seq.empty[ScalaModule])
      }
      else {
        val transitive = transitiveModuleDeps(module, module).distinct
        val moduleGraph = transitive.tail
        val allModules = transitive.flatMap(x => Seq(x._1, x._2)).distinct
        (moduleGraph, allModules)
      }
    T.task {
      val depMap: Map[ScalaModule, Agg[Dep]] =
        if (isOverride)
          Map(module -> Agg.from(overrideDeps))
        else
          allModules.zip(Task.traverse(allModules)(_.ivyDeps)()).toMap

      val repos = module.repositories
      val scalaVersion = module.scalaVersion()
      val platformSuffix = module.platformSuffix()

      val dp = new DependencyProcessor(moduleGraph, depMap, repos, scalaVersion, platformSuffix)

      val withEvictions = dp.calculate(includeEvictions = true)
      val withOutEvictions = dp.calculate(includeEvictions = false)

      val depData = DepData(module.toString,
        withEvictions, withOutEvictions,
        Utils.dataSize(withEvictions.jarTotal), Utils.dataSize(withEvictions.srcTotal))

      val depDataJson = upickle.default.write(depData,2)

      val outDir = pwd / 'out / 'plugins / 'dgraph
      mkdir(outDir)
      val webSrc = home / 'dev / 'scripts / "mill-deps" / 'dgraph
      for (file <- Seq("DepInfo.html", "DepInfo.css", "dagre-d3.js"))
        cp.over(webSrc / file, outDir / file)

      write.over(outDir / "DepData.js", s"var depData = $depDataJson")
      Utils.openInOS(outDir / "DepInfo.html")
    }
  }

  implicit def millScoptEvaluatorReads[T] = new mill.main.EvaluatorScopt[T]()
  def millDiscover = Discover[this.type]
}