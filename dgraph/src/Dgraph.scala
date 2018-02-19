package plugin

import ammonite.ops._
import mill._
import mill.define._
import fansi.{Bold, Color, Str}
import ajr.util.Utils
import ajr.util.CollectionExtensions._
import mill.scalalib.{Dep, ScalaModule}
import mill.util.Strict
import ajr.mill.dgraph.{DependencyProcessor, Results}
import ajr.mill.dgraph.DependencyProcessor._
import mill.eval.Evaluator
import upickle.default.{ReadWriter, macroRW}
import mill.eval.Result

import scala.util.Try

object dgraph extends ExternalModule {
  case class DepData(title: String,
                     depsWithEvictions: Results,
                     depsWithoutEvictions: Results,
                     jarTotal: String, srcTotal: String)

  object DepData { implicit def rw: ReadWriter[DepData] = macroRW }

  implicit class ScalaModuleExtensions(module: Module) {
    def name = module.toString
  }

  def transitiveModules(module: Module): Seq[Module] = {
    module +: module.millModuleDirectChildren.flatMap(transitiveModules)
  }

  def browseEm(ev: Evaluator[Unit], moduleName: String, overrideDepNames: String*) = T.command{
    overrideDepNames.foreach(println)
    transitiveModules(ev.rootModule).find(_.name == moduleName) match {
      case None =>
        Result.Failure(s"- module '$moduleName' not found")

      case Some(module: ScalaModule) =>
        val depVals =
          overrideDepNames
            .map(_.replaceFirst("^(ivy)?", "").replaceAll("[\"' ]", "").replace('%', ':'))
            .map(d => Try{ scalalib.Dep.parse(d)}.toEither.left.map(_ => d))

        val errs = depVals.collect{case Left(name) => name}
        if (errs.isEmpty) {
          val deps = depVals.collect{case Right(mod) => mod}
          println(s"browseDeps: ${module.name}, deps=$deps")
          val task = browseDeps(module, deps: _*)
          val res = ev.evaluate(Strict.Agg(task))
          println(res.failing) // XXX fix me - check and display errors
//          println("results", res)
          Result.Success()
        }
        else {
          Result.Failure(s"Unable to parse module(s): ${errs.mkString(", ")}")
        }

      case Some(module) =>
        Result.Failure(s"Module: ${module.name} is not a scala module")
    }
  }



  private def transitiveModuleDeps(start: ProjectDep, parent: ProjectDep): Seq[(ProjectDep, ProjectDep)] = {
    (parent, start) +: start.module.moduleDeps.flatMap(mod => transitiveModuleDeps(ProjectDep(mod), start))
  }

  private def moduleResolveContext(module: ScalaModule) = T.task {
    ModuleResolveContext(module.repositories, module.scalaVersion(), module.platformSuffix())
  }

  private def moduleInfo(tDeps: Seq[(ProjectDep, ProjectDep)]) = {
    (tDeps.tail, tDeps.flatMap(x => Seq(x._1, x._2)).distinct)
  }

  private def extractDependencies(x: (Seq[(ProjectDep, ProjectDep)], Seq[ProjectDep])) = T.task {
    (x, x._2.zip(Task.traverse(x._2)(_.module.ivyDeps)()).toMap: Map[DepWrapper, Agg[Dep]])
  }

  private def modulesIvyDependencies(modules: Seq[ProjectDep]) = T.task{
    Task.traverse(modules)(_.module.ivyDeps)
  }


  def browseDeps(module: ScalaModule, overrideDeps: Dep*) = T.task {
    // need to specify the class loader if being called via a command from the command line
    val resourceRoot = resource(dgraph.getClass.getClassLoader)
    println("browseDeps")

    val resolveContext = moduleResolveContext(module)()

    val ((moduleGraph, modules), depMap) =
      if (overrideDeps.isEmpty)
        extractDependencies(moduleInfo(transitiveModuleDeps(ProjectDep(module), ProjectDep(module))))()
      else {
        ((Seq.empty[(DepWrapper, DepWrapper)], Seq.empty[ProjectDep]), Map[DepWrapper,Agg[Dep]](RefProjectDep(module) -> Agg.from(overrideDeps)))
      }
    //    println(moduleGraph)
    //    println(modules)
    //    println(depMap)
    //    println(resolveContext)
    val dp = new DependencyProcessor(moduleGraph, depMap, resolveContext)

    val withEvictions = dp.calculate(includeEvictions = true)
    val withOutEvictions = dp.calculate(includeEvictions = false)

    val depData = DepData(module.toString,
      withEvictions, withOutEvictions,
      Utils.dataSize(withEvictions.jarTotal), Utils.dataSize(withEvictions.srcTotal))

    val depDataJson = upickle.default.write(depData, 2)

    val outDir = pwd / 'out / 'plugins / 'dgraph
    mkdir(outDir)
    for (file <- Seq("DepInfo.html", "DepInfo.css", "dagre-d3.js")) {
      write.over(outDir / file, read(resourceRoot / file))
    }

    write.over(outDir / "DepData.js", s"var depData = $depDataJson")
    Utils.openInOS(outDir / "DepInfo.html")
  }

  implicit def millScoptEvaluatorReads[T] = new mill.main.EvaluatorScopt[T]()
  def millDiscover = Discover[this.type]
}