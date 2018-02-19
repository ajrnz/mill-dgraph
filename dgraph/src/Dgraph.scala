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

import scala.collection.mutable
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

  def browseDeps(ev: Evaluator[Unit],
               moduleName: String,
               overrideDepNames: String*): mill.define.Command[Unit] = T.command
  {
    transitiveModules(ev.rootModule).find(_.name == moduleName) match {
      case None =>
        Result.Failure(s"- module '$moduleName' not found")

      case Some(module: ScalaModule) =>
        val task = browseDeps(module, overrideDepNames: _*)
        val results = ev.evaluate(Strict.Agg(task))
        taskErrorReport[Unit](results) match {
          case Right(v)  => Result.Success()
          case Left(msg) => Result.Failure(msg)
        }

      case Some(module) =>
        Result.Failure(s"Module: ${module.name} is not a scala module")
    }
  }

  private def validateStringDependencies(depStrs: Seq[String]): Either[String, Seq[Dep]] = {
    val depVals =
      depStrs
        .map(_.replaceFirst("^(ivy)?", "").replaceAll("[\"' ]", "").replace('%', ':'))
        .map(d => Try{ scalalib.Dep.parse(d)}.toEither.left.map(_ => d))

    val deps = depVals.collect{case Right(dep) => dep}
    val errs = depVals.collect{case Left(name) => name}
    if (errs.nonEmpty)
      Left(s"Unable to parse: ${errs.mkString(",")}")
    else
      Right(deps)
  }

  def taskErrorReport[V](results: Evaluator.Results): Either[String, V] = {
    results.values match{
      case Seq(head: V) => Right(head)
      case Nil =>
        val msg = new mutable.StringBuilder()
        msg.append(results.failing.keyCount + " targets failed\n")
        for((k, vs) <- results.failing.items){
          msg.append(k match{
            case Left(t) => "Anonymous Task\n"
            case Right(k) => k.segments.render + "\n"
          })

          for(v <- vs){
            v match{
              case Result.Failure(m, _) => msg.append(m + "\n")
              case Result.Exception(t, outerStack) =>
                msg.append(
                  t.toString +
                    t.getStackTrace.dropRight(outerStack.value.length).map("\n    " + _).mkString +
                    "\n"
                )

            }
          }
        }
        Left(msg.toString)
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


  def browseDeps(module: ScalaModule, overrideDepStrs: String*) = T.task {
    // need to specify the class loader if being called via a command from the command line

    val overrideDeps = validateStringDependencies(overrideDepStrs) match {
      case Right(deps) => deps
      case Left(msg) => throw new IllegalArgumentException(s"Invalid dependency format: $msg")
    }

    val resourceRoot = resource(dgraph.getClass.getClassLoader)

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
    for (file <- Seq("DepInfo.html", "DepInfo.css", "d3.js", "dagre-d3.js")) {
      write.over(outDir / file, read(resourceRoot / file))
    }

    write.over(outDir / "DepData.js", s"var depData = $depDataJson")
    Utils.openInOS(outDir / "DepInfo.html")
  }

  implicit def millScoptEvaluatorReads[T] = new mill.main.EvaluatorScopt[T]()
  def millDiscover = Discover[this.type]
}