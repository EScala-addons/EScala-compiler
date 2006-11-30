/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc

import java.lang.System.getProperty
import scala.tools.nsc.util.FakePos //{Position}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.doc.DocGenerator


/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends AnyRef with EvalLoop {

  val PRODUCT: String = getProperty("scala.tool.name", "scalac")
  val VERSION: String = getProperty("scala.tool.version", "unknown version")
  val COPYRIGHT: String = getProperty("scala.copyright", "(c) 2002-2006 LAMP/EPFL")
  val versionMsg = PRODUCT + " " + VERSION + " -- " + COPYRIGHT
  val prompt = "\nnsc> "

  var reporter: ConsoleReporter = _

  def error(msg: String): unit =
    reporter.error(/*new Position */FakePos(PRODUCT),
                   msg + "\n  " + PRODUCT + " -help  gives more information")

  /* needed ?? */
  def errors() = reporter.errors

  def resident(compiler: Global): unit =
    loop { line =>
      val args = List.fromString(line, ' ')
      val command = new CompilerCommand(args, error, true)
      (new compiler.Run) compile command.files
    }

  def process(args: Array[String]): unit = {
    reporter = new ConsoleReporter();
    val command = new CompilerCommand(List.fromArray(args), error, false);
    reporter.prompt = command.settings.prompt.value;
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (command.settings.help.value)
      reporter.info(null, command.usageMsg, true)
    else {
      try {
        object compiler extends Global(command.settings, reporter);
        if (command.settings.resident.value)
          resident(compiler)
        else if (command.files.isEmpty)
          reporter.info(null, command.usageMsg, true)
        else {
          val run = new compiler.Run
          run compile command.files
          if (command.settings.doc.value) {
            object generator extends DocGenerator {
              val global : compiler.type = compiler
              def outdir = command.settings.outdir.value
              def windowTitle = command.settings.windowtitle.value
              def documentTitle = command.settings.documenttitle.value
            };
            generator.process(run.units)
          }
        }
      } catch {
        case ex @ FatalError(msg) =>
          if (command.settings.debug.value)
            ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
      }
      reporter.printSummary()
    }
  }

  def main(args: Array[String]): unit = {
    process(args)
    exit(if (reporter.hasErrors) 1 else 0)
  }

}
