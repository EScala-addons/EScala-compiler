package scala.tools.nsc
package interactive

import scala.util.control.ControlThrowable
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, WorkScheduler}
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** Interface of interactive compiler to a client such as an IDE
 *  The model the presentation compiler consists of the following parts:
 * 
 *  unitOfFile: The map from sourcefiles to loaded units. A sourcefile/unit is loaded if it occurs in that map.
 *  
 *  manipulated by: removeUnitOf, reloadSources.
 *  
 *  A call to reloadSources will add the given sources to the loaded units, and 
 *  start a new background compiler pass to compile all loaded units (with the indicated sources first).
 *  Each background compiler pass has its own typer run. 
 *  The background compiler thread can be interrupted each time an AST node is 
 *  completely typechecked in the following ways:
 
 *  1. by a new call to reloadSources. This starts a new background compiler pass with a new typer run.
 *  2. by a call to askTypeTree. This starts a new typer run if the forceReload parameter = true
 *  3. by a call to askTypeAt, askTypeCompletion, askScopeCompletion, askToDoFirst, askLinkPos, askLastType.
 *  4. by raising an exception in the scheduler.
 *  5. by passing a high-priority action wrapped in ask { ... }.
 *  
 *  Actions under 1-3 can themselves be interrupted if they involve typechecking
 *  AST nodes. High-priority actions under 5 cannot; they always run to completion.
 *  So these high-priority actions should to be short.
 *  
 *  Normally, an interrupted action continues after the interrupting action is finished.
 *  However, if the interrupting action created a new typer run, the interrupted
 *  action is aborted. If there's an outstanding response, it will be set to
 *  a Right value with a FreshRunReq exception.
 */
trait CompilerControl { self: Global =>

  import syntaxAnalyzer.UnitParser
 
  type Response[T] = scala.tools.nsc.interactive.Response[T]

  /** The scheduler by which client and compiler communicate
   *  Must be initialized before starting compilerRunner
   */
  protected[interactive] val scheduler = new WorkScheduler
  
  /** Return the compilation unit attached to a source file, or None
   *  if source is not loaded.
   */
  def getUnitOf(s: SourceFile): Option[RichCompilationUnit] = getUnit(s)
  
  /** Run operation `op` on a compilation unit assocuated with given `source`.
   *  If source has a loaded compilation unit, this one is passed to `op`.
   *  Otherwise a new compilation unit is created, but not added to the set of loaded units.
   */
  def onUnitOf[T](source: SourceFile)(op: RichCompilationUnit => T): T =
    op(unitOfFile.getOrElse(source.file, new RichCompilationUnit(source)))

  /** The compilation unit corresponding to a source file
   *  if it does not yet exist create a new one atomically
   *  Note: We want to get roid of this operation as it messes compiler invariants.
   */
  @deprecated("use getUnitOf(s) or onUnitOf(s) instead")
  def unitOf(s: SourceFile): RichCompilationUnit = getOrCreateUnitOf(s)
    
  /** The compilation unit corresponding to a position */
  @deprecated("use getUnitOf(pos.source) or onUnitOf(pos.source) instead")
  def unitOf(pos: Position): RichCompilationUnit = getOrCreateUnitOf(pos.source)

  /** Removes the CompilationUnit corresponding to the given SourceFile
   *  from consideration for recompilation.
   */ 
  def removeUnitOf(s: SourceFile): Option[RichCompilationUnit] = {  toBeRemoved += s.file; unitOfFile get s.file }

  /** Returns the top level classes and objects that were deleted
   * in the editor since last time recentlyDeleted() was called.
   */
  def recentlyDeleted(): List[Symbol] = deletedTopLevelSyms.synchronized {
    val result = deletedTopLevelSyms
    deletedTopLevelSyms.clear()
    result.toList
  }

  /** Locate smallest tree that encloses position
   *  @pre Position must be loaded
   */
  def locateTree(pos: Position): Tree = onUnitOf(pos.source) { unit => new Locator(pos) locateIn unit.body }
   
  /** Locates smallest context that encloses position as an optional value.
   */
  def locateContext(pos: Position): Option[Context] =
    for (unit <- getUnit(pos.source); cx <- locateContext(unit.contexts, pos)) yield cx

  /** Returns the smallest context that contains given `pos`, throws FatalError if none exists.
   */
  def doLocateContext(pos: Position): Context = locateContext(pos) getOrElse {
    throw new FatalError("no context found for "+pos)
  }
    
  /** Makes sure a set of compilation units is loaded and parsed.
   *  Returns () to syncvar `response` on completions.
   *  Afterwards a new background compiler run is started with
   *  the given sources at the head of the list of to-be-compiled sources.
   */
  def askReload(sources: List[SourceFile], response: Response[Unit]) = {
    val superseeded = scheduler.dequeueAll {
      case ri: ReloadItem if ri.sources == sources => Some(ri)
      case _ => None 
    }
    superseeded foreach (_.response.set())
    scheduler postWorkItem new ReloadItem(sources, response)
  }

  /** Sets sync var `response` to the smallest fully attributed tree that encloses position `pos`.
   *  Note: Unlike for most other ask... operations, the source file belonging to `pos` needs not be be loaded.
   */
  def askTypeAt(pos: Position, response: Response[Tree]) = 
    scheduler postWorkItem new AskTypeAtItem(pos, response)

  /** Sets sync var `response` to the fully attributed & typechecked tree contained in `source`.
   *  @pre `source` needs to be loaded.
   */
  def askType(source: SourceFile, forceReload: Boolean, response: Response[Tree]) = {
    if (debugIDE) {
      println("ask type called")
      new Exception().printStackTrace()
    }
    scheduler postWorkItem new AskTypeItem(source, forceReload, response)
  }

  /** Sets sync var `response` to the position of the definition of the given link in 
   *  the given sourcefile. 
   * 
   *  @param   sym      The symbol referenced by the link (might come from a classfile)
   *  @param   source   The source file that's supposed to contain the definition
   *  @param   response A response that will be set to the following:
   *                    If `source` contains a definition that is referenced by the given link
   *                    the position of that definition, otherwise NoPosition.
   *  Note: This operation does not automatically load `source`. If `source`
   *  is unloaded, it stays that way.
   */
  def askLinkPos(sym: Symbol, source: SourceFile, response: Response[Position]) = 
    scheduler postWorkItem new AskLinkPosItem(sym, source, response)
  
  /** Sets sync var `response' to list of members that are visible
   *  as members of the tree enclosing `pos`, possibly reachable by an implicit.
   *  @pre  source is loaded
   */
  def askTypeCompletion(pos: Position, response: Response[List[Member]]) = 
    scheduler postWorkItem new AskTypeCompletionItem(pos, response)

  /** Sets sync var `response' to list of members that are visible
   *  as members of the scope enclosing `pos`.
   *  @pre  source is loaded
   */
  def askScopeCompletion(pos: Position, response: Response[List[Member]]) = 
    scheduler postWorkItem new AskScopeCompletionItem(pos, response)

  /** Asks to do unit corresponding to given source file on present and subsequent type checking passes */
  def askToDoFirst(source: SourceFile) =
    scheduler postWorkItem new AskToDoFirstItem(source)

  /** If source is not yet loaded, loads it, and starts a new run, otherwise
   *  continues with current pass.
   *  Waits until source is fully type checked and returns body in response.
   *  @param source    The source file that needs to be fully typed.
   *  @param response  The response, which is set to the fully attributed tree of `source`.
   *                   If the unit corresponding to `source` has been removed in the meantime
   *                   the a NoSuchUnitError is raised in the response.
   */
  def askLoadedTyped(source: SourceFile, response: Response[Tree]) =
    scheduler postWorkItem new AskLoadedTypedItem(source, response)
  
  /** Build structure of source file. The structure consists of a list of top-level symbols
   *  in the source file, which might contain themselves nested symbols in their scopes.
   *  All reachable symbols are forced, i.e. their types are completed.
   *  @param source       The source file to be analyzed
   *  @param keepLoaded   If set to `true`, source file will be kept as a loaded unit afterwards.
   *                      If keepLoaded is `false` the operation is run at low priority, only after
   *                      everything is brought up to date in a regular type checker run.
   *  @param response     The response, which is set to the list of toplevel symbols found in `source`
   */
  def askStructure(source: SourceFile, keepLoaded: Boolean, response: Response[List[Symbol]]) =
    scheduler postWorkItem new AskStructureItem(source, keepLoaded, response)

  /** Cancels current compiler run and start a fresh one where everything will be re-typechecked
   *  (but not re-loaded).
   */
  def askReset() = scheduler raise FreshRunReq

  /** Tells the compile server to shutdown, and not to restart again */
  def askShutdown() = scheduler raise ShutdownReq
  
  @deprecated("use parseTree(source) instead") 
  def askParse(source: SourceFile, response: Response[Tree]) = respond(response) {
    parseTree(source)
  }
  
  /** Returns parse tree for source `source`. No symbols are entered. Syntax errors are reported.
   */
  def parseTree(source: SourceFile): Tree = ask { () =>
    getUnit(source) match { 
      case Some(unit) if unit.status >= JustParsed =>
        unit.body
      case _ =>
        new UnitParser(new CompilationUnit(source)).parse()
    }
  }
    
  /** Asks for a computation to be done quickly on the presentation compiler thread */
  def ask[A](op: () => A): A = scheduler doQuickly op
  
  /** Info given for every member found by completion
   */
  abstract class Member {
    val sym: Symbol 
    val tpe: Type
    val accessible: Boolean
  }

  case class TypeMember(
    sym: Symbol, 
    tpe: Type, 
    accessible: Boolean, 
    inherited: Boolean, 
    viaView: Symbol) extends Member

  case class ScopeMember(
    sym: Symbol, 
    tpe: Type, 
    accessible: Boolean, 
    viaImport: Tree) extends Member

  // items that get sent to scheduler
  
  abstract class WorkItem extends (() => Unit)

  case class ReloadItem(sources: List[SourceFile], response: Response[Unit]) extends WorkItem {
    def apply() = reload(sources, response)
    override def toString = "reload "+sources
  }

  class AskTypeAtItem(val pos: Position, response: Response[Tree]) extends WorkItem {
    def apply() = self.getTypedTreeAt(pos, response)
    override def toString = "typeat "+pos.source+" "+pos.show
  }

  class AskTypeItem(val source: SourceFile, val forceReload: Boolean, response: Response[Tree]) extends WorkItem {
    def apply() = self.getTypedTree(source, forceReload, response)
    override def toString = "typecheck"
  }

  class AskTypeCompletionItem(val pos: Position, response: Response[List[Member]]) extends WorkItem {
    def apply() = self.getTypeCompletion(pos, response)
    override def toString = "type completion "+pos.source+" "+pos.show
  }

  class AskScopeCompletionItem(val pos: Position, response: Response[List[Member]]) extends WorkItem {
    def apply() = self.getScopeCompletion(pos, response)
    override def toString = "scope completion "+pos.source+" "+pos.show
  }

  class AskToDoFirstItem(val source: SourceFile) extends WorkItem {
    def apply() = moveToFront(List(source))
    override def toString = "dofirst "+source
  }

  class AskLinkPosItem(val sym: Symbol, val source: SourceFile, response: Response[Position]) extends WorkItem {
    def apply() = self.getLinkPos(sym, source, response)
    override def toString = "linkpos "+sym+" in "+source
  }

  class AskLoadedTypedItem(val source: SourceFile, response: Response[Tree]) extends WorkItem {
    def apply() = self.waitLoadedTyped(source, response)
    override def toString = "wait loaded & typed "+source
  }
  
  class AskStructureItem(val source: SourceFile, val keepLoaded: Boolean, response: Response[List[Symbol]]) extends WorkItem {
    def apply() = self.buildStructure(source, keepLoaded, response)
    override def toString = "buildStructure "+source+", keepLoaded = "+keepLoaded
  }
}

  // ---------------- Interpreted exceptions -------------------

/** Signals a request for a fresh background compiler run.
 *  Note: The object has to stay top-level so that the PresentationCompilerThread may access it.
 */
object FreshRunReq extends ControlThrowable

/** Signals a request for a shutdown of the presentation compiler.
 *  Note: The object has to stay top-level so that the PresentationCompilerThread may access it.
 */
object ShutdownReq extends ControlThrowable

class NoSuchUnitError(file: AbstractFile) extends Exception("no unit found for file "+file)

