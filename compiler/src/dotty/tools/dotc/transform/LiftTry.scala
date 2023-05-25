package dotty.tools.dotc
package transform

import MegaPhase.*
import core.DenotTransformers.*
import core.Symbols.*
import core.Contexts.*
import core.Types.*
import core.Flags.*
import core.Decorators.*
import core.NameKinds.LiftedTreeName
import NonLocalReturns.*
import util.Store

/** Lifts try's that might be executed on non-empty expression stacks to their
  * own methods. I.e.
  *
  * try body catch handler
  *
  * is lifted to
  *
  * { def liftedTree$n() = try body catch handler; liftedTree$n() }
  *
  * However, don't lift try's without catch expressions (try-finally). Lifting
  * is needed only for try-catch expressions that are evaluated in a context
  * where the stack might not be empty. `finally` does not attempt to continue
  * evaluation after an exception, so the fact that values on the stack are
  * 'lost' does not matter (copied from
  * https://github.com/scala/scala/pull/922).
  */
class LiftTry extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>
  import ast.tpd.*

  override def phaseName: String = LiftTry.name

  override def description: String = LiftTry.description

  private var NeedLift: Store.Location[Boolean] = _
  private def needLift(using Context): Boolean = ctx.store(NeedLift)

  override def initContext(ctx: FreshContext): Unit =
    NeedLift = ctx.addLocation(false)

  private def liftingCtx(p: Boolean)(using Context) =
    if needLift == p then ctx else ctx.fresh.updateStore(NeedLift, p)

  override def prepareForApply(tree: Apply)(using Context): Context =
    liftingCtx(true)

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    liftingCtx(false)

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if !tree.symbol.exists
      || tree.symbol.isSelfSym
      || tree.symbol.owner == ctx.owner.enclosingMethod
      && !tree.symbol.is(Lazy)
      // The current implementation wraps initializers of lazy vals in
      // calls to an initialize method, which means that a `try` in the
      // initializer needs to be lifted. Note that the new scheme proposed
      // in #6979 would avoid this.
    then ctx
    else liftingCtx(true)

  override def prepareForAssign(tree: Assign)(using Context): Context =
    if tree.lhs.symbol.maybeOwner == ctx.owner.enclosingMethod then ctx
    else liftingCtx(true)

  override def prepareForReturn(tree: Return)(using Context): Context =
    if !isNonLocalReturn(tree) then ctx
    else liftingCtx(true)

  override def prepareForTemplate(tree: Template)(using Context): Context =
    liftingCtx(false)

  override def transformTry(tree: Try)(using Context): Tree =
    if needLift && tree.cases.nonEmpty then
      report.debuglog(
        i"lifting tree at ${tree.span}, current owner = ${ctx.owner}"
      )
      val fn = newSymbol(
        ctx.owner,
        LiftedTreeName.fresh(),
        Synthetic | Method,
        MethodType(Nil, tree.tpe.widenIfUnstable),
        coord = tree.span
      )
      tree.changeOwnerAfter(ctx.owner, fn, thisPhase)
      Block(DefDef(fn, tree) :: Nil, ref(fn).appliedToNone)
    else tree
end LiftTry
object LiftTry:
  val name = "liftTry"
  val description: String =
    "lift any try that might be executed on a non-empty expression stack"
