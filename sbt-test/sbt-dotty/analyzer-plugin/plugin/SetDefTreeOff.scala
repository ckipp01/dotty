package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.transform.MegaPhase.*
import dotty.tools.dotc.transform.{PickleQuotes, FirstTransform}
import dotty.tools.dotc.plugins.*

/** Unset the `defTree` property of symbols. See the doc for `SetDefTree` */
class SetDefTreeOff extends PluginPhase:
  import tpd.*

  override val phaseName: String = SetDefTreeOff.name
  override def runsAfter: Set[String] = Set(SetDefTree.name)
  override def runsBefore: Set[String] = Set(FirstTransform.name)

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree =
    tree.symbol.defTree = EmptyTree
    tree

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree =
    tree.symbol.defTree = EmptyTree
    tree

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree =
    tree.symbol.defTree = EmptyTree
    tree

object SetDefTreeOff:
  val name: String = "SetDefTreeOff"
