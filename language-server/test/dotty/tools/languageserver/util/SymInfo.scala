package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.PositionContext.*
import org.eclipse.lsp4j.*

class SymInfo(
    name: String,
    kind: SymbolKind,
    range: CodeRange,
    container: String
):
  def toSymInformation: PosCtx[SymbolInformation] =
    new SymbolInformation(name, kind, range.toLocation, container)

  def show: PosCtx[String] =
    s"SymInfo($name, $kind, ${range.show}, $container)"
  override def toString: String =
    s"SymInfo($name, $kind, $range, $container)"
