package dotty.tools.scaladoc

import dotty.tools.dotc.config.Settings.*
import dotty.tools.dotc.config.Properties.*
import dotty.tools.dotc.config.CliCommand
import dotty.tools.dotc.core.Contexts.Context

object ScaladocCommand extends CliCommand:
  type ConcreteSettings = ScaladocSettings
  override def cmdName: String = "scaladoc"
  override def versionMsg: String =
    s"Scaladoc $versionString -- $copyrightString"
  override def ifErrorsMsg: String = "  scaladoc -help  gives more information"

  def helpMsg(using
      settings: ScaladocSettings
  )(using SettingsState, Context): String =
    if settings.help.value then usageMessage
    else ""

  def isHelpFlag(using settings: ScaladocSettings)(using
      SettingsState
  ): Boolean =
    Set(settings.help) exists (_.value)
