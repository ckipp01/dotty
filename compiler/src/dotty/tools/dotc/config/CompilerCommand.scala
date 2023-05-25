package dotty.tools.dotc
package config

import Settings.*
import core.Contexts.*

abstract class CompilerCommand extends CliCommand:
  type ConcreteSettings = ScalaSettings

  final def helpMsg(using
      settings: ScalaSettings
  )(using SettingsState, Context): String =
    settings.allSettings.find(isHelping) match
      case Some(s) => s.description
      case _ =>
        if settings.help.value then usageMessage
        else if settings.Vhelp.value then vusageMessage
        else if settings.Whelp.value then wusageMessage
        else if settings.Xhelp.value then xusageMessage
        else if settings.Yhelp.value then yusageMessage
        else if settings.showPlugins.value then ctx.base.pluginDescriptions
        else if settings.XshowPhases.value then phasesMessage
        else ""

  final def isHelpFlag(using settings: ScalaSettings)(using
      SettingsState
  ): Boolean =
    import settings.*
    val flags = Set(help, Vhelp, Whelp, Xhelp, Yhelp, showPlugins, XshowPhases)
    flags.exists(_.value) || allSettings.exists(isHelping)
