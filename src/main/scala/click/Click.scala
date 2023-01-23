package click

import click.util.Types.*
import scala.compiletime.*
import click.builder.CommandBuilder

object Click:
    type IsLabel[T <: String & Singleton] = T
    type IsCharLabel[T <: Char & Singleton] = T

    lazy val buildCommand = CommandBuilder.empty

    transparent inline def stringOption[L <: Singleton](label: L): CliOption[String, ?, ?, CliElement.Required, CliOption.Single] =
        inline erasedValue[L] match
            case _: Char => inline erasedValue[L] match
                case _: IsCharLabel[sl] => CliOption[String, Unit, sl, CliElement.Required, CliOption.Single]((), label.asInstanceOf[sl], (), str => Right(str), None)
            case _: String => inline erasedValue[L] match
                case _: IsLabel[fl] => CliOption[String, fl, Unit, CliElement.Required, CliOption.Single](label.asInstanceOf[fl], (), (), str => Right(str), None)

    transparent inline def stringArgument[L <: Label](label: L): CliArgument[String, L, CliElement.Required, CliArgument.Single] =
        CliArgument(label, (), str => Right(str), None)

    case class CtxOptionPartiallyApplied[Ctx]():
        transparent inline def apply[L <: Singleton](label: L)(update: (Ctx, CliOption.Result[String, CliOption.Single]) => Ctx): CliContextOption[Ctx, String, ?, ?, CliElement.Required, CliOption.Single, ContextCliElement.Default] =
            inline erasedValue[L] match
                case _: Char => inline erasedValue[L] match
                    case _: IsCharLabel[sl] => CliContextOption[Ctx, String, Unit, sl, CliElement.Required, CliOption.Single, ContextCliElement.Default]((), label.asInstanceOf[sl], (), str => Right(str), update, None)
                case _: String => inline erasedValue[L] match
                    case _: IsLabel[fl] => CliContextOption[Ctx, String, fl, Unit, CliElement.Required, CliOption.Single, ContextCliElement.Default](label.asInstanceOf[fl], (), (), str => Right(str), update, None)

    transparent inline def contextStringOption[Ctx]: CtxOptionPartiallyApplied[Ctx] =
        CtxOptionPartiallyApplied[Ctx]()
        
    transparent inline def flag[L <: Singleton](label: L): CliFlag[Boolean, ?, ?] =
        inline erasedValue[L] match
            case _: Char => inline erasedValue[L] match
                case _: IsCharLabel[sl] => CliFlag[Boolean, Unit, sl]((), label.asInstanceOf[sl], identity, None)
            case _: String => inline erasedValue[L] match
                case _: IsLabel[fl] => CliFlag[Boolean, fl, Unit](label.asInstanceOf[fl], (), identity, None)
        

    case class CtxFlagPartiallyApplied[Ctx]():
        transparent inline def apply[L <: Singleton](label: L)(update: (Ctx, Boolean) => Ctx): CliContextFlag[Ctx, ?, ?, ContextCliElement.Default] =
            inline erasedValue[L] match
                case _: Char => inline erasedValue[L] match
                    case _: IsCharLabel[sl] => CliContextFlag[Ctx, Unit, sl, ContextCliElement.Default]((), label.asInstanceOf[sl], update, None)
                case _: String => inline erasedValue[L] match
                    case _: IsLabel[fl] => CliContextFlag[Ctx, fl, Unit, ContextCliElement.Default](label.asInstanceOf[fl], (), update, None)

    inline def contextFlag[Ctx]: CtxFlagPartiallyApplied[Ctx] = CtxFlagPartiallyApplied()