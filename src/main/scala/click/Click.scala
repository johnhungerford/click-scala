package click

import click.util.Types.*
import scala.compiletime.*
import click.util.Util
import Util.extensions.*

object Click:
    type IsLabel[T <: String & Singleton] = T
    type IsCharLabel[T <: Char & Singleton] = T

    lazy val command = CliCommand[Unit, Unit, EmptyTuple, EmptyTuple, CliCommand.ByDefOrder](EmptyTuple, EmptyTuple, identity, None)
    lazy val commandGroup = CliCommandGroup[Unit, Unit, EmptyTuple, EmptyTuple, EmptyTuple, CliCommand.ByDefOrder](EmptyTuple, EmptyTuple, EmptyTuple, identity, None)

    transparent inline def oneOf[T, Ele <: CliElement.DirectCliElement[T]](first: Ele): CliOneOf[T, Ele *: EmptyTuple] =
        CliOneOf(elements = first *: EmptyTuple, handle = (params) => Util.convertType[CliOneOf.UnionType[Ele *: EmptyTuple], T](params), None)

    transparent inline def allOf[T, Ele <: CliElement.DirectCliElement[T]](first: Ele): CliAllOf[T, T *: EmptyTuple, Ele *: EmptyTuple] =
        CliAllOf[T, T *: EmptyTuple, Ele *: EmptyTuple](elements = first *: EmptyTuple, handle = params => Util.convertType[T *: EmptyTuple, T *: EmptyTuple](params).head, None)(
            using CliAllOf.ValidParams.validParams[T *: EmptyTuple, Ele *: EmptyTuple],
        )

    def contextOneOf[Ctx, Ele <: CliElement.ContextCliElement[Ctx]](first: Ele): CliContextOneOf[Ctx, Ele *: EmptyTuple] =
        CliContextOneOf[Ctx, Ele *: EmptyTuple](elements = first *: EmptyTuple)
    
    transparent inline def stringOption[L <: Singleton](label: L): CliOption[String, ?, ?, CliElement.Required, CliOption.Single] =
        inline erasedValue[L] match
            case _: Char => inline erasedValue[L] match
                case _: IsCharLabel[sl] => CliOption[String, Unit, sl, CliElement.Required, CliOption.Single]((), label.inlineAs[sl], (), str => Right(str), None)
            case _: String => inline erasedValue[L] match
                case _: IsLabel[fl] => CliOption[String, fl, Unit, CliElement.Required, CliOption.Single](label.inlineAs[fl], (), (), str => Right(str), None)

    transparent inline def stringArgument[L <: Label](label: L): CliArgument[String, L, CliElement.Required, CliArgument.Single] =
        CliArgument(label, (), str => Right(str), None)

    case class CtxOptionPartiallyApplied[Ctx]():
        transparent inline def apply[L <: Singleton](label: L)(update: (Ctx, String) => Ctx): CliContextOption[Ctx, String, ?, ?, CliElement.Required, CliOption.Single, ContextCliElement.Default] =
            inline erasedValue[L] match
                case _: Char => inline erasedValue[L] match
                    case _: IsCharLabel[sl] => CliContextOption[Ctx, String, Unit, sl, CliElement.Required, CliOption.Single, ContextCliElement.Default]((), label.inlineAs[sl], (), str => Right(str), update, None)
                case _: String => inline erasedValue[L] match
                    case _: IsLabel[fl] => CliContextOption[Ctx, String, fl, Unit, CliElement.Required, CliOption.Single, ContextCliElement.Default](label.inlineAs[fl], (), (), str => Right(str), update, None)

    transparent inline def contextStringOption[Ctx]: CtxOptionPartiallyApplied[Ctx] =
        CtxOptionPartiallyApplied[Ctx]()
        
    transparent inline def flag[L <: Singleton](label: L): CliFlag[Boolean, ?, ?] =
        inline erasedValue[L] match
            case _: Char => inline erasedValue[L] match
                case _: IsCharLabel[sl] => CliFlag[Boolean, Unit, sl]((), Util.convertType[L, sl](label), identity, None)
            case _: String => inline erasedValue[L] match
                case _: IsLabel[fl] => CliFlag[Boolean, fl, Unit](Util.convertType[L, fl](label), (), identity, None)

    transparent inline def oneOfFlag[L <: Singleton, T](label: L, value: T): CliFlag[T, ?, ?] =
        inline erasedValue[L] match
            case _: Char => inline erasedValue[L] match
                case _: IsCharLabel[sl] => CliFlag[T, Unit, sl]((), label.inlineAs[sl], _ => value, None)
            case _: String => inline erasedValue[L] match
                case _: IsLabel[fl] => CliFlag[T, fl, Unit](label.inlineAs[fl], (), _ => value, None)

    transparent inline def oneOfByLabel[L <: Label, T](label: L): CliFlag[String, ?, ?] =
        oneOfFlag(label, label)

    case class CtxFlagPartiallyApplied[Ctx]():
        transparent inline def apply[L <: Singleton](label: L)(update: (Ctx, Boolean) => Ctx): CliContextFlag[Ctx, ?, ?, ContextCliElement.Default] =
            inline erasedValue[L] match
                case _: Char => inline erasedValue[L] match
                    case _: IsCharLabel[sl] => CliContextFlag[Ctx, Unit, sl, ContextCliElement.Default]((), label.inlineAs[sl], update, None)
                case _: String => inline erasedValue[L] match
                    case _: IsLabel[fl] => CliContextFlag[Ctx, fl, Unit, ContextCliElement.Default](label.inlineAs[fl], (), update, None)

    inline def contextFlag[Ctx]: CtxFlagPartiallyApplied[Ctx] = CtxFlagPartiallyApplied()

