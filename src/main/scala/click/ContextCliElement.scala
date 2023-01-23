package click

import click.exception.ClickException.OrParseErr
import click.util.Types.{Maybe, Label, CharLabel}
import scala.NonEmptyTuple

import click.util.Types

case class CliContextOption[
    Ctx,
    T,
    FullLabel <: Maybe[Label],
    ShortLabel <: Maybe[CharLabel],
    Req <: CliElement.IsRequired,
    Md <: CliOption.Mode,
    Ord <: ContextCliElement.EvalOrder,
](
    fullLabel: FullLabel,
    shortLabel: ShortLabel,
    default: CliOption.Default[T, Req, Md],
    parse: String => OrParseErr[T],
    update: (Ctx, CliOption.Result[T, Md]) => Ctx,
    override val description: Option[String],
) extends CliElement.ContextCliElement[Ctx]:
    def withDescription(description: String): CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord] =
        copy(description = Some(description))

    def withoutDescription: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord] =
        copy(description = None)

    def withFullLabel[L <: Label](label : L): CliContextOption[Ctx, T, L, ShortLabel, Req, Md, Ord] =
        copy(fullLabel = label)

    def withoutFullLabel: CliContextOption[Ctx, T, Unit, ShortLabel, Req, Md, Ord] = copy(fullLabel = ())

    def withShortLabel[L <: CharLabel](label : L): CliContextOption[Ctx, T, FullLabel, L, Req, Md, Ord] =
        copy(shortLabel = label)

    def withoutShortLabel: CliContextOption[Ctx, T, FullLabel, Unit, Req, Md, Ord] = copy(shortLabel = ())

case class CliContextFlag[
    Ctx,
    FullLabel <: Maybe[Label],
    ShortLabel <: Maybe[CharLabel],
    Ord <: ContextCliElement.EvalOrder,
](
    fullLabel: FullLabel,
    shortLabel: ShortLabel,
    update: (Ctx, Boolean) => Ctx,
    override val description: Option[String],
) extends CliElement.ContextCliElement[Ctx]:
    def withDescription(description: String): CliContextFlag[Ctx, FullLabel, ShortLabel, Ord] =
        copy(description = Some(description))

    def withoutDescription: CliContextFlag[Ctx, FullLabel, ShortLabel, Ord] =
        copy(description = None)

    def withFullLabel[L <: Label](label : L): CliContextFlag[Ctx, L, ShortLabel, Ord] =
        copy(fullLabel = label)

    def withoutFullLabel: CliContextFlag[Ctx, Unit, ShortLabel, Ord] = copy(fullLabel = ())

    def withShortLabel[L <: CharLabel](label : L): CliContextFlag[Ctx, FullLabel, L, Ord] =
        copy(shortLabel = label)

    def withoutShortLabel: CliContextFlag[Ctx, FullLabel, Unit, Ord] = copy(shortLabel = ())

case class CliContextOneOf[
    Ctx,
    Eles <: NonEmptyTuple,
](
    elements: Eles,
)(
    using
    CliContextOneOf.ValidEles[Ctx, Eles],
) extends CliElement.ContextCliElement[Ctx]:
    override def description: Option[String] = None

object CliContextOneOf:
    trait ValidEles[Ctx, Eles <: Tuple]
    object ValidEles:
        private inline def validCtxType[Ctx, Ele]: Unit =
            scala.compiletime.summonInline[Ele <:< CliElement.ContextCliElement[Ctx]]

        inline given validEles[Ctx, Eles <: Tuple]: ValidEles[Ctx, Eles] =
            inline scala.compiletime.erasedValue[Eles] match
                case _: (ele *: EmptyTuple) =>
                    validCtxType[Ctx, ele]
                    new ValidEles[Ctx, Eles] {}

                case _: (ele *: tail) =>
                    validCtxType[Ctx, ele]
                    validEles[Ctx, tail]
                    new ValidEles[Ctx, Eles] {}

object ContextCliElement:
    sealed trait EvalOrder
    sealed trait Eager extends EvalOrder
    sealed trait Lazy extends EvalOrder
    sealed trait Default extends EvalOrder

    case object Eager extends Eager
    case object Lazy extends Lazy
    case object Default extends Default
