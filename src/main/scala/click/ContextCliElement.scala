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
) extends CliElement.ContextCliElement[Ctx]

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
) extends CliElement.ContextCliElement[Ctx]

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
