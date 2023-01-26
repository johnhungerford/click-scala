package click

import click.exception.ClickException.OrParseErr
import click.util.Types.{Maybe, Label, CharLabel}
import scala.NonEmptyTuple

import click.util.Types
import click.CliElement.Required
import scala.util.NotGiven
import click.exception.ClickException
import click.util.Util.extensions.*

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
    update: (Ctx, CliOption.Result[T, Req, Md]) => Ctx,
    override val description: Option[String],
) extends CliElement.ContextCliElement[Ctx]:
    def withDescription(description: String): CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord] =
        copy(description = Some(description))

    def withoutDescription: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord] =
        copy(description = None)

    transparent inline def withFullLabel[L <: Label](label : L): CliContextOption[Ctx, T, L, ShortLabel, Req, Md, Ord] =
        copy(fullLabel = label)

    transparent inline def withoutFullLabel: CliContextOption[Ctx, T, Unit, ShortLabel, Req, Md, Ord] = copy(fullLabel = ())

    transparent inline def withShortLabel[L <: CharLabel](label : L): CliContextOption[Ctx, T, FullLabel, L, Req, Md, Ord] =
        copy(shortLabel = label)

    transparent inline def withoutShortLabel: CliContextOption[Ctx, T, FullLabel, Unit, Req, Md, Ord] = copy(shortLabel = ())

    transparent inline def map[U](fn: T => U)(update: (Ctx, CliOption.Result[U, Req, Md]) => Ctx): CliContextOption[Ctx, U, FullLabel, ShortLabel, Req, Md, Ord] =
        import scala.compiletime.*
        val newDefault: CliOption.Default[U, Req, Md] = inline erasedValue[CliOption.Default[T, Req, Md]] match
            case _: Unit => ().inlineAs[CliOption.Default[U, Req, Md]]
            case _: T =>
                fn(default.inlineAs[T]).inlineAs[CliOption.Default[U, Req, Md]]
            case _: List[T] =>
                default.inlineAs[List[T]].map(fn).inlineAs[CliOption.Default[U, Req, Md]]
        
        copy[Ctx, U, FullLabel, ShortLabel, Req, Md, Ord](parse = (str) => parse(str).map(fn), default = newDefault, update = update)

    inline def mapParse[U](fn: T => OrParseErr[U])(update: (Ctx, CliOption.Result[U, Req, Md]) => Ctx): CliContextOption[Ctx, U, FullLabel, ShortLabel, Req, Md, Ord] =
        import scala.compiletime.*
        inline erasedValue[CliOption.Default[T, Req, Md]] match
            case _: Unit =>
                copy[Ctx, U, FullLabel, ShortLabel, Req, Md, Ord](parse = (str) => parse(str).flatMap(fn), default = default.inlineAs[CliOption.Default[U, Req, Md]], update = update)
            case _ => error("Unable to map parse function on an option with a default value")

    inline def withValidation(validate: T => Option[ClickException.ParseError]): CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord] =
        mapParse[T](t => validate(t) match
            case None => Right(t)
            case Some(err) => Left(err)
        )(update)

    inline def withPartialValidation(validatePartial: PartialFunction[T, ClickException.ParseError]): CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord] =
        withValidation(t => if (validatePartial.isDefinedAt(t)) Some(validatePartial(t)) else None)


object CliContextOption:

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  CliOption.Default[T, Req, Md] =:= CliOption.Default[T, Req, CliOption.Multi],
        ev2: NotGiven[CliOption.Result[T, Req, CliOption.Multi] =:= CliOption.Result[T, Req, Md]],
    )
        def multi(update: (Ctx, CliOption.Result[T, Req, CliOption.Multi]) => Ctx): CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, CliOption.Multi, Ord] =
            option.copy(default = ev(option.default), update = update)
    
    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  CliOption.Default[T, Req, Md] =:= CliOption.Default[T, Req, CliOption.Single],
        ev2: NotGiven[CliOption.Result[T, Req, CliOption.Single] =:= CliOption.Result[T, Req, Md]],
    )
        def single(update: (Ctx, CliOption.Result[T, Req, CliOption.Single]) => Ctx): CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, CliOption.Single, Ord] =
            option.copy(default = ev(option.default), update = update)

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.HasDefault, CliOption.Single, Ord])
        def multi(default: CliOption.Default[T, CliElement.HasDefault, CliOption.Multi], update: (Ctx, CliOption.Result[T, CliElement.HasDefault, CliOption.Multi]) => Ctx): CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.HasDefault, CliOption.Multi, Ord] =
            option.copy(default = default, update = update)
    
    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.HasDefault, CliOption.Multi, Ord])
        def single(default: CliOption.Default[T, CliElement.HasDefault, CliOption.Single], update: (Ctx, CliOption.Result[T, CliElement.HasDefault, CliOption.Single]) => Ctx): CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.HasDefault, CliOption.Single, Ord] =
            option.copy(default = default, update = update)

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  CliOption.Result[T, CliElement.Required, Md] =:= CliOption.Result[T, Req, Md],
    )
        def required: CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.Required, Md, Ord] =
            option.copy(default = (), update = (ctx, res) => option.update(ctx, ev(res)))

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  CliOption.Result[T, CliElement.Optional, Md] =:= CliOption.Result[T, Req, Md],
    )
        def optional: CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.Optional, Md, Ord] =
            option.copy(default = (), update = (ctx, res) => option.update(ctx, ev(res)))

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  CliOption.Result[T, CliElement.HasDefault, Md] =:= CliOption.Result[T, Req, Md],
    )
        def withDefault(default: CliOption.Default[T, CliElement.HasDefault, Md]): CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.HasDefault, Md, Ord] =
            option.copy(default = default, update = (ctx, res) => option.update(ctx, ev(res)))

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  NotGiven[CliOption.Result[T, CliElement.Required, Md] =:= CliOption.Result[T, Req, Md]],
    )
        def required(update: (Ctx, CliOption.Result[T, CliElement.Required, Md]) => Ctx): CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.Required, Md, Ord] =
            option.copy(default = (), update = update)

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  NotGiven[CliOption.Result[T, CliElement.Optional, Md] =:= CliOption.Result[T, Req, Md]],
    )
        def optional(update: (Ctx, CliOption.Result[T, CliElement.Optional, Md]) => Ctx): CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.Optional, Md, Ord] =
            option.copy(default = (), update = update)

    extension [
        Ctx,
        T,
        FullLabel <: Maybe[Label],
        ShortLabel <: Maybe[CharLabel],
        Req <: CliElement.IsRequired,
        Md <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](option: CliContextOption[Ctx, T, FullLabel, ShortLabel, Req, Md, Ord])(
        using
        ev:  NotGiven[CliOption.Result[T, CliElement.HasDefault, Md] =:= CliOption.Result[T, Req, Md]],
    )
        def withDefault(default: CliOption.Default[T, CliElement.HasDefault, Md], update: (Ctx, CliOption.Result[T, CliElement.HasDefault, Md]) => Ctx): CliContextOption[Ctx, T, FullLabel, ShortLabel, CliElement.HasDefault, Md, Ord] =
            option.copy(default = default, update = update)
        

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

    transparent inline def withFullLabel[L <: Label](label : L): CliContextFlag[Ctx, L, ShortLabel, Ord] =
        copy(fullLabel = label)

    transparent inline def withoutFullLabel: CliContextFlag[Ctx, Unit, ShortLabel, Ord] = copy(fullLabel = ())

    transparent inline def withShortLabel[L <: CharLabel](label : L): CliContextFlag[Ctx, FullLabel, L, Ord] =
        copy(shortLabel = label)

    transparent inline def withoutShortLabel: CliContextFlag[Ctx, FullLabel, Unit, Ord] = copy(shortLabel = ())

    def withUpdateOnTrue(fn: Ctx => Ctx): CliContextFlag[Ctx, FullLabel, ShortLabel, Ord] =
        copy(update = (ctx, bool) => if (bool) fn(ctx) else ctx)
    
    def withUpdateOnFalse(fn: Ctx => Ctx): CliContextFlag[Ctx, FullLabel, ShortLabel, Ord] =
        copy(update = (ctx, bool) => if (bool) ctx else fn(ctx))

    def withUpdate(fn: (context: Ctx, isPresent: Boolean) => Ctx): CliContextFlag[Ctx, FullLabel, ShortLabel, Ord] =
        copy(update = fn)


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

    def add[Ele <: CliElement.ContextCliElement[Ctx]](element: Ele)(
        using
        CliContextOneOf.ValidEles[Ctx, Ele *: Eles],
    ): CliContextOneOf[Ctx, Ele *: Eles] =
        copy(elements = element *: elements)

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
