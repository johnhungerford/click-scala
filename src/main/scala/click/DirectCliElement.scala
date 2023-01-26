package click

import click.util.Nat
import click.util.Types.{Maybe, Label, CharLabel}
import click.exception.ClickException.OrParseErr
import scala.NonEmptyTuple

import click.util.Types
import click.exception.ClickException.InternalStateError
import click.exception.ClickException.InvalidArgument
import click.exception.ClickException.InvalidOption
import click.exception.ClickException.ParseError
import click.exception.ClickException
import click.util.Util
import Util.extensions.*

case class CliOption[
    T,
    FullLabel <: Maybe[Label],
    ShortLabel <: Maybe[CharLabel],
    Req <: CliElement.IsRequired,
    Md <: CliOption.Mode,
](
    fullLabel: FullLabel,
    shortLabel: ShortLabel,
    default: CliOption.Default[T, Req, Md],
    parse: String => OrParseErr[T],
    override val description: Option[String],
) extends CliElement.DirectCliElement[CliOption.Result[T, Req, Md]]:
    def withDescription(description: String): CliOption[ T, FullLabel, ShortLabel, Req, Md] =
        copy(description = Some(description))

    def withoutDescription: CliOption[ T, FullLabel, ShortLabel, Req, Md] =
        copy(description = None)

    transparent inline def withFullLabel[L <: Label](inline label : L): CliOption[T, L, ShortLabel, Req, Md] =
        copy(fullLabel = label)

    transparent inline def withoutFullLabel: CliOption[T, Unit, ShortLabel, Req, Md] = copy(fullLabel = ())

    transparent inline def withShortLabel[L <: CharLabel](inline label : L): CliOption[T, FullLabel, L, Req, Md] =
        copy(shortLabel = label)

    transparent inline def withoutShortLabel: CliOption[T, FullLabel, Unit, Req, Md] = copy(shortLabel = ())

    transparent inline def required: CliOption[T, FullLabel, ShortLabel, CliElement.Required, Md] =
        copy(default = ())

    transparent inline def optional: CliOption[T, FullLabel, ShortLabel, CliElement.Optional, Md] =
        copy(default = ())
    
    transparent inline def withDefault(default: CliOption.Default[T, CliElement.HasDefault, Md]): CliOption[T, FullLabel, ShortLabel, CliElement.HasDefault, Md] =
        copy(default = default)

    transparent inline def map[U](fn: T => U):  CliOption[U, FullLabel, ShortLabel, Req, Md] =
        import scala.compiletime.*
        val newDefault: CliOption.Default[U, Req, Md] = inline erasedValue[CliOption.Default[T, Req, Md]] match
            case _: Unit => Util.convertType[Unit, CliOption.Default[U, Req, Md]](())
            case _: T =>
                Util.convertType[U, CliOption.Default[U, Req, Md]](fn(Util.convertType[CliOption.Default[T, Req, Md], T](default)))
            case _: List[T] =>
                Util.convertType[List[U], CliOption.Default[U, Req, Md]](Util.convertType[CliOption.Default[T, Req, Md], List[T]](default).map(fn))
        
        copy[U, FullLabel, ShortLabel, Req, Md](parse = (str) => parse(str).map(fn), default = newDefault)

    inline def mapParse[U](fn: T => OrParseErr[U]): CliOption[U, FullLabel, ShortLabel, Req, Md] =
        import scala.compiletime.*
        inline erasedValue[CliOption.Default[T, Req, Md]] match
            case _: Unit =>
                copy[U, FullLabel, ShortLabel, Req, Md](
                    parse = (str) => parse(str).flatMap(fn),
                    default = Util.convertType[CliOption.Default[T, Req, Md], CliOption.Default[U, Req, Md]](default))
            case _ => error("Unable to map parse function on an option with a default value")

    inline def withValidation(validate: T => Option[ParseError]): CliOption[T, FullLabel, ShortLabel, Req, Md] =
        mapParse[T](t => validate(t) match
            case None => Right(t)
            case Some(err) => Left(err)
        )

    inline def withPartialValidation(validatePartial: PartialFunction[T, ParseError]): CliOption[T, FullLabel, ShortLabel, Req, Md] =
        withValidation(t => if (validatePartial.isDefinedAt(t)) Some(validatePartial(t)) else None)


object CliOption:
    sealed trait Mode
    sealed trait Single extends Mode
    sealed trait Multi extends Mode

    type Result[T, Req <: CliElement.IsRequired, Md <: Mode] = Md match
        case Single => CliElement.IsOptional[T, Req]
        case _ => CliElement.IsOptional[List[T], Req]

    type Default[T, Req <: CliElement.IsRequired, Md <: Mode] =
        CliElement.Default[Result[T, Req, Md], Req]

    extension [T, FL <: Maybe[Label], SL <: Maybe[CharLabel], Req <: CliElement.IsRequired, Md <: Mode](
        option: CliOption[T, FL, SL, Req, Md]
    )(
        using
        Default[T, Req, Md] =:= Unit,
    )
        inline def single: CliOption[T, FL, SL, Req, Single] =
            option.copy(default = ().inlineAs[Default[T, Req, Single]])

        def multi: CliOption[T, FL, SL, CliElement.HasDefault, Multi] =
            option.copy(default = Nil)

    extension [T, FL <: Maybe[Label], SL <: Maybe[CharLabel], Md <: Mode](
        option: CliOption[T, FL, SL, CliElement.HasDefault, Md]
    )
        def single(newDefault: Default[T, CliElement.HasDefault, Single]): CliOption[T, FL, SL, CliElement.HasDefault, Single] =
            option.copy(default = newDefault)

        def multi(newDefault: Default[T, CliElement.HasDefault, Multi] = Nil): CliOption[T, FL, SL, CliElement.HasDefault, Multi] =
            option.copy(default = newDefault)

case class CliFlag[
    T,
    FullLabel <: Maybe[Label],
    ShortLabel <: Maybe[CharLabel],
](
    fullLabel: FullLabel,
    shortLabel: ShortLabel,
    value: Boolean => T,
    override val description: Option[String],
) extends CliElement.DirectCliElement[T]:
    def withDescription(description: String):  CliFlag[T, FullLabel, ShortLabel] =
        copy(description = Some(description))

    transparent inline def withoutDescription: CliFlag[T, FullLabel, ShortLabel] =
        copy(description = None)

    transparent inline def withFullLabel[L <: Label](label : L): CliFlag[T, L, ShortLabel] =
        copy(fullLabel = label)

    transparent inline def withoutFullLabel: CliFlag[T, Unit, ShortLabel] = copy(fullLabel = ())

    transparent inline def withShortLabel[L <: CharLabel](label : L): CliFlag[T, FullLabel, L] =
        copy(shortLabel = label)

    def withoutShortLabel: CliFlag[T, FullLabel, Unit] = copy(shortLabel = ())

    def map[U](fn: T => U): CliFlag[U, FullLabel, ShortLabel] =
        copy(value = (b) => fn(value(b)))

    transparent inline def asOption[T](value: T): CliFlag[Option[T], FullLabel, ShortLabel] =
        copy(value = bool => if (bool) Some(value) else None)


case class CliArgument[
    T,
    Lb <: Label,
    Req <: CliElement.IsRequired,
    Md <: CliArgument.Mode,
](
    label: Lb,
    default: CliArgument.Default[T, Req, Md],
    parse: String => OrParseErr[T],
    override val description: Option[String],
) extends CliElement.DirectCliElement[CliArgument.Result[T, Req, Md]]:
    def withDescription(description: String):  CliArgument[T, Lb, Req, Md] =
        copy(description = Some(description))

    def withoutDescription: CliArgument[T, Lb, Req, Md] =
        copy(description = None)

    transparent inline def withLabel[L <: Label](label: L): CliArgument[T, L, Req, Md] =
        copy(label = label)

    def required: CliArgument[T, Lb, CliElement.Required, Md] =
        copy(default = ())
    
    def optional: CliArgument[T, Lb, CliElement.Optional, Md] =
        copy(default = ())

    def withDefault(default: CliArgument.Default[T, CliElement.HasDefault, Md]): CliArgument[T, Lb, CliElement.HasDefault, Md] =
        copy(default = default)

    inline def map[U](fn: T => U): CliArgument[U, Lb, Req, Md] =
        import scala.compiletime.*
        val newDefault: CliArgument.Default[U, Req, Md] = inline erasedValue[CliArgument.Default[T, Req, Md]] match
            case _: Unit => ().inlineAs[CliArgument.Default[U, Req, Md]]
            case _: T =>
                fn(default.inlineAs[T]).inlineAs[CliArgument.Default[U, Req, Md]]
            case _: List[T] =>
                default.inlineAs[List[T]].map(fn).inlineAs[CliArgument.Default[U, Req, Md]]
        
        copy(parse = (str: String) => parse(str).map(fn), default = newDefault)

    inline def mapParse[U](fn: T => OrParseErr[U]): CliArgument[U, Lb, Req, Md] =
        import scala.compiletime.*
        inline erasedValue[CliArgument.Default[T, Req, Md]] match
            case _: Unit =>
                copy[U, Lb, Req, Md](parse = (str) => parse(str).flatMap(fn), default = default.inlineAs[CliArgument.Default[U, Req, Md]])
            case _ => error("Unable to map parse function on an option with a default value")

    inline def withValidation(validate: T => Option[ParseError]): CliArgument[T, Lb, Req, Md] =
        mapParse[T](t => validate(t) match
            case None => Right(t)
            case Some(err) => Left(err)
        )

    inline def withPartialValidation(validatePartial: PartialFunction[T, ParseError]): CliArgument[T, Lb, Req, Md] =
        withValidation(t => if (validatePartial.isDefinedAt(t)) Some(validatePartial(t)) else None)


object CliArgument:
    sealed trait Mode
    sealed trait Single extends Mode
    sealed trait AnyMulti extends Mode
    sealed trait AtLeast[N <: Nat] extends Mode
    sealed trait NoMoreThan[N <: Nat] extends Mode
    sealed trait Exactly[N <: Nat] extends Mode

    type Result[T, Req <: CliElement.IsRequired, Md <: Mode] = Md match
        case Single => CliElement.IsOptional[T, Req]
        case _ => CliElement.IsOptional[List[T], Req]

    type Default[T, Req <: CliElement.IsRequired, Md <: Mode] =
        CliElement.Default[Result[T, Req, Md], Req]

    extension [T, L <: Label, Req <: CliElement.IsRequired, Md <: Mode](
        cliArg: CliArgument[T, L, Req, Md]
    )(
        using
        Default[T, Req, Md] =:= Unit,
    )
        inline def single: CliArgument[T, L, Req, Single] =
            cliArg.copy(default = ().inlineAs[Default[T, Req, Single]])

        def multi: CliArgument[T, L, CliElement.HasDefault, AnyMulti] =
            cliArg.copy(default = Nil)

        inline def multiAtLeast[I <: Int & Singleton](minimum: I): CliArgument[T, L, Req, AtLeast[Nat.Of[I]]] =
            cliArg.copy(default = ().inlineAs[Default[T, Req, AtLeast[Nat.Of[I]]]])

        inline def multiAtMost[I <: Int & Singleton](maximum: I): CliArgument[T, L, CliElement.HasDefault, NoMoreThan[Nat.Of[I]]] =
            cliArg.copy(default = Nil)

        inline def multiExactly[I <: Int & Singleton](amount: I): CliArgument[T, L, Req, Exactly[Nat.Of[I]]] =
            cliArg.copy(default = ().inlineAs[Default[T, Req, Exactly[Nat.Of[I]]]])

    extension [T, L <: Label, Md <: Mode](
        cliArg: CliArgument[T, L, CliElement.HasDefault, Md]
    )
        def single(newDefault: Default[T, CliElement.HasDefault, Single]): CliArgument[T, L, CliElement.HasDefault, Single] =
            cliArg.copy(default = newDefault)

        transparent inline def multi(newDefault: Default[T, CliElement.HasDefault, AnyMulti] = Nil): CliArgument[T, L, CliElement.HasDefault, AnyMulti] =
            cliArg.copy(default = newDefault)

        inline def multiAtLeast[I <: Int & Singleton](minimum: I, newDefault: Default[T, CliElement.HasDefault, AtLeast[Nat.Of[I]]]): CliArgument[T, L, CliElement.HasDefault, AtLeast[Nat.Of[I]]] =
            cliArg.copy(default = newDefault)

        inline def multiAtMost[I <: Int & Singleton](maximum: I, newDefault: Default[T, CliElement.HasDefault, NoMoreThan[Nat.Of[I]]] = Nil): CliArgument[T, L, CliElement.HasDefault, NoMoreThan[Nat.Of[I]]] =
            cliArg.copy(default = newDefault)

        inline def multiExactly[I <: Int & Singleton](amount: I, newDefault: Default[T, CliElement.HasDefault, Exactly[Nat.Of[I]]]): CliArgument[T, L, CliElement.HasDefault, Exactly[Nat.Of[I]]] =
            cliArg.copy(default = newDefault)
    


case class CliOneOf[
    T,
    Eles <: NonEmptyTuple,
](
    elements: Eles,
    handle: CliOneOf.UnionType[Eles] => T,
    override val description: Option[String],
) extends CliElement.DirectCliElement[T]:

    transparent inline def add[EleT, Ele <: CliElement.DirectCliElement[EleT]](element: Ele): CliOneOf[?, Ele *: Eles] =
        import scala.compiletime.*
        inline erasedValue[T] match
            case _: EleT =>
                copy[EleT, Ele *: Eles](elements = element *: elements, handle = (x) => Util.convertType[CliOneOf.UnionType[Ele *: Eles], EleT](x))
            case _: (EleT | other) =>
                copy[EleT | other, Ele *: Eles](elements = element *: elements, handle = (x) => Util.convertType[CliOneOf.UnionType[Ele *: Eles], EleT | other](x))
            case _ =>
                copy[EleT | CliOneOf.UnionType[Eles], Ele *: Eles](elements = element *: elements, handle = (x) => Util.convertType[CliOneOf.UnionType[Ele *: Eles], EleT | CliOneOf.UnionType[Eles]](x))

    def map[U](fn: T => U): CliOneOf[U, Eles] =
        copy(handle = (params) => fn(handle(params)))

object CliOneOf:
    type OptType[Ele] = Ele match
        case CliOption[t, _, _, _, _] => t
        case CliFlag[t, _, _] => t

    type UnionType[Eles <: NonEmptyTuple] = Eles match
        case CliOption[t, _, _, _, _] *: EmptyTuple => t
        case CliFlag[t, _, _] *: EmptyTuple => t
        case CliOneOf[t, _] *: EmptyTuple => t
        case CliAllOf[t, _, _] *: EmptyTuple => t
        case CliOption[t, _, _, _, _] *: tail => t | UnionType[tail]
        case CliFlag[t, _, _] *: tail => t | UnionType[tail]
        case CliOneOf[t, _] *: tail => t | UnionType[tail]
        case CliAllOf[t, _,  _] *: tail => t | UnionType[tail]


case class CliAllOf[
    T,
    Ps <: NonEmptyTuple,
    Eles <: NonEmptyTuple,
](
    elements: Eles,
    handle: Ps => T,
    override val description: Option[String],
)(
    using
    CliAllOf.ValidParams[Ps, Eles], 
) extends CliElement.DirectCliElement[T]:
    transparent inline def add[EleT, Ele <: CliElement.DirectCliElement[EleT]](element: Ele): CliAllOf[Util.Append[EleT, Ps], Util.Append[EleT, Ps], Util.Append[Ele, Eles]] =
        import scala.compiletime.*
        copy[Util.Append[EleT, Ps], Util.Append[EleT, Ps], Util.Append[Ele, Eles]](
            elements = Util.Append(element, elements),
            handle = (x) => x
        )(using CliAllOf.ValidParams.validParams[Util.Append[EleT, Ps], Util.Append[Ele, Eles]])

    def map[U](fn: T => U): CliAllOf[U, Ps, Eles] =
        copy(handle = (params) => fn(handle(params)))

object CliAllOf:
    sealed trait ValidParams[Params <: Tuple, Eles <: Tuple]
    object ValidParams:
        import scala.compiletime.*
        inline given validParams[Ps <: Tuple, Eles <: Tuple]: ValidParams[Ps, Eles] = inline erasedValue[Eles] match
            case _: EmptyTuple => inline erasedValue[Ps] match
                case _: EmptyTuple => new ValidParams[Ps, Eles] {}
            case _: (CliOption[t, _, _, _, _] *: tail) => inline erasedValue[Ps] match
                case _: (t *: psTail) =>
                    validParams[psTail, tail].asInstanceOf[ValidParams[Ps, Eles]]
            case _: (CliFlag[t, _, _] *: tail) => inline erasedValue[Ps] match
                case _: (t *: psTail) =>
                    validParams[psTail, tail].asInstanceOf[ValidParams[Ps, Eles]]
            case _: (CliOneOf[t, _] *: tail) => inline erasedValue[Ps] match
                case _: (t *: psTail) =>
                    validParams[psTail, tail].asInstanceOf[ValidParams[Ps, Eles]]
            case _: (CliAllOf[t, _, _] *: tail) => inline erasedValue[Ps] match
                case _: (t *: psTail) =>
                    validParams[psTail, tail].asInstanceOf[ValidParams[Ps, Eles]]
