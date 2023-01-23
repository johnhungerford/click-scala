package click

import click.util.Nat
import click.util.Types.{Maybe, Label, CharLabel}
import click.exception.ClickException.OrParseErr
import scala.NonEmptyTuple

import click.util.Types
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
) extends CliElement.DirectCliElement[CliOption.Result[T, Md]]:
    def withDescription(description: String): CliOption[ T, FullLabel, ShortLabel, Req, Md] =
        copy(description = Some(description))

    def withoutDescription: CliOption[ T, FullLabel, ShortLabel, Req, Md] =
        copy(description = None)

    def withFullLabel[L <: Label](label : L): CliOption[T, L, ShortLabel, Req, Md] =
        copy(fullLabel = label)

    def withoutFullLabel: CliOption[T, Unit, ShortLabel, Req, Md] = copy(fullLabel = ())

    def withShortLabel[L <: CharLabel](label : L): CliOption[T, FullLabel, L, Req, Md] =
        copy(shortLabel = label)

    def withoutShortLabel: CliOption[T, FullLabel, Unit, Req, Md] = copy(shortLabel = ())

    transparent inline def map[U](fn: T => U):  CliOption[U, FullLabel, ShortLabel, Req, Md] =
        import scala.compiletime.*
        val newDefault: CliOption.Default[U, Req, Md] = inline erasedValue[CliOption.Default[T, Req, Md]] match
            case _: Unit => ().asInstanceOf[CliOption.Default[U, Req, Md]]
            case _: T =>
                fn(default.asInstanceOf[T]).asInstanceOf[CliOption.Default[U, Req, Md]]
            case _: List[T] =>
                default.asInstanceOf[List[T]].map(fn).asInstanceOf[CliOption.Default[U, Req, Md]]
        
        copy[U, FullLabel, ShortLabel, Req, Md](parse = (str) => parse(str).map(fn), default = newDefault)

    inline def mapParse[U](fn: T => OrParseErr[U]): CliOption[U, FullLabel, ShortLabel, Req, Md] =
        import scala.compiletime.*
        inline erasedValue[CliOption.Default[T, Req, Md]] match
            case _: Unit =>
                copy[U, FullLabel, ShortLabel, Req, Md](parse = (str) => parse(str).flatMap(fn), default = default.asInstanceOf[CliOption.Default[U, Req, Md]])
            case _ => error("Unable to map parse function on an option with a default value")
        

object CliOption:
    sealed trait Mode
    sealed trait Single extends Mode
    sealed trait Multi extends Mode

    type Result[T, Md <: Mode] = Md match
        case Single => T
        case Multi => List[T]

    type Default[T, Req <: CliElement.IsRequired, Md <: Mode] =
        CliElement.Default[Result[T, Md], Req]

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

    def withoutDescription: CliFlag[T, FullLabel, ShortLabel] =
        copy(description = None)

    def withFullLabel[L <: Label](label : L): CliFlag[T, L, ShortLabel] =
        copy(fullLabel = label)

    def withoutFullLabel: CliFlag[T, Unit, ShortLabel] = copy(fullLabel = ())

    def withShortLabel[L <: CharLabel](label : L): CliFlag[T, FullLabel, L] =
        copy(shortLabel = label)

    def withoutShortLabel: CliFlag[T, FullLabel, Unit] = copy(shortLabel = ())

    def map[U](fn: T => U): CliFlag[U, FullLabel, ShortLabel] =
        copy(value = (b) => fn(value(b)))

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
) extends CliElement.DirectCliElement[CliArgument.Result[T, Md]]:
    def withDescription(description: String):  CliArgument[T, Lb, Req, Md] =
        copy(description = Some(description))

    def withoutDescription: CliArgument[T, Lb, Req, Md] =
        copy(description = None)

    def withLabel[L <: Label](label: L): CliArgument[T, L, Req, Md] =
        copy(label = label)

    inline def map[U](fn: T => U): CliArgument[U, Lb, Req, Md] =
        import scala.compiletime.*
        val newDefault: CliArgument.Default[U, Req, Md] = inline erasedValue[CliArgument.Default[T, Req, Md]] match
            case _: Unit => ().asInstanceOf[CliArgument.Default[U, Req, Md]]
            case _: T =>
                fn(default.asInstanceOf[T]).asInstanceOf[CliArgument.Default[U, Req, Md]]
            case _: List[T] =>
                default.asInstanceOf[List[T]].map(fn).asInstanceOf[CliArgument.Default[U, Req, Md]]
        
        copy(parse = (str: String) => parse(str).map(fn), default = newDefault)

    inline def mapParse[U](fn: T => OrParseErr[U]): CliArgument[U, Lb, Req, Md] =
        import scala.compiletime.*
        inline erasedValue[CliArgument.Default[T, Req, Md]] match
            case _: Unit =>
                copy[U, Lb, Req, Md](parse = (str) => parse(str).flatMap(fn), default = default.asInstanceOf[CliArgument.Default[U, Req, Md]])
            case _ => error("Unable to map parse function on an option with a default value") 

case class CliOneOf[
    T,
    Eles <: NonEmptyTuple,
](
    elements: Eles,
    handle: CliOneOf.UnionType[Eles] => T,
) extends CliElement.DirectCliElement[T]:
    override def description: Option[String] = None

object CliOneOf:
    type OptType[Ele] = Ele match
        case CliOption[t, _, _, _, _] => t
        case CliFlag[t, _, _] => t

    type UnionType[Eles <: NonEmptyTuple] = Eles match
        case ele *: EmptyTuple => OptType[ele]
        case ele *: tail => OptType[ele] | UnionType[tail]

object CliArgument:
    sealed trait Mode
    sealed trait Single extends Mode
    sealed trait AnyMulti extends Mode
    sealed trait AtLeast[N <: Nat] extends Mode
    sealed trait NoMoreThan[N <: Nat] extends Mode
    sealed trait Exactly[N <: Nat] extends Mode

    type Result[T, Md <: Mode] = Md match
        case Single => T
        case _ => List[T]

    type Default[T, Req <: CliElement.IsRequired, Md <: Mode] =
        CliElement.Default[Result[T, Md], Req]



