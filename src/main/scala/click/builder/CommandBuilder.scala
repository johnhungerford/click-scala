package click.builder

import click.{CliCommand, CliOption, CliElement}
import click.util.Types.*
import click.util.Util

final case class CommandBuilder[
    T,
    CX,
    DE <: Tuple,
    CE <: Tuple,
    HD,
    OR <: CliCommand.DefaultEvalOrder,
](
    private val de: DE,
    private val ce: CE,
    private val hd: HD,
    private val ds: Option[String],
):
    def handleResult[NewT](handler: CliCommand.Params[DE, CX] => NewT): CommandBuilder[NewT, CX, DE, CE, CliCommand.Params[DE, CX] => NewT, OR] =
        copy(hd = handler)

    def description(description: String): CommandBuilder[T, CX, DE, CE, HD, OR] = copy(ds = Some(description))

object CommandBuilder:
    def empty: CommandBuilder[Unit, Unit, EmptyTuple, EmptyTuple, Unit, CliCommand.ByDefOrder] =
        CommandBuilder(EmptyTuple, EmptyTuple, (), None)

    extension [T, CX, DE <: Tuple, CE <: Tuple, HD, OR <: CliCommand.DefaultEvalOrder](builder: CommandBuilder[T, CX, DE, CE, HD, OR])
        inline def add[EleT, Ele <: CliElement.DirectCliElement[EleT]](element: Ele):
            CommandBuilder[T, CX, Util.Append[Ele, DE], CE, Unit, OR] =
                builder.copy(de = Util.Append(element, builder.de), hd = ())

    extension [T, CX, DE <: Tuple, CE <: NonEmptyTuple, HD, OR <: CliCommand.DefaultEvalOrder](builder: CommandBuilder[T, CX, DE, CE, HD, OR])(
        using
        CE <:< NonEmptyTuple
    )
        inline def addContext[Ele <: CliElement.ContextCliElement[CX]](element: Ele):
            CommandBuilder[T, CX, DE, Util.Append[Ele, CE], HD, OR] =
                builder.copy(ce = Util.Append(element, builder.ce))

    extension [T, DE <: Tuple, HD, OR <: CliCommand.DefaultEvalOrder](builder: CommandBuilder[T, Unit, DE, EmptyTuple, HD, OR])
        def addContext[NewCX, Ele <: CliElement.ContextCliElement[NewCX]](element: Ele):
            CommandBuilder[T, NewCX, DE, Util.Append[Ele, EmptyTuple], HD, OR] =
                builder.copy(ce = Util.Append(element, EmptyTuple))


    extension [T, CX, DE <: Tuple, CE <: Tuple, OR <: CliCommand.DefaultEvalOrder](
        builder: CommandBuilder[T, CX, DE, CE, CliCommand.Params[DE, CX] => T, OR]
    )(
        using
        CliCommand.CtxTypes[CE] =:= CX,
    )
        inline def build: CliCommand[T, CX, DE, CE, OR] =
            CliCommand(builder.de, builder.ce, builder.hd, builder.ds)

    extension [T, CX, DE <: Tuple, CE <: Tuple, HD, OR <: CliCommand.DefaultEvalOrder](
        builder: CommandBuilder[T, CX, DE, CE, HD, OR]
    )(
        using
        CliCommand.CtxTypes[CE] =:= CX,
    )
        inline def build: CliCommand[CliCommand.Params[DE, CX], CX, DE, CE, OR] =
            CliCommand(builder.de, builder.ce, identity, builder.ds)
