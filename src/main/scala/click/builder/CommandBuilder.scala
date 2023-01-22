package click.builder

import click.{CliCommand, CliOption, CliElement}
import click.util.Types.*

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
    def add[EleT, Ele <: CliElement.DirectCliElement[EleT]](element: Ele):
        CommandBuilder[T, CliCommand.CtxTypes[Ele *: DE], Ele *: DE, CE, Unit, OR] =
            copy(de = element *: de, hd = ())

    def handleResult(handler: CliCommand.Params[DE, CX] => T): CommandBuilder[T, CX, DE, CE, CliCommand.Params[DE, CX] => T, OR] =
        copy(hd = handler)

object CommandBuilder:

    extension [T, CX, DE <: Tuple, CE <: NonEmptyTuple, HD, OR <: CliCommand.DefaultEvalOrder](builder: CommandBuilder[T, CX, DE, CE, HD, OR])(
        using
        CE <:< NonEmptyTuple
    )
        def addContext[Ele <: CliElement.ContextCliElement[CX]](element: Ele):
            CommandBuilder[T, CX, DE, Ele *: CE, HD, OR] =
                builder.copy(ce = element *: builder.ce)

    extension [T, CX, DE <: Tuple, HD, OR <: CliCommand.DefaultEvalOrder](builder: CommandBuilder[T, CX, DE, EmptyTuple, HD, OR])
        def addContext[Ele <: CliElement.ContextCliElement[CX]](element: Ele):
            CommandBuilder[T, CX, DE, Ele *: EmptyTuple, HD, OR] =
                builder.copy(ce = element *: EmptyTuple)


    extension [T, CX, DE <: Tuple, CE <: Tuple, OR <: CliCommand.DefaultEvalOrder](
        builder: CommandBuilder[T, CX, DE, CE, CliCommand.Params[DE, CX] => T, OR]
    )(
        using
        CliCommand.CtxTypes[CE] =:= CX,
    )
        inline def build(): CliCommand[T, CX, DE, CE, OR] =
            CliCommand(builder.de, builder.ce, builder.hd, builder.ds)
