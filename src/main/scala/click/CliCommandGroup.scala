package click

import scala.reflect.ManifestFactory.NothingManifest
import click.util.Util
import click.util.Types.Maybe

import click.util.Types
import click.util.Types.Label
import click.exception.ClickException
import click.runtime.CommandGroup
import click.runtime.ArgumentInterpreter
import scala.util.NotGiven
final case class CliCommandGroup[
    T,
    Ctx,
    Cmds <: Tuple,
    LocEles <: Tuple,
    GlbEles <: Tuple,
    DefCtx <: CliCommand.DefaultEvalOrder,
](
    labeledCommands: Cmds,
    localContextElements: LocEles,
    globalContextElements: GlbEles,
    handler: CliCommandGroup.Params[Cmds] => T,
    override val description: Option[String],
)(
    using
    CliCommandGroup.CmdsCtxType[Cmds] <:< Maybe[Ctx],
    CliCommand.CtxTypes[LocEles] <:< Maybe[Ctx],
    CliCommand.CtxTypes[GlbEles] <:< Maybe[Ctx],
) extends CliElement.CommandCliElementWithCtx[T, Ctx]:

    def withDescription(description: String): CliCommandGroup[T, Ctx, Cmds, LocEles, GlbEles, DefCtx] =
        copy(description = Some(description))

    def withoutDescription: CliCommandGroup[T, Ctx, Cmds, LocEles, GlbEles, DefCtx] =
        copy(description = None)

    protected transparent inline def addCommand[L <: Label, CmdT, CmdCX, DirEles <: Tuple, CtxEles <: Tuple, DefOrd <: CliCommand.DefaultEvalOrder](
        commandName: L,
        command: CliCommand[CmdT, CmdCX, DirEles, CtxEles, DefOrd],
    ): CliCommandGroup[?, ?, ?, LocEles, GlbEles, DefCtx] =
        import scala.compiletime.*
        type Cmd = CliCommand[CmdT, CmdCX, DirEles, CtxEles, DefOrd]
        copy[CliCommandGroup.Params[(L, Cmd) *: Cmds], CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds], (L, Cmd) *: Cmds, LocEles, GlbEles, DefCtx](
                labeledCommands = (commandName, command) *: labeledCommands, handler = (x) => x,
            )(
                using
                summonInline[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds] <:< Maybe[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds]]],
                summonInline[CliCommand.CtxTypes[LocEles] <:< Maybe[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds]]],
                summonInline[CliCommand.CtxTypes[GlbEles] <:< Maybe[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds]]],
            )    

    protected transparent inline def addGroup[L <: Label, CmdT, CmdCX, CmdCmds <: Tuple, DirEles <: Tuple, CtxEles <: Tuple, DefOrd <: CliCommand.DefaultEvalOrder](
        inline commandName: L,
        command: CliCommandGroup[CmdT, CmdCX, CmdCmds, DirEles, CtxEles, DefOrd],
    ): CliCommandGroup[?, ?, ?, LocEles, GlbEles, DefCtx] =
        import scala.compiletime.*
        type Cmd = CliCommandGroup[CmdT, CmdCX, CmdCmds, DirEles, CtxEles, DefOrd]
        copy[CliCommandGroup.Params[(L, Cmd) *: Cmds], CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds], (L, Cmd) *: Cmds, LocEles, GlbEles, DefCtx](
                labeledCommands = (commandName, command) *: labeledCommands, handler = (x) => x,
            )(
                using
                summonInline[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds] <:< Maybe[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds]]],
                summonInline[CliCommand.CtxTypes[LocEles] <:< Maybe[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds]]],
                summonInline[CliCommand.CtxTypes[GlbEles] <:< Maybe[CliCommandGroup.CmdsCtxType[(L, Cmd) *: Cmds]]],
            )

    protected transparent inline def addContextElement[EleCX, Ele <: CliElement.ContextCliElement[EleCX]](ele: Ele, inline global: Boolean = false): CliCommandGroup[T, ?, Cmds, ?, ?, DefCtx] =
        import scala.compiletime.*
        inline global match
            case true => 
                type NewCtx = CliCommandGroup.TotalCtxType[Cmds, Util.Append[Ele, GlbEles], LocEles]
                copy[T, NewCtx, Cmds, LocEles, Util.Append[Ele, GlbEles], DefCtx](
                    globalContextElements = Util.Append(ele, globalContextElements),
                )(
                    using
                    summonInline[CliCommandGroup.CmdsCtxType[Cmds] <:< Maybe[NewCtx]],
                    summonInline[CliCommand.CtxTypes[LocEles] <:< Maybe[NewCtx]],
                    summonInline[CliCommand.CtxTypes[Util.Append[Ele, GlbEles]] <:< Maybe[NewCtx]],
                )
            case false =>
                type NewCtx = CliCommandGroup.TotalCtxType[Cmds, GlbEles, Util.Append[Ele, LocEles]]
                copy[T, NewCtx, Cmds, Util.Append[Ele, LocEles], GlbEles, DefCtx](
                    localContextElements = Util.Append(ele, localContextElements),
                )(
                    using
                    summonInline[CliCommandGroup.CmdsCtxType[Cmds] <:< Maybe[NewCtx]],
                    summonInline[CliCommand.CtxTypes[Util.Append[Ele, LocEles]] <:< Maybe[NewCtx]],
                    summonInline[CliCommand.CtxTypes[GlbEles] <:< Maybe[NewCtx]],
                )

    transparent inline def add[L <: Label, Cmd](inline label: L, element: Cmd): CliCommandGroup[?, ?, ?, ?, ?, DefCtx] =
        import scala.compiletime.*
        inline element match
            case cmd: CliCommand[a, b, c, d, e] => addCommand(label, cmd)
            case grp: CliCommandGroup[a, b, c, d, e, f] => addGroup(label, grp)
            case ctxOpt: CliContextOption[a, b, c, d, e, f, g] => addContextElement(ctxOpt, false)

    transparent inline def add[Cmd](element: Cmd): CliCommandGroup[?, ?, ?, ?, ?, DefCtx] =
        import scala.compiletime.*
        inline element match
            case ctxOpt: CliContextOption[a, b, c, d, e, f, g] => addContextElement(ctxOpt, false)
            case ctxFlg: CliContextFlag[a, b, c, d] => addContextElement(ctxFlg, false)
            case ctxOnf: CliContextOneOf[a, b] => addContextElement(ctxOnf, false)
            case _ => error("Unable to add element: you must provide a command or a command group")

    transparent inline def addGlobal[L <: Label, EleT, Ele <: CliElement.ContextCliElement[EleT]](inline label: L, element: Ele): CliCommandGroup[T, ?, Cmds, ?, ?, DefCtx] =
        import scala.compiletime.*
        addContextElement(element)

    def map[U](fn: T => U): CliCommandGroup[U, Ctx, Cmds, LocEles, GlbEles, DefCtx] =
        copy(handler = (params) => fn(handler(params)))


object CliCommandGroup:
    type CompareCtx[Ctx1, Ctx2] = Ctx1 match
        case Ctx2 => Ctx1
        case Unit => Ctx2
        case _ => Ctx2 match
            case Unit => Ctx1

    type CmdsCtxType[Cmds <: Tuple] = Cmds match
        case EmptyTuple => Unit
        case (Util.IsLabel[_], CliCommand[_, ctx, _, _, _]) *: EmptyTuple => ctx
        case (Util.IsLabel[_], CliCommandGroup[_, ctx, _, _, _, _]) *: EmptyTuple => ctx
        case (Util.IsLabel[_], CliCommand[_, ctx, _, _, _]) *: tail => CompareCtx[ctx, CmdsCtxType[tail]]
        case (Util.IsLabel[_], CliCommandGroup[_, ctx, _, _, _, _]) *: tail => CompareCtx[ctx, CmdsCtxType[tail]]

    type TotalCtxType[Cmds <: Tuple, Eles1 <: Tuple, Eles2 <: Tuple] =
        CompareCtx[CliCommandGroup.CmdsCtxType[Cmds], CompareCtx[CliCommand.CtxTypes[Eles1], CliCommand.CtxTypes[Eles2]]]

    type GetTypeFrom[X] = X match
        case CliCommand[t, _, _, _, _] => t
        case CliCommandGroup[t, _, _, _, _, _] => t

    type Params[Cmds <: Tuple] = Cmds match
        case EmptyTuple => Unit
        case (Util.IsLabel[_], cmd) *: EmptyTuple => GetTypeFrom[cmd]
        case (Util.IsLabel[_], cmd) *: tail => GetTypeFrom[cmd] | Params[tail]

    extension [T, Cmds <: Tuple, DirEles <: Tuple, CtxEles <: Tuple, DefCtx <:  CliCommand.DefaultEvalOrder](cmd: CliCommandGroup[T, Unit, Cmds, DirEles, CtxEles, DefCtx])
        inline def evaluate: Array[String] => ClickException.OrErr[T] =
            val command: CommandGroup = CommandGroupTranslation.translate(cmd)

            (args) => 
                val parsedArgs = ParseArgs.parse(args.toList)
                ArgumentInterpreter.interpret(command)(parsedArgs)(()).asInstanceOf[ClickException.OrErr[T]]

    extension [T, Ctx, Cmds <: Tuple, DirEles <: Tuple, CtxEles <: Tuple, DefCtx <:  CliCommand.DefaultEvalOrder](cmd: CliCommandGroup[T, Ctx, Cmds, DirEles, CtxEles, DefCtx])(
        using
        NotGiven[Ctx =:= Unit],
    )
        inline def evaluate(initialContext: Ctx): Array[String] => ClickException.OrErr[T] =
            val command: CommandGroup = CommandGroupTranslation.translate(cmd)

            (args) => 
                val parsedArgs = ParseArgs.parse(args.toList)
                ArgumentInterpreter.interpret(command)(parsedArgs)(initialContext).asInstanceOf[ClickException.OrErr[T]]