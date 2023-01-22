package click

import scala.reflect.ManifestFactory.NothingManifest
import click.util.Util
import click.util.Types.Maybe

import click.util.Types
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
) extends CliElement.CommandCliElement[T]

object CliCommandGroup:
    type CmdsCtxType[Cmds <: Tuple] = Cmds match
        case (Util.IsLabel[_], CliCommand[_, ctx, _, _, _]) *: EmptyTuple => ctx
        case (Util.IsLabel[_], CliCommandGroup[_, ctx, _, _, _, _]) *: EmptyTuple => ctx
        case (Util.IsLabel[_], CliCommand[_, ctx, _, _, _]) *: tail => (ctx, CmdsCtxType[tail]) match
            case Util.SameTwo[sameCtx] => sameCtx
        case (Util.IsLabel[_], CliCommandGroup[_, ctx, _, _, _, _]) *: tail => (ctx, CmdsCtxType[tail]) match
            case Util.SameTwo[sameCtx] => sameCtx

    type GetTypeFrom[X] = X match
        case CliCommand[t, _, _, _, _] => t
        case CliCommandGroup[t, _, _, _, _, _] => t

    type Params[Cmds <: Tuple] = Cmds match
        case (Util.IsLabel[_], cmd) *: EmptyTuple => GetTypeFrom[cmd]
        case (Util.IsLabel[_], cmd) *: tail => GetTypeFrom[cmd] | Params[tail]
