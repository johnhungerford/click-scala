package click

import click.CliElement
import click.util.Util
import click.exception.ClickException
import click.runtime.Command
import click.runtime.ArgumentInterpreter
import scala.util.NotGiven

final case class CliCommand[
    T,
    Ctx,
    DirEles <: Tuple,
    CtxEles <: Tuple,
    DefCtx <: CliCommand.DefaultEvalOrder,
](
    directElements: DirEles,
    contextElements: CtxEles,
    handler: CliCommand.Params[DirEles, Ctx] => T,
    override val description: Option[String],
)(
    using
    CliCommand.CtxTypes[CtxEles] =:= Ctx,
) extends CliElement.CommandCliElement[T]:
    def withDescription(description: String): CliCommand[T, Ctx, DirEles, CtxEles, DefCtx] =
        copy(description = Some(description))
    
    def withoutDescription: CliCommand[T, Ctx, DirEles, CtxEles, DefCtx] =
        copy(description = None)

    def map[U](fn: T => U): CliCommand[U, Ctx, DirEles, CtxEles, DefCtx] =
        copy(handler = (params) => fn(handler(params)))

object CliCommand:
    /**
      * Determine the order in which context options are evaluated 
      * when their own evaluation order is set to `Default`
      */
    sealed trait DefaultEvalOrder
    sealed trait ByDefOrder extends DefaultEvalOrder
    sealed trait ByUseOrder extends DefaultEvalOrder
    sealed trait InheritOr[Ordr <: DefaultEvalOrder] extends DefaultEvalOrder

    type IsCmdType[T, Cmd <: CliElement.CommandCliElement[T]] = Cmd
    type IsCtxType[Ctx, CtxEle <: CliElement.ContextCliElement[Ctx]] = CtxEle
    type IsDirType[T, DirEle <: CliElement.DirectCliElement[T]] = DirEle

    type CtxType[Ele] = Ele match
        case CliElement.ContextCliElement[t] => t

    type CtxTypes[Eles <: Tuple] = Eles match
        case EmptyTuple => Unit
        case head *: EmptyTuple => CtxType[head]
        case head *: tail => (CtxType[head], CtxTypes[tail]) match
            case Util.SameTwo[u] => u
            case (u, Unit) => u

    type DirType[Ele] = Ele match
        case CliElement.DirectCliElement[t] => t

    type DirsType[Eles <: Tuple] = Eles match
        case EmptyTuple => EmptyTuple
        case head *: tail => DirType[head] *: DirsType[tail]

    type Params[DirEles <: Tuple, Ctx] = (Ctx, DirsType[DirEles]) match
        case (Unit, EmptyTuple) => Unit
        case (Unit, t *: EmptyTuple) => t
        case (Unit, tTup) => tTup
        case (ctx, EmptyTuple) => ctx
        case (ctx, tTup) => ctx *: tTup

    extension [T, DirEles <: Tuple, CtxEles <: Tuple, DefCtx <:  CliCommand.DefaultEvalOrder](cmd: CliCommand[T, Unit, DirEles, CtxEles, DefCtx])
        inline def evaluate: Array[String] => ClickException.OrErr[T] =
            val command: Command = CommandTranslation.translate(cmd)

            (args) => 
                val parsedArgs = ParseArgs.parse(args.toList)
                ArgumentInterpreter.interpret(command)(parsedArgs)(()).asInstanceOf[ClickException.OrErr[T]]

    extension [T, Ctx, DirEles <: Tuple, CtxEles <: Tuple, DefCtx <:  CliCommand.DefaultEvalOrder](cmd: CliCommand[T, Ctx, DirEles, CtxEles, DefCtx])(
        using
        NotGiven[Ctx =:= Unit],
    )
        inline def evaluate(initialContext: Ctx): Array[String] => ClickException.OrErr[T] =
            val command: Command = CommandTranslation.translate(cmd)

            (args) => 
                val parsedArgs = ParseArgs.parse(args.toList)
                ArgumentInterpreter.interpret(command)(parsedArgs)(initialContext).asInstanceOf[ClickException.OrErr[T]]
