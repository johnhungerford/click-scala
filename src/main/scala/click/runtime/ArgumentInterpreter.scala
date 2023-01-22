package click.runtime

import click.InputElement
import click.exception.ClickException
import scala.compiletime.ops.int
import click.exception.ClickException.OrErr
import click.util.Util
import scala.collection.mutable.ListMap


object ArgumentInterpreter:
    extension [A, B] (optEither: Option[Either[A, B]])
        def flatMapT[C](fn: B => Option[Either[A, C]]): Option[Either[A, C]] =
            optEither match
                case None => None
                case Some(eitherB) => eitherB match
                    case Left(value) => Some(Left(value))
                    case Right(value) => fn(value)
                

    def interpret(context: RuntimeCommandElement)(args: List[InputElement])(ctx: Any): ClickException.OrErr[Any] =
        // println(context)
        args match
            case Nil => context match
                case cmd @ Command(_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                    cmd.evaluate(ctx)
                case _ =>
                    Left(ClickException.ParseError("No command selected"))

            case InputElement.ShortOptionGroup(opts) :: tail =>
                opts.foldLeft(Right(context): Either[ClickException, RuntimeCommandElement]) {
                    case (currentContext, InputElement.ShortOption(optChar)) =>
                        Some(currentContext).flatMapT(_.applyShortFlag(optChar)) match
                            case None => Left(ClickException.InvalidOption(s"Invalid flag -$optChar"))
                            case Some(value) => value
                } flatMap(v => interpret(v)(tail)(ctx))

            case InputElement.ShortOption(optChar) :: (argEle @ InputElement.Argument(arg)) :: tail =>
                context.applyShortOptionArgument(optChar, arg).flatMapT { newContext =>
                    Some(interpret(newContext)(tail)(ctx))
                } orElse {
                    context.applyShortFlag(optChar).flatMapT { newContext =>
                        Some(interpret(newContext)(argEle :: tail)(ctx))
                    }
                } getOrElse Left(ClickException.InvalidOption(s"Invalid flag -$optChar"))

            case InputElement.ShortOption(optChar) :: tail =>
                context
                    .applyShortFlag(optChar)
                    .getOrElse(Left(ClickException.InvalidOption(s"Invalid flag $optChar")))
                    .flatMap(v => interpret(v)(tail)(ctx))

            case InputElement.LongOption(optStr) :: (argEle @ InputElement.Argument(arg)) :: tail =>
                context.applyLongOptionArgument(optStr, arg).flatMapT { newContext =>
                    Some( interpret(newContext)(tail)(ctx))
                } orElse {
                    context.applyLongFlag(optStr).flatMapT { newContext =>
                        Some(interpret(newContext)(argEle :: tail)(ctx))
                    }
                } getOrElse Left(ClickException.InvalidOption(s"Invalid option --$optStr"))
                    .flatMap(v => interpret(v)(tail)(ctx))

            case InputElement.LongOption(optStr) :: tail =>
                context
                    .applyLongFlag(optStr)
                    .getOrElse(Left(ClickException.InvalidOption(s"Invalid option --$optStr")))
                    .flatMap(v => interpret(v)(tail)(ctx))

            case InputElement.Argument(argStr) :: tail =>
                context.applyArgument(argStr)
                    .flatMap(v => interpret(v)(tail)(ctx))
                
