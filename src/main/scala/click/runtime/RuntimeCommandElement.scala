package click.runtime

import click.InputElement
import click.exception.ClickException
import scala.compiletime.ops.int
import click.exception.ClickException.OrErr
import click.exception.ClickException
import click.util.Util
import scala.collection.immutable.ListMap

sealed trait RuntimeCommandElement:
    def applyShortOptionArgument(label: Char, argument: String): Option[Either[ClickException, RuntimeCommandElement]]

    def applyLongOptionArgument(label: String, argument: String): Option[Either[ClickException, RuntimeCommandElement]]

    def applyShortFlag(label: Char): Option[Either[ClickException, RuntimeCommandElement]]

    def applyLongFlag(label: String): Option[Either[ClickException, RuntimeCommandElement]]

    def applyArgument(label: String): Either[ClickException, RuntimeCommandElement]

object RuntimeCommandElement:
    case class Appl[T](value: T, index: Int)

    enum DoubleLabel:
        final lazy val shortLabelOpt: Option[Char] = this match
            case Short(label) => Some(label)
            case Long(label) => None
            case Both(shortLabel, longLabel) => Some(shortLabel)

        final lazy val longLabelOpt: Option[String] = this match
            case Short(label) => None
            case Long(label) => Some(label)
            case Both(shortLabel, longLabel) => Some(longLabel)

        case Short(label: Char)
        case Long(label: String)
        case Both(shortLabel: Char, longLabel: String)

    enum LabelSelector:
        def matches(label: DoubleLabel): Boolean = this match
            case Short(shortLabel) if label.shortLabelOpt.contains(shortLabel) => true
            case Long(longLabel) if label.longLabelOpt.contains(longLabel) => true
            case _ => false
        
        case Short(label: Char)
        case Long(label: String)

    case class OneOfApplication(selector: LabelSelector, applications: List[Appl[String]] | Appl[Unit])

    // Update elements matching the partial function yielding the updated list as well as a
    // boolean value indicating whether any had been updated
    def updateList[A](list: List[A], fn: PartialFunction[A, OrErr[A]]): (Boolean, OrErr[List[A]]) = list match
        case Nil => false -> Right(Nil)
        case head :: tail => if (fn.isDefinedAt(head)) {
            true -> fn.applyOrElse(head, Right.apply).flatMap { appliedHead => updateList[A](tail, fn) match
                case (_, updatedTailOrErr) => updatedTailOrErr.map(appliedHead :: _)
             }
        } else {
            updateList[A](tail, fn) match
                case (updated, updatedTailOrError) => updated -> updatedTailOrError.map(head :: _)
        }


final case class Command(
    commandPath: List[String],
    eagerPriorContextElements: List[RuntimeCtxElement],
    priorContextElements: List[RuntimeCtxElement],
    lazyPriorContextElements: List[RuntimeCtxElement],
    eagerLocalContextElements: List[RuntimeCtxElement],
    localContextElements: List[RuntimeCtxElement],
    lazyLocalContextElements: List[RuntimeCtxElement],
    eagerGlobalContextElements: List[RuntimeCtxElement],
    globalContextElements: List[RuntimeCtxElement],
    lazyGlobalContextOptions: List[RuntimeCtxElement],
    options: List[RuntimeDirElement & RuntimeOptLike],
    arguments: List[Argument],
    handler: List[Any] => Any,
    nextApplicationIndex: Int,
) extends RuntimeCommandElement:
    import RuntimeCommandElement.{LabelSelector, updateList}

    def updateCtxOpts(fn: PartialFunction[RuntimeCtxElement, OrErr[RuntimeCtxElement]]): Option[OrErr[Command]] =
        val tup1 = updateList(eagerLocalContextElements, fn)
        (for {
            newEagerLocalContextElements <- tup1._2
            tup2 = updateList(localContextElements, fn)
            newLocalContextElements <- tup2._2
            tup3 = updateList(lazyLocalContextElements, fn)
            newLazyLocalContextElements <- tup3._2
            tup4 = updateList(eagerGlobalContextElements, fn)
            newEagerGlobalContextElements <- tup4._2
            tup5 = updateList(globalContextElements, fn)
            newGlobalContextElements <- tup5._2
            tup6 = updateList(lazyGlobalContextOptions, fn)
            newLazyGlobalContextElements <- tup6._2
        } yield {
            (tup1._1 | tup2._1 | tup3._1 | tup4._1 | tup5._1 | tup6._1) ->
                copy(
                    eagerLocalContextElements = newEagerLocalContextElements,
                    localContextElements = newLocalContextElements,
                    lazyLocalContextElements = newLazyLocalContextElements,
                    eagerGlobalContextElements = newEagerGlobalContextElements,
                    globalContextElements = newGlobalContextElements,
                    lazyGlobalContextOptions = newLazyGlobalContextElements,
                )
        }) match
            case Left(res) => Some(Left(res))
            case Right((true, res)) => Some(Right(res))
            case _ => None
    
    def updateDirOpts(fn: PartialFunction[RuntimeDirOptLike, OrErr[RuntimeDirOptLike]]): Option[OrErr[Command]] =
        val (updated, newContextOrErr) = updateList(options, fn)
        if (updated) Some(newContextOrErr.map(v => copy(options = v)))
        else None

    def applyShortOptionArgument(label: Char, argument: String): Option[OrErr[Command]] =
        updateCtxOpts {
            case state@CtxOption(_, _, _, _) if state.matches(LabelSelector.Short(label)) =>
                state.invoke(argument, nextApplicationIndex)
        } orElse updateDirOpts {
            case state@DirOption(_, _, _, _, _) if state.matches(LabelSelector.Short(label)) =>
                state.invoke(argument, nextApplicationIndex)
        }
        
    def applyLongOptionArgument(label: String, argument: String): Option[OrErr[Command]] =
        updateCtxOpts {
            case state@CtxOption(_, _, _, _) if state.matches(LabelSelector.Long(label)) =>
                state.invoke(argument, nextApplicationIndex)
        } orElse updateDirOpts {
            case state@DirOption(_, _, _, _, _) if state.matches(LabelSelector.Long(label)) =>
                state.invoke(argument, nextApplicationIndex)
        }

    def applyShortFlag(label: Char): Option[OrErr[Command]] =
        updateCtxOpts {
            case state@CtxFlag(_, _, _, _) if state.matches(LabelSelector.Short(label)) =>
                state.invoke(nextApplicationIndex)
        } orElse updateDirOpts {
            case state@DirFlag(_, _, _, _, _) if state.matches(LabelSelector.Short(label)) =>
                state.invoke(nextApplicationIndex)
        }

    def applyLongFlag(label: String): Option[OrErr[Command]] =
        updateCtxOpts {
            case state@CtxFlag(_, _, _, _) if state.matches(LabelSelector.Long(label)) =>
                state.invoke(nextApplicationIndex)
        } orElse updateDirOpts {
            case state@DirFlag(_, _, _, _, _) if state.matches(LabelSelector.Long(label)) =>
                state.invoke(nextApplicationIndex)
        }

    def applyArgument(argument: String): OrErr[Command] =
        arguments.foldLeft(Right((false, List.empty[Argument])): OrErr[(Boolean, List[Argument])]) {
            case (err@Left(_), _) => err
            case (Right((true, updatedList)), nextArg) => Right(true, nextArg :: updatedList)
            case (Right((false, currentList)), argEle @ Argument(_, _, _, _, _)) if argEle.canInvoke  =>
                argEle.invoke(argument, nextApplicationIndex).map { newArgState =>
                    (true, newArgState :: currentList)
                }
            case (Right((false, currentList)), arg) => Right((false, arg :: currentList))
        } match
            case Left(err) => Left(err)
            // Need to reverse!!
            case Right((true, res)) => Right(copy(arguments = res.reverse))
            case Right((false, _)) => Left(ClickException.InvalidArgument(s"Unexpected argument ${argument}"))


    def evalCtxOpts(opts: List[RuntimeCtxElement])(ctx: OrErr[Any]): OrErr[Any] =
        opts.foldLeft(ctx)((currentCtx, nextEle) => {
            currentCtx.flatMap(c => nextEle.update(c))
        })

    def buildParams: OrErr[List[Any]] =
        Util.orErrSequence(
            (options ++ arguments)
                .sortBy(_.paramIndex)
                .map(_.param)
        ).map(_.toList)

    def evaluate(ctx: Any): Either[ClickException, Any] =
        val updater =
            evalCtxOpts(eagerGlobalContextElements) andThen
            evalCtxOpts(eagerPriorContextElements) andThen
            evalCtxOpts(eagerLocalContextElements) andThen
            evalCtxOpts(globalContextElements) andThen
            evalCtxOpts(priorContextElements) andThen
            evalCtxOpts(localContextElements) andThen
            evalCtxOpts(lazyLocalContextElements) andThen
            evalCtxOpts(lazyPriorContextElements) andThen
            evalCtxOpts(lazyGlobalContextOptions)
        val finalCtx = updater(Right(ctx))
        val params = buildParams
        for {
            c <- finalCtx
            params <- buildParams
        } yield {
            handler(c :: params)
        }

final case class CommandGroup(
    commandPath: List[String],
    labeledCommands: ListMap[String, RuntimeCommandElement],
    eagerPriorContextElements: List[RuntimeCtxElement],
    priorContextElements: List[RuntimeCtxElement],
    lazyPriorContextElements: List[RuntimeCtxElement],
    eagerLocalContextElements: List[RuntimeCtxElement],
    localContextElements: List[RuntimeCtxElement],
    lazyLocalContextElements: List[RuntimeCtxElement],
    eagerGlobalContextElements: List[RuntimeCtxElement],
    globalContextElements: List[RuntimeCtxElement],
    lazyGlobalContextOptions: List[RuntimeCtxElement],
    handler: Any => Any,
    nextApplicationIndex: Int,
) extends RuntimeCommandElement:
    import RuntimeCommandElement.{LabelSelector, updateList}

    def updateCtxOpts(fn: PartialFunction[RuntimeCtxElement, OrErr[RuntimeCtxElement]]): Option[OrErr[CommandGroup]] =
        val tup1 = updateList(eagerLocalContextElements, fn)
        (for {
            newEagerLocalContextElements <- tup1._2
            tup2 = updateList(localContextElements, fn)
            newLocalContextElements <- tup2._2
            tup3 = updateList(lazyLocalContextElements, fn)
            newLazyLocalContextElements <- tup3._2
            tup4 = updateList(eagerGlobalContextElements, fn)
            newEagerGlobalContextElements <- tup4._2
            tup5 = updateList(globalContextElements, fn)
            newGlobalContextElements <- tup5._2
            tup6 = updateList(lazyGlobalContextOptions, fn)
            newLazyGlobalContextElements <- tup6._2
        } yield {
            (tup1._1 | tup2._1 | tup3._1 | tup4._1 | tup5._1 | tup6._1) ->
                copy(
                    eagerLocalContextElements = newEagerLocalContextElements,
                    localContextElements = newLocalContextElements,
                    lazyLocalContextElements = newLazyLocalContextElements,
                    eagerGlobalContextElements = newEagerGlobalContextElements,
                    globalContextElements = newGlobalContextElements,
                    lazyGlobalContextOptions = newLazyGlobalContextElements,
                )
        }) match
            case Left(res) => Some(Left(res))
            case Right((true, res)) => Some(Right(res))
            case _ => None

    def applyShortOptionArgument(label: Char, argument: String): Option[OrErr[CommandGroup]] =
        updateCtxOpts {
            case state@CtxOption(_, _, _, _) if state.matches(LabelSelector.Short(label)) =>
                state.invoke(argument, nextApplicationIndex)
        }
        
    def applyLongOptionArgument(label: String, argument: String): Option[OrErr[CommandGroup]] =
        updateCtxOpts {
            case state@CtxOption(_, _, _, _) if state.matches(LabelSelector.Long(label)) =>
                state.invoke(argument, nextApplicationIndex)
        }

    def applyShortFlag(label: Char): Option[OrErr[CommandGroup]] =
        updateCtxOpts {
            case state@CtxFlag(_, _, _, _) if state.matches(LabelSelector.Short(label)) =>
                state.invoke(nextApplicationIndex)
        }

    def applyLongFlag(label: String): Option[OrErr[CommandGroup]] =
        updateCtxOpts {
            case state@CtxFlag(_, _, _, _) if state.matches(LabelSelector.Long(label)) =>
                state.invoke(nextApplicationIndex)
        }

    def applyArgument(argument: String): OrErr[RuntimeCommandElement] =
        labeledCommands.get(argument) match
            case None =>
                Left(ClickException.InvalidArgument(s"Could not match argument $argument to a command"))
            case Some(nextGrp@CommandGroup(_, nextCommands, nextEagerPriorCtx, nextPriorCtx, nextLazyPriorCtx, nextEagerLocalCtx, nextLocalCtx, nextLazyLocalCtx, nextEagerGlobalCtx, nextGlobalCtx, nextLazyGlobalCtx, nextHandler, _)) =>
                Right(nextGrp.copy(
                    commandPath = commandPath :+ argument,
                    eagerPriorContextElements =  eagerPriorContextElements ++ eagerLocalContextElements,
                    priorContextElements = priorContextElements ++ localContextElements,
                    lazyPriorContextElements = lazyPriorContextElements ++ lazyLocalContextElements,
                    eagerGlobalContextElements = eagerGlobalContextElements ++ nextEagerGlobalCtx,
                    globalContextElements = globalContextElements ++ nextGlobalCtx,
                    lazyGlobalContextOptions = nextLazyGlobalCtx ++ lazyGlobalContextOptions,
                    nextApplicationIndex = nextApplicationIndex,
                    handler = params => handler(nextHandler(params))
                ))
            case Some(nextCmd@Command(_, nextEagerPriorCtx, nextPriorCtx, nextLazyPriorCtx, nextEagerLocalCtx, nextLocalCtx, nextLazyLocalCtx, nextEagerGlobalCtx, nextGlobalCtx, nextLazyGlobalCtx, _, _, nextHandler, _)) =>
                Right(nextCmd.copy(
                    commandPath = commandPath :+ argument,
                    eagerPriorContextElements =  eagerPriorContextElements ++ eagerLocalContextElements,
                    priorContextElements = priorContextElements ++ localContextElements,
                    lazyPriorContextElements = lazyPriorContextElements ++ lazyLocalContextElements,
                    eagerGlobalContextElements = eagerGlobalContextElements ++ nextEagerGlobalCtx,
                    globalContextElements = globalContextElements ++ nextGlobalCtx,
                    lazyGlobalContextOptions = nextLazyGlobalCtx ++ lazyGlobalContextOptions,
                    nextApplicationIndex = nextApplicationIndex,
                    handler = input => handler(nextHandler(input))
                ))
