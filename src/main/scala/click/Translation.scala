package click

import click.util.Types.{Maybe, Label, CharLabel}
import click.exception.ClickException.*
import click.runtime.DirOption
import click.runtime.RuntimeCommandElement.{Appl, LabelSelector}


import scala.compiletime.*
import click.runtime.DirFlag
import click.runtime.DirOneOf
import click.runtime.Argument
import click.util.{Nat, Macros}
import click.runtime.CtxOption
import click.runtime.CtxFlag
import click.runtime.CtxOneOf
import click.runtime.Command
import click.runtime.RuntimeDirElement
import click.runtime.RuntimeCtxElement
import click.runtime.RuntimeOptLike
import click.runtime.CommandGroup
import scala.collection.immutable.ListMap
import click.runtime.RuntimeCommandElement
import click.runtime.ArgumentInterpreter
import click.ParseArgs
import click.runtime.RuntimeCommandElement.DoubleLabel
import click.runtime.DirAllOf

import click.util.Util.extensions.*

object SelectorTranslation:
    inline def doubleLabel[FL <: Maybe[Label], SL <: Maybe[CharLabel]](fl: FL, sl: SL): DoubleLabel =
        inline fl match
            case () => inline sl match
                case () => error("Long and short label is empty")
                case short: Char => DoubleLabel.Short(short)
            case long: String => inline sl match
                case () => DoubleLabel.Long(long)
                case short: Char => DoubleLabel.Both(short, long)

object DirOptionTranslation:
    inline def translate[
        T,
        FL <: Maybe[Label],
        SL <: Maybe[CharLabel],
        RQ <: CliElement.IsRequired,
        MD <: CliOption.Mode,
    ](opt: CliOption[T, FL, SL, RQ, MD], paramIndex: Int): DirOption =
        val labels = SelectorTranslation.doubleLabel[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: (DirOption, String, Int) => OrErr[DirOption] = (dirOpt, argument, applIndex) =>
            if (dirOpt.applications.isEmpty)
                opt.parse(argument).map(
                    parsedVal =>
                        dirOpt.copy(applications = List(Appl(parsedVal, applIndex)))
                )
            else inline erasedValue[MD] match
                case _: CliOption.Single =>
                    Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                case _: CliOption.Multi =>
                    opt.parse(argument).map(
                        parsedVal =>
                            dirOpt.copy(
                                applications = dirOpt.applications :+ Appl(parsedVal, applIndex),
                            )
                    )
        
        val genParam: DirOption => OrErr[Any] = inline erasedValue[CliOption.Result[T, RQ, MD]] match
            case _: List[T] =>
                inline opt.default match
                    case _: Unit =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Left(InvalidOption(s"Must invoke $labels at least once"))
                            else
                                Right(dirOpt.applications.sortBy(_.index).map(_.value))
                    case defaultValues: List[T] =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Right(defaultValues)
                            else
                                Right(dirOpt.applications.sortBy(_.index).map(_.value))
            case _: Option[List[T]] =>
                (dirOpt) =>
                    dirOpt.applications match
                        case Nil =>
                            Right(None)
                        case other =>
                            Right(Some(other.sortBy(_.index).map(_.value)))


            case _: Option[T] =>
                (dirOpt) =>
                    dirOpt.applications match
                        case head :: other :: _ =>
                            Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                        case head :: Nil =>
                            Right(Some(head.value))
                        case Nil =>
                            Right(None)
            case _: T =>
                inline opt.default match
                    case _: Unit => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Left(InvalidOption(s"Required option $labels was never invoked"))
                    case defaultValue: T => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Right(defaultValue)
                

        DirOption(labels, Nil, genInvoke, genParam, paramIndex)

object DirFlagTranslation:
    inline def translate[
        T,
        FL <: Maybe[Label],
        SL <: Maybe[CharLabel],
    ](opt: CliFlag[T, FL, SL], paramIndex: Int): DirFlag =
        val labels = SelectorTranslation.doubleLabel[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: (DirFlag, Int) => OrErr[DirFlag] = (dirFlag, applIndex) =>
            if (dirFlag.application.isEmpty)
                Right(dirFlag.copy(application = Some(applIndex)))
            else
                Left(InvalidOption(s"Can't invoke $labels multiple times"))
        
        val genParam: DirFlag => OrErr[Any] = (dirFlag) =>
            dirFlag.application match
                case None => Right(opt.value(false))
                case Some(_) => Right(opt.value(true))
            
        DirFlag(labels, None, genInvoke, genParam, paramIndex)


object DirMultiTranslation:
    inline def translateOptions[Opts <: Tuple](opts: Opts): List[DirOption | DirFlag | DirOneOf | DirAllOf] =
        inline erasedValue[Opts] match
            case EmptyTuple => Nil
            case _: (CliOption[a, b, c, d, e] *: tail) =>
                val typedOpts = opts.inlineAs[CliOption[a, b, c, d, e] *: tail]
                val opt *: tail = typedOpts
                DirOptionTranslation.translate(opt, -1) :: translateOptions(tail)
            case _: (CliFlag[a, b, c] *: tail) =>
                val typedOpts = opts.inlineAs[CliFlag[a, b, c] *: tail]
                val flg *: tail = typedOpts
                DirFlagTranslation.translate(flg, -1) :: translateOptions(tail)
            case _: (CliOneOf[a, b] *: tail) =>
                val typedOpts = opts.inlineAs[CliOneOf[a, b] *: tail]
                val oneOf *: tail = typedOpts
                DirOneOfTranslation.translate(oneOf, -1) :: translateOptions(tail)
            case _: (CliAllOf[a, b, c] *: tail) =>
                val typedOpts = opts.inlineAs[CliAllOf[a, b, c] *: tail]
                val allOf *: tail = typedOpts
                DirAllOfTranslation.translate(allOf, -1) :: translateOptions(tail)

    inline def optionsShow[Opts <: Tuple](opts: Opts): List[String] =
        inline erasedValue[Opts] match
            case _: EmptyTuple => Nil
            case _: (CliOption[a, b, c, d, e] *: tail) =>
                val typedOpts = opts.inlineAs[CliOption[a, b, c, d, e] *: tail]
                val opt *: tail = typedOpts
                SelectorTranslation.doubleLabel[b, c](opt.fullLabel, opt.shortLabel).toString() :: optionsShow(tail)
            case _: (CliFlag[a, b, c] *: tail) =>
                val typedOpts = opts.inlineAs[CliFlag[a, b, c] *: tail]
                val flg *: tail = typedOpts
                SelectorTranslation.doubleLabel[b, c](flg.fullLabel, flg.shortLabel).toString() :: optionsShow(tail)
            case _: (CliOneOf[a, b] *: tail) =>
                val typedOpts = opts.inlineAs[CliOneOf[a, b] *: tail]
                val oneOf *: tail = typedOpts
                optionsShow(oneOf.elements) ++ optionsShow(tail)
            case _: (CliAllOf[a, b, c] *: tail) =>
                val typedOpts = opts.inlineAs[CliAllOf[a, b, c] *: tail]
                val allOf *: tail = typedOpts
                optionsShow(allOf.elements) ++ optionsShow(tail)

object DirOneOfTranslation:    
    inline def translate[
        T,
        Eles <: NonEmptyTuple,
    ](opt: CliOneOf[T, Eles], paramIndex: Int): DirOneOf =
        val optionLabels: List[String] = DirMultiTranslation.optionsShow(opt.elements)
        val options = DirMultiTranslation.translateOptions(opt.elements)
        val matchesAndIsFlag: LabelSelector => Option[Boolean] =
            (sel) => options.find(_.matches(sel)).map {
                case DirFlag(_, _, _, _, _) => true
                case _ => false
            }

        val genInvoke: (DirOneOf, LabelSelector, Option[String], Int) => OrErr[DirOneOf] =
            (dirOneOf, sel, argOpt, applIndex) =>
                dirOneOf.selection match
                    case Some(opt) if opt.matches(sel) => opt match
                        case dirOpt @ DirOption(_, _, _, _, _) => argOpt match
                            case None => Left(InvalidOption(s"$sel requires an argument"))
                            case Some(value) =>
                                dirOpt.invoke(value, applIndex)
                                    .map(updated => dirOneOf.copy(selection = Some(updated)))
                                
                        case dirFlag @ DirFlag(_, _, _, _, _) =>
                            argOpt match
                                case Some(_) => Left(InvalidOption(s"$sel does not take an argument"))
                                case None =>
                                    dirFlag.invoke(applIndex)
                                        .map(updated => dirOneOf.copy(selection = Some(updated)))

                        case nestedDirOneOf @ DirOneOf(_, _, _, _, _) =>
                            argOpt match
                                case None => nestedDirOneOf.invokeFlag(sel, applIndex)
                                    .map(updated => dirOneOf.copy(selection = Some(updated)))
                                case Some(value) => nestedDirOneOf.invokeOpt(sel, value, applIndex)
                                    .map(updated => dirOneOf.copy(selection = Some(updated)))

                        case nestedDirAllOf @ DirAllOf(_, _, _, _, _) =>
                            argOpt match
                                case None => nestedDirAllOf.invokeFlag(sel, applIndex)
                                    .map(updated => dirOneOf.copy(selection = Some(updated)))
                                case Some(value) => nestedDirAllOf.invokeOpt(sel, value, applIndex)
                                    .map(updated => dirOneOf.copy(selection = Some(updated))) 

                    case Some(opt) =>
                        Left(InvalidOption(s"$sel is not a valid option"))
                        
                    case None => 
                        options.find {
                            _.matches(sel)
                        } match
                            case None => Left(InvalidOption(s"$sel does not match any option or flag"))
                            case Some(opt @ DirOption(_, _, _, _, _)) =>
                                argOpt match
                                    case None => Left(InvalidOption(s"$sel requires an argument"))
                                    case Some(value) =>
                                        opt.invoke(value, applIndex)
                                            .map(updatedDir => dirOneOf.copy(selection = Some(updatedDir)))

                            case Some(opt @ DirFlag(_, _, _, _, _)) =>
                                argOpt match
                                    case Some(_) => Left(InvalidOption(s"$sel is a flag and does not take an argument"))
                                    case None =>
                                        opt.invoke(applIndex)
                                            .map(updatedDir => dirOneOf.copy(selection = Some(updatedDir)))

                            case Some(opt @ DirOneOf(_, _, _, _, _)) =>
                                argOpt match
                                    case Some(arg) => opt.invokeOpt(sel, arg, applIndex)
                                        .map(updatedDir => dirOneOf.copy(selection = Some(updatedDir)))
                                    case None =>
                                        opt.invokeFlag(sel, applIndex)
                                            .map(updatedDir => dirOneOf.copy(selection = Some(updatedDir)))

                            case Some(opt @ DirAllOf(_, _, _, _, _)) =>
                                argOpt match
                                    case Some(arg) => opt.invokeOpt(sel, arg, applIndex)
                                        .map(updatedDir => dirOneOf.copy(selection = Some(updatedDir)))
                                    case None =>
                                        opt.invokeFlag(sel, applIndex)
                                            .map(updatedDir => dirOneOf.copy(selection = Some(updatedDir)))
        
        val genParam: DirOneOf => OrErr[Any] = (dirOneOf) =>
            dirOneOf.selection.map(_.param) match
                case None =>
                    Left(ParseError(s"At least one of the following options must be invoked: ${optionLabels.mkString(", ")}"))
                case Some(res) => res

        DirOneOf(matchesAndIsFlag, None, genInvoke, genParam, paramIndex)


object DirAllOfTranslation:
    inline def translate[
        T,
        Ps <: NonEmptyTuple,
        Eles <: NonEmptyTuple,
    ](opt: CliAllOf[T, Ps, Eles], paramIndex: Int): DirAllOf = 
        val optionLabels: List[String] = DirMultiTranslation.optionsShow(opt.elements)
        val options =
            DirMultiTranslation.translateOptions(opt.elements)
                .zipWithIndex
                .map(tup => tup._2 -> tup._1)
                .toVector
        val matchesAndIsFlag: LabelSelector => Option[Boolean] =
            (sel) => options.find(_._2.matches(sel)).map {
                case (_, DirFlag(_, _, _, _, _)) => true
                case _ => false
            }

        val genInvoke: (DirAllOf, LabelSelector, Option[String], Int) => OrErr[DirAllOf] =
            (dirAllOf, sel, argOpt, applIndex) =>
                dirAllOf.findElement(sel) match
                    case Some(opt) if opt.matches(sel) => opt match
                        case dirOpt @ DirOption(_, _, _, _, _) => argOpt match
                            case None => Left(InvalidOption(s"$sel requires an argument"))
                            case Some(value) =>
                                dirOpt.invoke(value, applIndex)
                                    .flatMap(updated => dirAllOf.replaceElement(sel, updated).get)
                                
                        case dirFlag @ DirFlag(_, _, _, _, _) =>
                            argOpt match
                                case Some(_) => Left(InvalidOption(s"$sel does not take an argument"))
                                case None =>
                                    dirFlag.invoke(applIndex)
                                        .flatMap(updated => dirAllOf.replaceElement(sel, updated).get)

                        case nestedDirOneOf @ DirOneOf(_, _, _, _, _) =>
                            argOpt match
                                case None => nestedDirOneOf.invokeFlag(sel, applIndex)
                                    .flatMap(updated => dirAllOf.replaceElement(sel, updated).get)
                                case Some(value) => nestedDirOneOf.invokeOpt(sel, value, applIndex)
                                    .flatMap(updated => dirAllOf.replaceElement(sel, updated).get)

                        case nestedDirAllOf @ DirAllOf(_, _, _, _, _) =>
                            argOpt match
                                case None => nestedDirAllOf.invokeFlag(sel, applIndex)
                                    .flatMap(updated => dirAllOf.replaceElement(sel, updated).get)
                                case Some(value) => nestedDirAllOf.invokeOpt(sel, value, applIndex)
                                    .flatMap(updated => dirAllOf.replaceElement(sel, updated).get)
                             

                    case Some(opt) =>
                        Left(InvalidOption(s"$sel is not a valid option"))

                    case None => Left(InvalidOption(s"$sel is not a valid option"))

        val tupler = CommandTranslation.listToTuple[Ps]

        val genParam: DirAllOf => OrErr[Any] = (dirAllOf) =>
            val paramsListOrErr = dirAllOf.elements.foldLeft(Right(Nil): OrErr[List[Any]])((currentParamsOrErr, nextState) => {
                currentParamsOrErr.flatMap { currentParams =>
                    nextState.param.map(_ :: currentParams)  
                }
            }).map(_.reverse)
            val tupledParams = paramsListOrErr.map(tupler)
            tupledParams

        DirAllOf(matchesAndIsFlag, options, genInvoke, genParam, paramIndex)

object ArgumentTranslation:
    inline def natToInt[N <: Nat]: Int = constValue[Nat.ToInt[N]]

    inline def translate[
        T,
        LB <: Label,
        RQ <: CliElement.IsRequired,
        MD <: CliArgument.Mode,
    ](
        argt: CliArgument[T, LB, RQ, MD],
        paramIndex: Int,
    ): Argument =
        val label: String = argt.label
        val genCanInvoke: (Argument) => Boolean =
            inline erasedValue[MD] match
                case _: CliArgument.Single => (arg) => arg.applications.isEmpty
                case _: CliArgument.AnyMulti => _ => true
                case _: CliArgument.AtLeast[n] => _ => true
                case _: CliArgument.NoMoreThan[n] =>
                    val upperLimit = natToInt[n]
                    (arg) => arg.applications.length < upperLimit

                case _: CliArgument.Exactly[n] =>
                    val upperLimit = natToInt[n]
                    (arg) => arg.applications.length < upperLimit

        val genInvoke: (Argument, String, Int) => OrErr[Argument] =
             inline erasedValue[MD] match
                case _: CliArgument.Single => { (ele, arg, index) =>
                    if (ele.applications.nonEmpty)
                        Left(InvalidOption(s"Cannot invoke argument \"$label\" more than once"))
                    else argt.parse(arg)
                        .map(argVal => ele.copy(applications = List(Appl(argVal, index))))
                }
                case _: CliArgument.AnyMulti => { (ele, arg, index) =>
                    argt.parse(arg)
                        .map(argVal => ele.copy(applications =  ele.applications :+ Appl(argVal, index)))
                }
                case _: CliArgument.AtLeast[n] => { (ele, arg, index) =>
                    argt.parse(arg)
                        .map(argVal => ele.copy(applications =  ele.applications :+ Appl(argVal, index)))
                }
                case _: CliArgument.NoMoreThan[n] =>
                    val upperLimit = natToInt[n]
                    { (ele, arg, index) =>
                        if (ele.applications.size >= upperLimit)
                            Left(InvalidOption(s"Cannot invoke argument \"$label\" more than $upperLimit times"))
                        else argt.parse(arg)
                            .map(argVal => ele.copy(applications =  ele.applications :+ Appl(argVal, index)))
                    }

                case _: CliArgument.Exactly[n] =>
                    val upperLimit = natToInt[n]
                    { (ele, arg, index) =>
                        if (ele.applications.size >= upperLimit)
                            Left(InvalidOption(s"Cannot invoke argument \"$label\" more than $upperLimit times"))
                        else argt.parse(arg)
                            .map(argVal => ele.copy(applications =  ele.applications :+ Appl(argVal, index)
                        ))
                    }
        val genParam: Argument => OrErr[Any] = inline erasedValue[CliArgument.Result[T, RQ, MD]] match
            case _: List[T] =>
                inline argt.default match
                    case _: Unit =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Left(InvalidOption(s"Must invoke argument \"$label\" at least once"))
                            else
                                Right(dirOpt.applications.sortBy(_.index).map(_.value))
                    case defaultValues: List[T] =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Right(defaultValues)
                            else
                                Right(dirOpt.applications.sortBy(_.index).map(_.value))

            case _: Option[List[T]] =>
                (dirOpt) =>
                    dirOpt.applications match
                        case Nil =>
                            Right(None)
                        case other =>
                            Right(Some(other.sortBy(_.index).map(_.value)))

            case _: Option[T] =>
                (dirOpt) =>
                    dirOpt.applications match
                        case Nil =>
                            Right(None)
                        case head :: other :: _ =>
                            Left(InvalidOption(s"Cannot invoke argument \"$label\" multiple times"))
                        case head :: tail =>
                            Right(Some(head.value))

            case _: T =>
                inline argt.default match
                    case _: Unit => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidArgument(s"Cannot invoke argument \"$label\" multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Left(InvalidArgument(s"Required argument \"$label\" was never invoked"))
                    case defaultValue: T => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidArgument(s"Cannot invoke argument \"$label\" multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Right(defaultValue)
        
        Argument(label, Nil, genCanInvoke, genInvoke, genParam, paramIndex)

object CtxOptionTranslation:
    inline def translate[
        Ctx,
        T,
        FL <: Maybe[Label],
        SL <: Maybe[CharLabel],
        RQ <: CliElement.IsRequired,
        MD <: CliOption.Mode,
        Ord <: ContextCliElement.EvalOrder,
    ](
        opt: CliContextOption[Ctx, T, FL, SL, RQ, MD, Ord],
    ): CtxOption =
        val labels: DoubleLabel = SelectorTranslation.doubleLabel[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: CtxOption => (String, Int) => OrErr[CtxOption] = (dirOpt) => (argument, applIndex) =>
            if (dirOpt.applications.isEmpty)
                opt.parse(argument).map(
                    parsedVal =>
                        dirOpt.copy(applications = List(Appl(parsedVal, applIndex)))
                )
            else inline erasedValue[MD] match
                case _: CliOption.Single =>
                    Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                case _: CliOption.Multi =>
                    opt.parse(argument).map(
                        parsedVal =>
                            dirOpt.copy(
                                applications = dirOpt.applications :+ Appl(parsedVal, applIndex),
                            )
                    )

        val genUpdate: CtxOption => Any => OrErr[Any] = inline erasedValue[CliOption.Result[T, RQ, MD]] match
            case _: List[T] =>
                inline opt.default match
                    case _: Unit =>
                        (ctxOpt) =>
                            if (ctxOpt.applications.isEmpty)
                                _ => Left(InvalidOption(s"Must invoke $labels at least once"))
                            else
                                v => Right(opt.update(v.asInstanceOf[Ctx], ctxOpt.applications.sortBy(_.index).map(_.value).asInstanceOf[CliOption.Result[T, RQ, MD]]))
                    case defaultValues: List[T] =>
                        (ctxOpt) =>
                            if (ctxOpt.applications.isEmpty)
                                v => Right(opt.update(v.asInstanceOf[Ctx], defaultValues.asInstanceOf[CliOption.Result[T, RQ, MD]]))
                            else
                                v => Right(opt.update(v.asInstanceOf[Ctx], ctxOpt.applications.sortBy(_.index).map(_.value).asInstanceOf[CliOption.Result[T, RQ, MD]]))
            case _: Option[T] =>
                (ctxOpt) =>
                    ctxOpt.applications match
                        case head :: other :: _ =>
                            v => Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                        case head :: Nil =>
                            v => Right(ctxOpt.update(v.asInstanceOf[Ctx], Some(head)))
                        case Nil =>
                            v => Right(ctxOpt.update(v.asInstanceOf[Ctx], None))

            case _: Option[List[T]] =>
                (ctxOpt) =>
                    ctxOpt.applications match
                        case Nil =>
                            v => Right(ctxOpt.update(v.asInstanceOf[Ctx], None))
                        case other =>
                            v => Right(ctxOpt.update(v.asInstanceOf[Ctx], Some(other.sortBy(_.index).map(_.value))))

                        
            case _: T =>
                inline opt.default match
                    case _: Unit => (ctxOpt) =>
                        ctxOpt.applications match
                            case head :: other :: _ =>
                                v => Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                            case head :: Nil =>
                                v => Right(opt.update(v.asInstanceOf[Ctx], head.value.asInstanceOf[CliOption.Result[T, RQ, MD]]))
                            case Nil =>
                                v => Left(InvalidOption(s"Required option $labels was never invoked"))
                    case defaultValue: T => (ctxOpt) =>
                        ctxOpt.applications match
                            case head :: other :: _ =>
                                v => Left(InvalidOption(s"Cannot invoke $labels multiple times"))
                            case head :: Nil =>
                                v => Right(opt.update(v.asInstanceOf[Ctx], head.value.asInstanceOf[CliOption.Result[T, RQ, MD]]))
                            case Nil =>
                                v => Right(opt.update(v.asInstanceOf[Ctx], defaultValue.asInstanceOf[CliOption.Result[T, RQ, MD]]))

        CtxOption(labels, Nil, genInvoke, genUpdate)

object CtxFlagTranslation:
    inline def translate[
        T,
        FL <: Maybe[Label],
        SL <: Maybe[CharLabel],
        Ord <: ContextCliElement.EvalOrder,
    ](opt: CliContextFlag[T, FL, SL, Ord]): CtxFlag =
        val labels: DoubleLabel = SelectorTranslation.doubleLabel[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: CtxFlag => Int => OrErr[CtxFlag] = (dirFlag) => (applIndex) =>
            if (dirFlag.application.isEmpty)
                Right(dirFlag.copy(application = Some(applIndex)))
            else
                Left(InvalidOption(s"Can't invoke $labels multiple times"))
        
        val genUpdate: CtxFlag => Any => OrErr[Any] = (dirFlag) =>
            dirFlag.application match
                case None => v => Right(opt.update(v.asInstanceOf[T], false))
                case Some(_) => v => Right(opt.update(v.asInstanceOf[T], true))

        CtxFlag(labels, None, genInvoke, genUpdate)


object CtxOneOfTranslation:
    private inline def translateOptions[Opts <: Tuple](opts: Opts): List[CtxOption | CtxFlag] =
        inline opts match
            case EmptyTuple => Nil
            case (opt: CliContextOption[a, b, c, d, e, f, g]) *: tail =>
                CtxOptionTranslation.translate(opt) :: translateOptions(tail)
            case (flg: CliContextFlag[a, b, c, d]) *: tail =>
                CtxFlagTranslation.translate(flg) :: translateOptions(tail)

    private inline def optionsShow[Opts <: Tuple](opts: Opts): List[String] =
        inline opts match
            case EmptyTuple => Nil
            case (opt: CliContextOption[a, b, c, d, e, f, g]) *: tail =>
                SelectorTranslation.doubleLabel[c, d](opt.fullLabel, opt.shortLabel).toString() :: optionsShow(tail)
            case (flg: CliContextFlag[a, b, c, d]) *: tail =>
                SelectorTranslation.doubleLabel[b, c](flg.fullLabel, flg.shortLabel).toString() :: optionsShow(tail)

    inline def translate[
        Ctx,
        Eles <: NonEmptyTuple,
    ](opt: CliContextOneOf[Ctx, Eles]): CtxOneOf =
        val optionLabels: List[String] = optionsShow(opt.elements)
        val options = translateOptions(opt.elements)
        val matchesAndIsFlag: LabelSelector => Option[Boolean] =
            (sel) => options.find(_.matches(sel)).map {
                case CtxFlag(_, _, _, _) => true
                case _ => false
            }

        val genInvoke: (CtxOneOf) => (LabelSelector, Option[String], Int) => OrErr[CtxOneOf] =
            (ctxOneOf) => (sel, argOpt, applIndex) =>
                ctxOneOf.selection match
                    case Some(ctxEle) if ctxEle.matches(sel) => ctxEle match
                        case ctxOpt @ CtxOption(_, _, _, _) => argOpt match
                            case None => Left(InvalidOption(s"$sel requires an argument"))
                            case Some(value) =>
                                ctxOpt.invoke(value, applIndex)
                                    .map(updated => ctxOneOf.copy(selection = Some(updated)))
                                
                        case ctxFlag @ CtxFlag(_, _, _, _) =>
                            argOpt match
                                case Some(_) => Left(InvalidOption(s"$sel does not take an argument"))
                                case None =>
                                    ctxFlag.invoke(applIndex)
                                        .map(updated => ctxOneOf.copy(selection = Some(updated)))

                    case Some(opt) =>
                        Left(InvalidOption(s"$sel is not a valid option"))
                        
                    case None => 
                        options.find {
                            _.matches(sel)
                        } match
                            case None => Left(InvalidOption(s"$sel does not match any option or flag"))
                            case Some(ctxEle @ CtxOption(_, _, _, _)) =>
                                argOpt match
                                    case None => Left(InvalidOption(s"$sel requires an argument"))
                                    case Some(value) =>
                                        ctxEle.invoke(value, applIndex)
                                            .map(updatedDir => ctxOneOf.copy(selection = Some(updatedDir)))

                            case Some(ctxEle @ CtxFlag(_, _, _, _)) =>
                                argOpt match
                                    case Some(_) => Left(InvalidOption(s"$sel is a flag and does not take an argument"))
                                    case None =>
                                        ctxEle.invoke(applIndex)
                                            .map(updatedDir => ctxOneOf.copy(selection = Some(updatedDir)))
        
        val genUpdate: CtxOneOf => Any => OrErr[Any] = (ctxOneOf) =>
            ctxOneOf.selection.map(_.update) match
                case None =>
                    Any => Left(ParseError(s"At least one of the following options must be invoked: ${optionLabels.mkString(", ")}"))
                case Some(res) => res

        CtxOneOf(matchesAndIsFlag, None, genInvoke, genUpdate)


object CommandTranslation:
    inline def translateDirectElements[DirEles <: Tuple](
        inline eles: DirEles,
        inline paramIndex: Int = 0
    ): (List[RuntimeDirElement & RuntimeOptLike], List[Argument]) = inline erasedValue[DirEles] match
        case _: EmptyTuple => (Nil.asInstanceOf[List[RuntimeDirElement & RuntimeOptLike]], Nil.asInstanceOf[List[Argument]])
        case _: (CliOption[a, b, c, d, e] *: tail) =>
            val typedEles = eles.inlineAs[CliOption[a, b, c, d, e] *: tail]
            val (nextOpts, nextArgs) = translateDirectElements(typedEles.tail, paramIndex + 1)
            (DirOptionTranslation.translate(typedEles.head, paramIndex) :: nextOpts, nextArgs)
        case _: (CliFlag[a, b, c] *: tail)  =>
            val typedEles = eles.inlineAs[CliFlag[a, b, c] *: tail]
            val (nextOpts, nextArgs) = translateDirectElements(typedEles.tail, paramIndex + 1)
            (DirFlagTranslation.translate(typedEles.head, paramIndex) :: nextOpts, nextArgs)
        case _: (CliOneOf[a, b] *: tail) =>
            val typedEles = eles.inlineAs[CliOneOf[a, b] *: tail]
            val (nextOpts, nextArgs) = translateDirectElements(typedEles.tail, paramIndex + 1)
            (DirOneOfTranslation.translate(typedEles.head, paramIndex) :: nextOpts, nextArgs)
        case _: (CliAllOf[a, b, c] *: tail) =>
            val typedEles = eles.inlineAs[CliAllOf[a, b, c] *: tail]
            val (nextOpts, nextArgs) = translateDirectElements(typedEles.tail, paramIndex + 1)
            (DirAllOfTranslation.translate(typedEles.head, paramIndex) :: nextOpts, nextArgs)
        case _: (CliArgument[a, b, c, d] *: tail) =>
            val typedEles = eles.inlineAs[CliArgument[a, b, c, d] *: tail]
            val (nextOpts, nextArgs) = translateDirectElements(typedEles.tail, paramIndex + 1)
            (nextOpts, ArgumentTranslation.translate(typedEles.head, paramIndex) :: nextArgs)

    inline def getCtxEvalOrder[Ord <: ContextCliElement.EvalOrder]: ContextCliElement.EvalOrder =
        inline erasedValue[Ord] match
            case _: ContextCliElement.Eager => ContextCliElement.Eager
            case _: ContextCliElement.Default => ContextCliElement.Eager
            case _: ContextCliElement.Lazy => ContextCliElement.Lazy

    inline def translateCtxEle[Ele](ele: Ele): (RuntimeCtxElement, ContextCliElement.EvalOrder) =
        inline ele match
            case typedEle: CliContextOption[a, b, c, d, e, f, ord] =>
                (CtxOptionTranslation.translate(typedEle), getCtxEvalOrder[ord])
            case typedEle: CliContextFlag[a, b, c, ord] =>
                (CtxFlagTranslation.translate(typedEle), getCtxEvalOrder[ord])
            case typedEle: CliContextOneOf[a, b] =>
                (CtxOneOfTranslation.translate(typedEle), ContextCliElement.Default)
        
    inline def translateContextElements[CtxEles <: Tuple](
        eles: CtxEles,
        paramIndex: Int = 0
    ): (List[RuntimeCtxElement], List[RuntimeCtxElement], List[RuntimeCtxElement]) =
        inline erasedValue[CtxEles] match
            case _: EmptyTuple => (Nil, Nil, Nil).asInstanceOf[(List[RuntimeCtxElement], List[RuntimeCtxElement], List[RuntimeCtxElement])]
            case _: (CliContextOption[a, b, c, d, e, f, g] *: tail) =>
                val typedEles = eles.inlineAs[CliContextOption[a, b, c, d, e, f, g] *: tail]
                val (nextEager, next, nextLazy) = translateContextElements(typedEles.tail, paramIndex + 1)
                val (headEle, order) = translateCtxEle(typedEles.head)
                    order match
                        case ContextCliElement.Eager => (headEle :: nextEager, next, nextLazy)
                        case ContextCliElement.Default => (nextEager, headEle :: next, nextLazy)
                        case ContextCliElement.Lazy => (nextEager, next, headEle :: nextLazy)

            case _: (CliContextFlag[a, b, c, d] *: tail) =>
                val typedEles = eles.inlineAs[CliContextFlag[a, b, c, d] *: tail]
                val (nextEager, next, nextLazy) = translateContextElements(typedEles.tail, paramIndex + 1)
                val (headEle, order) = translateCtxEle(typedEles.head)
                    order match
                        case ContextCliElement.Eager => (headEle :: nextEager, next, nextLazy)
                        case ContextCliElement.Default => (nextEager, headEle :: next, nextLazy)
                        case ContextCliElement.Lazy => (nextEager, next, headEle :: nextLazy)

            case _: (CliContextOneOf[a, b] *: tail) =>
                val typedEles = eles.inlineAs[CliContextOneOf[a, b] *: tail]
                val (nextEager, next, nextLazy) = translateContextElements(typedEles.tail, paramIndex + 1)
                val (headEle, order) = translateCtxEle(typedEles.head)
                    order match
                        case ContextCliElement.Eager => (headEle :: nextEager, next, nextLazy)
                        case ContextCliElement.Default => (nextEager, headEle :: next, nextLazy)
                        case ContextCliElement.Lazy => (nextEager, next, headEle :: nextLazy)
                

    inline def listToTuple[Tup <: Tuple]: List[Any] => Tup = inline erasedValue[Tup] match
        case _: EmptyTuple => (list) => EmptyTuple.asInstanceOf[Tup]
        case _: (tupHead *: tupTail) =>
            val nextFunction = listToTuple[tupTail]
            (list) => {
                (list.head *: nextFunction(list.tail)).asInstanceOf[Tup]
            }

    inline def generateListConverter[DirEles <: Tuple, Ctx]: List[Any] => CliCommand.Params[DirEles, Ctx] =
        inline erasedValue[Ctx] match
            case _: Unit => inline erasedValue[DirEles] match
                case _: EmptyTuple => (list) => ().asInstanceOf[CliCommand.Params[DirEles, Ctx]]
                case _: (_ *: EmptyTuple) => (list) => list.tail.head.asInstanceOf[CliCommand.Params[DirEles, Ctx]]
                case _: (_ *: _ *: _) =>
                    val listConverter = listToTuple[DirEles]
                    (list) => listConverter(list.tail).asInstanceOf[CliCommand.Params[DirEles, Ctx]]
            case _ => inline erasedValue[DirEles] match
                case _: EmptyTuple => (list) => list.head.asInstanceOf[CliCommand.Params[DirEles, Ctx]]
                case _ =>
                    val listConverter = listToTuple[Ctx *: DirEles]
                    (list) => {
                        listConverter(list).asInstanceOf[CliCommand.Params[DirEles, Ctx]]
                    }

    inline def translate[
        T,
        Ctx,
        DirEles <: Tuple,
        CtxEles <: Tuple,
        DefCtx <: CliCommand.DefaultEvalOrder,
    ](inline cmd: CliCommand[T, Ctx, DirEles, CtxEles, DefCtx]): Command =
        inline translateContextElements[CtxEles](cmd.contextElements) match
            case (eagerLocalCtxEles, localCtxEles, lazyLocalCtxEles) =>
                inline translateDirectElements(cmd.directElements) match
                    case (options, arguments) =>
                        val listConverter: List[Any] => CliCommand.Params[DirEles, Ctx] = generateListConverter[DirEles, Ctx]
                        val handler: List[Any] => Any = (list) => cmd.handler(listConverter(list))
                            
                        Command(
                            Nil,
                            Nil,
                            Nil,
                            Nil,
                            eagerLocalCtxEles,
                            localCtxEles,
                            lazyLocalCtxEles,
                            Nil,
                            Nil,
                            Nil,
                            options,
                            arguments,
                            handler,
                            0,
                        )

object CommandGroupTranslation:
    inline def translateCommands[Cmds <: Tuple](cmds: Cmds): ListMap[String, RuntimeCommandElement] =
        inline erasedValue[Cmds] match
            case _: EmptyTuple => ListMap.empty[String, RuntimeCommandElement]
            case _: ((String, CliCommand[a, b, c, d, e]) *: tail) => 
                val typedCmds = cmds.inlineAs[(String, CliCommand[a, b, c, d, e]) *: tail]
                ListMap((typedCmds.head._1,  CommandTranslation.translate(typedCmds.head._2))) ++ translateCommands(typedCmds.tail)
            case _: ((String, CliCommandGroup[a, b, c, d, e, f]) *: tail) =>
                val typedCmds = cmds.asInstanceOf[(String, CliCommandGroup[a, b, c, d, e, f]) *: tail]
                ListMap((typedCmds.head._1 -> CommandGroupTranslation.translate(typedCmds.head._2))) ++ translateCommands(typedCmds.tail)

    inline def translate[
        T,
        Ctx,
        Cmds <: Tuple,
        LocEles <: Tuple,
        GlbEles <: Tuple,
        DefCtx <: CliCommand.DefaultEvalOrder,
    ](grp: CliCommandGroup[T, Ctx, Cmds, LocEles, GlbEles, DefCtx]): CommandGroup =
        val (eagerLocalCtxEles, localCtxEles, lazyLocalCtxEles) =
            CommandTranslation.translateContextElements[LocEles](grp.localContextElements)

        val labeledCommands: ListMap[String, RuntimeCommandElement] =
            translateCommands(grp.labeledCommands)

        val handler: Any => Any = v => grp.handler(v.asInstanceOf[CliCommandGroup.Params[Cmds]])  

        CommandGroup(
            Nil,
            labeledCommands,
            Nil,
            Nil,
            Nil,
            eagerLocalCtxEles,
            localCtxEles,
            lazyLocalCtxEles,
            Nil,
            Nil,
            Nil,
            handler,
            0,
        )
