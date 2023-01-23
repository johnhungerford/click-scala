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

object SelectorTranslation:
    inline def matcherFrom[FL <: Maybe[Label], SL <: Maybe[CharLabel]](fl: FL, sl: SL): LabelSelector => Boolean =
        val longLabel: Option[String] = inline erasedValue[FL] match
            case _: Unit => None
            case _ => Some(fl.asInstanceOf[String])
        val shortLabel: Option[Char] = inline erasedValue[SL] match
            case _: Unit => None
            case _ => Some(sl.asInstanceOf[Char])
        
        (selector: LabelSelector) => selector match
            case RuntimeCommandElement.LabelSelector.Short(label) =>
                shortLabel.contains(label)
            case RuntimeCommandElement.LabelSelector.Long(label) =>
                longLabel.contains(label)
    
    inline def labelShow[FL <: Maybe[Label], SL <: Maybe[CharLabel]](fl: FL, sl: SL): String =
        inline fl match
            case _: String => inline sl match
                case _: Char => s"-$sl/--$fl"
                case _: Unit => s"--$fl"
            case _: Unit => inline sl match
                case _: Char => s"-$sl"
                case _: Unit => error("Neither the full label nor the short label is defined")
    

object DirOptionTranslation:
    inline def translate[
        T,
        FL <: Maybe[Label],
        SL <: Maybe[CharLabel],
        RQ <: CliElement.IsRequired,
        MD <: CliOption.Mode,
    ](opt: CliOption[T, FL, SL, RQ, MD], paramIndex: Int): DirOption =
        val labelShow: String = SelectorTranslation.labelShow[FL, SL](opt.fullLabel, opt.shortLabel).toString()
        val selector = SelectorTranslation.matcherFrom[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: (DirOption, String, Int) => OrErr[DirOption] = (dirOpt, argument, applIndex) =>
            if (dirOpt.applications.isEmpty)
                opt.parse(argument).map(
                    parsedVal =>
                        dirOpt.copy(applications = List(Appl(parsedVal, applIndex)))
                )
            else inline erasedValue[MD] match
                case _: CliOption.Single =>
                    Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                case _: CliOption.Multi =>
                    opt.parse(argument).map(
                        parsedVal =>
                            dirOpt.copy(
                                applications = Appl(parsedVal, applIndex)::dirOpt.applications,
                            )
                    )
        
        val genParam: DirOption => OrErr[Any] = inline erasedValue[CliOption.Result[T, MD]] match
            case _: List[T] =>
                inline opt.default match
                    case _: Unit =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Left(InvalidOption(s"Must invoke $labelShow at least once"))
                            else
                                Right(dirOpt.applications.map(_.value))
                    case defaultValues: List[T] =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Right(defaultValues)
                            else
                                Right(dirOpt.applications.map(_.value))
            case _: Option[T] =>
                (dirOpt) =>
                    dirOpt.applications match
                        case head :: other :: _ =>
                            Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                        case head :: Nil =>
                            Right(Some(head.value))
                        case Nil =>
                            Right(None)
            case _: T =>
                inline opt.default match
                    case _: Unit => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Left(InvalidOption(s"Required option $labelShow was never invoked"))
                    case defaultValue: T => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Right(defaultValue)
                

        DirOption(selector, Nil, genInvoke, genParam, paramIndex)

object DirFlagTranslation:
    inline def translate[
        T,
        FL <: Maybe[Label],
        SL <: Maybe[CharLabel],
    ](opt: CliFlag[T, FL, SL], paramIndex: Int): DirFlag =
        val labelShow: String = SelectorTranslation.labelShow[FL, SL](opt.fullLabel, opt.shortLabel).toString()
        val selector = SelectorTranslation.matcherFrom[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: (DirFlag, Int) => OrErr[DirFlag] = (dirFlag, applIndex) =>
            if (dirFlag.application.isEmpty)
                Right(dirFlag.copy(application = Some(applIndex)))
            else
                Left(InvalidOption(s"Can't invoke $labelShow multiple times"))
        
        val genParam: DirFlag => OrErr[Any] = (dirFlag) =>
            dirFlag.application match
                case None => Right(opt.value(false))
                case Some(_) => Right(opt.value(true))
            
        DirFlag(selector, None, genInvoke, genParam, paramIndex)

object DirOneOfTranslation:
    private inline def translateOptions[Opts <: Tuple](opts: Opts): List[DirOption | DirFlag] =
        inline opts match
            case EmptyTuple => Nil
            case (opt: CliOption[a, b, c, d, e]) *: tail =>
                DirOptionTranslation.translate(opt, -1) :: translateOptions(tail)
            case (flg: CliFlag[a, b, c]) *: tail =>
                DirFlagTranslation.translate(flg, -1) :: translateOptions(tail)
    
    private inline def optionsShow[Opts <: Tuple](opts: Opts): List[String] =
        inline opts match
            case EmptyTuple => Nil
            case (opt: CliOption[a, b, c, d, e]) *: tail =>
                SelectorTranslation.labelShow[b, c](opt.fullLabel, opt.shortLabel).toString() :: optionsShow(tail)
            case (flg: CliFlag[a, b, c]) *: tail =>
                SelectorTranslation.labelShow[b, c](flg.fullLabel, flg.shortLabel).toString() :: optionsShow(tail)
    
    inline def translate[
        T,
        Eles <: NonEmptyTuple,
    ](opt: CliOneOf[T, Eles], paramIndex: Int): DirOneOf =
        val optionLabels: List[String] = optionsShow(opt.elements)
        val options = translateOptions(opt.elements)
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
        
        val genParam: DirOneOf => OrErr[Any] = (dirOneOf) =>
            dirOneOf.selection.map(_.param) match
                case None =>
                    Left(ParseError(s"At least one of the following options must be invoked: ${optionLabels.mkString(", ")}"))
                case Some(res) => res

        DirOneOf(matchesAndIsFlag, None, genInvoke, genParam, paramIndex)

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
        val labelShow: String = constValue[LB]
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
                        Left(InvalidOption(s"Cannot invoke argument \"$labelShow\" more than once"))
                    else argt.parse(arg)
                        .map(argVal => ele.copy(applications = List(Appl(argVal, index))))
                }
                case _: CliArgument.AnyMulti => { (ele, arg, index) =>
                    argt.parse(arg)
                        .map(argVal => ele.copy(applications = Appl(argVal, index) :: ele.applications))
                }
                case _: CliArgument.AtLeast[n] => { (ele, arg, index) =>
                    argt.parse(arg)
                        .map(argVal => ele.copy(applications = Appl(argVal, index) :: ele.applications))
                }
                case _: CliArgument.NoMoreThan[n] =>
                    val upperLimit = natToInt[n]
                    { (ele, arg, index) =>
                        if (ele.applications.size >= upperLimit)
                            Left(InvalidOption(s"Cannot invoke argument \"$labelShow\" more than $upperLimit times"))
                        else argt.parse(arg)
                            .map(argVal => ele.copy(applications = Appl(argVal, index) :: ele.applications))
                    }

                case _: CliArgument.Exactly[n] =>
                    val upperLimit = natToInt[n]
                    { (ele, arg, index) =>
                        if (ele.applications.size >= upperLimit)
                            Left(InvalidOption(s"Cannot invoke argument \"$labelShow\" more than $upperLimit times"))
                        else argt.parse(arg)
                            .map(argVal => ele.copy(applications = Appl(argVal, index) :: ele.applications))
                    }
        val genParam: Argument => OrErr[Any] = inline erasedValue[CliArgument.Result[T, MD]] match
            case _: List[T] =>
                inline argt.default match
                    case _: Unit =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Left(InvalidOption(s"Must invoke argument \"$labelShow\" at least once"))
                            else
                                Right(dirOpt.applications.map(_.value))
                    case defaultValues: List[T] =>
                        (dirOpt) =>
                            if (dirOpt.applications.isEmpty)
                                Right(defaultValues)
                            else
                                Right(dirOpt.applications.map(_.value))
            case _: Option[T] =>
                (dirOpt) =>
                    dirOpt.applications match
                        case head :: other :: _ =>
                            Left(InvalidOption(s"Cannot invoke argument \"$labelShow\" multiple times"))
                        case head :: Nil =>
                            Right(Some(head.value))
                        case Nil =>
                            Right(None)
            case _: T =>
                inline argt.default match
                    case _: Unit => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidArgument(s"Cannot invoke argument \"$labelShow\" multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Left(InvalidArgument(s"Required argument \"$labelShow\" was never invoked"))
                    case defaultValue: T => (dirOpt) =>
                        dirOpt.applications match
                            case head :: other :: _ =>
                                Left(InvalidArgument(s"Cannot invoke argument \"$labelShow\" multiple times"))
                            case head :: Nil =>
                                Right(head.value)
                            case Nil =>
                                Right(defaultValue)
        
        Argument(Nil, genCanInvoke, genInvoke, genParam, paramIndex)

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
        val labelShow: String = SelectorTranslation.labelShow[FL, SL](opt.fullLabel, opt.shortLabel).toString()
        val selector = SelectorTranslation.matcherFrom[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: CtxOption => (String, Int) => OrErr[CtxOption] = (dirOpt) => (argument, applIndex) =>
            if (dirOpt.applications.isEmpty)
                opt.parse(argument).map(
                    parsedVal =>
                        dirOpt.copy(applications = List(Appl(parsedVal, applIndex)))
                )
            else inline erasedValue[MD] match
                case _: CliOption.Single =>
                    Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                case _: CliOption.Multi =>
                    opt.parse(argument).map(
                        parsedVal =>
                            dirOpt.copy(
                                applications = Appl(parsedVal, applIndex)::dirOpt.applications,
                            )
                    )

        val genUpdate: CtxOption => Any => OrErr[Any] = inline erasedValue[CliOption.Result[T, MD]] match
            case _: List[T] =>
                inline opt.default match
                    case _: Unit =>
                        (ctxOpt) =>
                            if (ctxOpt.applications.isEmpty)
                                _ => Left(InvalidOption(s"Must invoke $labelShow at least once"))
                            else
                                v => Right(opt.update(v.asInstanceOf[Ctx], ctxOpt.applications.map(_.value).asInstanceOf[CliOption.Result[T, MD]]))
                    case defaultValues: List[T] =>
                        (ctxOpt) =>
                            if (ctxOpt.applications.isEmpty)
                                v => Right(opt.update(v.asInstanceOf[Ctx], defaultValues.asInstanceOf[CliOption.Result[T, MD]]))
                            else
                                v => Right(opt.update(v.asInstanceOf[Ctx], ctxOpt.applications.map(_.value).asInstanceOf[CliOption.Result[T, MD]]))
            case _: Option[T] =>
                (ctxOpt) =>
                    ctxOpt.applications match
                        case head :: other :: _ =>
                            v => Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                        case head :: Nil =>
                            v => Right(ctxOpt.update(v.asInstanceOf[Ctx], Some(head)))
                        case Nil =>
                            v => Right(ctxOpt.update(v.asInstanceOf[Ctx], None))
            case _: T =>
                inline opt.default match
                    case _: Unit => (ctxOpt) =>
                        ctxOpt.applications match
                            case head :: other :: _ =>
                                v => Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                            case head :: Nil =>
                                v => Right(opt.update(v.asInstanceOf[Ctx], head.value.asInstanceOf[CliOption.Result[T, MD]]))
                            case Nil =>
                                v => Left(InvalidOption(s"Required option $labelShow was never invoked"))
                    case defaultValue: T => (ctxOpt) =>
                        ctxOpt.applications match
                            case head :: other :: _ =>
                                v => Left(InvalidOption(s"Cannot invoke $labelShow multiple times"))
                            case head :: Nil =>
                                v => Right(opt.update(v.asInstanceOf[Ctx], head.value.asInstanceOf[CliOption.Result[T, MD]]))
                            case Nil =>
                                v => Right(opt.update(v.asInstanceOf[Ctx], defaultValue.asInstanceOf[CliOption.Result[T, MD]]))

        CtxOption(selector, Nil, genInvoke, genUpdate)

object CtxFlagTranslation:
    inline def translate[
        T,
        FL <: Maybe[Label],
        SL <: Maybe[CharLabel],
        Ord <: ContextCliElement.EvalOrder,
    ](opt: CliContextFlag[T, FL, SL, Ord]): CtxFlag =
        val labelShow: String = SelectorTranslation.labelShow[FL, SL](opt.fullLabel, opt.shortLabel).toString()
        val selector = SelectorTranslation.matcherFrom[FL, SL](opt.fullLabel, opt.shortLabel)
        val genInvoke: CtxFlag => Int => OrErr[CtxFlag] = (dirFlag) => (applIndex) =>
            if (dirFlag.application.isEmpty)
                Right(dirFlag.copy(application = Some(applIndex)))
            else
                Left(InvalidOption(s"Can't invoke $labelShow multiple times"))
        
        val genUpdate: CtxFlag => Any => OrErr[Any] = (dirFlag) =>
            dirFlag.application match
                case None => v => Right(opt.update(v.asInstanceOf[T], false))
                case Some(_) => v => Right(opt.update(v.asInstanceOf[T], true))

        CtxFlag(selector, None, genInvoke, genUpdate)


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
                SelectorTranslation.labelShow[c, d](opt.fullLabel, opt.shortLabel).toString() :: optionsShow(tail)
            case (flg: CliContextFlag[a, b, c, d]) *: tail =>
                SelectorTranslation.labelShow[b, c](flg.fullLabel, flg.shortLabel).toString() :: optionsShow(tail)

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
        case _: EmptyTuple => (Nil, Nil)
        case _: (CliOption[a, b, c, d, e] *: tail) =>
            val typedEles = eles.asInstanceOf[CliOption[a, b, c, d, e] *: tail]
            val (nextOpts, nextArgs) = translateDirectElements(typedEles.tail, paramIndex + 1)
            (DirOptionTranslation.translate(typedEles.head, paramIndex) :: nextOpts, nextArgs)
        case _: (CliFlag[a, b, c] *: tail)  =>
            val typedEles = eles.asInstanceOf[CliFlag[a, b, c] *: tail]
            val (nextOpts, nextArgs) = translateDirectElements(typedEles.tail, paramIndex + 1)
            (DirFlagTranslation.translate(typedEles.head, paramIndex) :: nextOpts, nextArgs)
        case _: (CliArgument[a, b, c, d] *: tail) =>
            val typedEles = eles.asInstanceOf[CliArgument[a, b, c, d] *: tail]
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
            case _: EmptyTuple => (Nil, Nil, Nil)
            case _: (CliContextOption[a, b, c, d, e, f, g] *: tail) =>
                val typedEles = eles.asInstanceOf[CliContextOption[a, b, c, d, e, f, g] *: tail]
                val (nextEager, next, nextLazy) = translateContextElements(typedEles.tail, paramIndex + 1)
                val (headEle, order) = translateCtxEle(typedEles.head)
                    order match
                        case ContextCliElement.Eager => (headEle :: nextEager, next, nextLazy)
                        case ContextCliElement.Default => (nextEager, headEle :: next, nextLazy)
                        case ContextCliElement.Lazy => (nextEager, next, headEle :: nextLazy)

            case _: (CliContextFlag[a, b, c, d] *: tail) =>
                val typedEles = eles.asInstanceOf[CliContextFlag[a, b, c, d] *: tail]
                val (nextEager, next, nextLazy) = translateContextElements(typedEles.tail, paramIndex + 1)
                val (headEle, order) = translateCtxEle(typedEles.head)
                    order match
                        case ContextCliElement.Eager => (headEle :: nextEager, next, nextLazy)
                        case ContextCliElement.Default => (nextEager, headEle :: next, nextLazy)
                        case ContextCliElement.Lazy => (nextEager, next, headEle :: nextLazy)

            case _: (CliContextOneOf[a, b] *: tail) =>
                val typedEles = eles.asInstanceOf[CliContextOneOf[a, b] *: tail]
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
                        // val handler: List[Any] => Any = inline erasedValue[CliCommand.Params[DirEles, Ctx]] match
                        //     case _: (Ctx *: tail) =>
                        //         println("HI")
                        //         val listConverter = listToTuple[Ctx *: tail]
                        //         (list) => {
                        //             // println(listConverter(list))
                        //             cmd.handler(listConverter(list).asInstanceOf[CliCommand.Params[DirEles, Ctx]])
                        //         }
                        //     case _: (t *: tail) =>
                        //         println("THERE")
                        //         val listConverter = listToTuple[Ctx *: tail]
                        //         (list) => {
                        //             // println(listConverter(list))
                        //             cmd.handler(listConverter(list.tail).asInstanceOf[CliCommand.Params[DirEles, Ctx]])
                        //         }
                        //     case _: EmptyTuple => _ => {
                        //         println("EmptyTuple")
                        //         cmd.handler(EmptyTuple.asInstanceOf[CliCommand.Params[DirEles, Ctx]])
                        //     }
                        //     case _: Ctx => list => {
                        //         println("Ctx")
                        //         cmd.handler(list.head.asInstanceOf[CliCommand.Params[DirEles, Ctx]])
                        //     }
                        //     case _ => list => {
                        //         println(s"Default: $list")
                        //         cmd.handler(list.tail.head.asInstanceOf[CliCommand.Params[DirEles, Ctx]])
                        //     }
                            
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
                val typedCmds = cmds.asInstanceOf[(String, CliCommand[a, b, c, d, e]) *: tail]
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
