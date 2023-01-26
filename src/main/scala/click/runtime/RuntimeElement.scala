package click.runtime

import click.exception.ClickException
import ClickException.OrErr
import scala.collection.mutable.ListMap
import RuntimeCommandElement.{LabelSelector, OneOfApplication, Appl}
import click.runtime.RuntimeCommandElement.DoubleLabel

sealed trait RuntimeElement

sealed trait RuntimeDirElement:
    def param: OrErr[Any]
    def paramIndex: Int

sealed trait RuntimeCtxElement:
    def update: Any => OrErr[Any]

sealed trait RuntimeOptLike:
    def matches: LabelSelector => Boolean

sealed trait RuntimeMulti extends RuntimeOptLike:
    def matchesAndIsFlag: LabelSelector => Option[Boolean]
    final lazy val matches: LabelSelector => Boolean =
        ls => matchesAndIsFlag(ls).nonEmpty

type RuntimeDirOptLike = RuntimeDirElement & RuntimeOptLike

case class DirOption(
    labels: DoubleLabel,
    applications: List[Appl[Any]],
    private val genInvoke: (DirOption, String, Int) => OrErr[DirOption],
    private val genParam: DirOption => OrErr[Any],
    override val paramIndex: Int,
) extends RuntimeDirElement with RuntimeOptLike:
    override val matches: LabelSelector => Boolean = _.matches(labels)
    val invoke: (String, Int) => OrErr[DirOption] =
        (str, int) => genInvoke(this, str, int)

    override val param: OrErr[Any] = genParam(this)

case class DirFlag(
    labels: DoubleLabel,
    application: Option[Int],
    private val genInvoke: (DirFlag, Int) => OrErr[DirFlag],
    private val genParam: DirFlag => OrErr[Any],
    override val paramIndex: Int,
) extends RuntimeDirElement with RuntimeOptLike:
    override val matches: LabelSelector => Boolean = _.matches(labels)
    val invoke: Int => OrErr[DirFlag] = int => genInvoke(this, int)
    override val param: OrErr[Any] = genParam(this)


case class DirOneOf(
    override val matchesAndIsFlag: LabelSelector => Option[Boolean],
    selection: Option[DirOption | DirFlag | DirOneOf | DirAllOf],
    private val genInvoke: (DirOneOf, LabelSelector, Option[String], Int) => OrErr[DirOneOf],
    private val genParam: DirOneOf => OrErr[Any],
    override val paramIndex: Int,
) extends RuntimeDirElement with RuntimeMulti:
    override val param: OrErr[Any] = genParam(this)
    val invokeFlag: (LabelSelector, Int) => OrErr[DirOneOf] =
        (sel, int) => genInvoke(this, sel, None, int)
    val invokeOpt: (LabelSelector, String, Int) => OrErr[DirOneOf] =
        (sel, str, int) => genInvoke(this, sel, Some(str), int)


case class DirAllOf(
    override val matchesAndIsFlag: LabelSelector => Option[Boolean],
    private val indexedElements: Vector[(Int, DirOption | DirFlag | DirOneOf | DirAllOf)],
    private val genInvoke: (DirAllOf, LabelSelector, Option[String], Int) => OrErr[DirAllOf],
    private val genParam: DirAllOf => OrErr[Any],
    override val paramIndex: Int,
) extends RuntimeDirElement with RuntimeMulti:
    override val param: OrErr[Any] = genParam(this)
    val invokeFlag: (LabelSelector, Int) => OrErr[DirAllOf] =
        (sel, int) => genInvoke(this, sel, None, int)
    val invokeOpt: (LabelSelector, String, Int) => OrErr[DirAllOf] =
        (sel, str, int) => genInvoke(this, sel, Some(str), int)

    lazy val elements: List[DirOption | DirFlag | DirOneOf | DirAllOf] = indexedElements.toList.map(_._2)

    def findElement(selector: LabelSelector): Option[DirOption | DirFlag | DirOneOf | DirAllOf] =
        indexedElements.find(_._2.matches(selector)).map(_._2)

    def updateElement(selector: LabelSelector, update: DirOption | DirFlag | DirOneOf | DirAllOf => Option[OrErr[DirOption | DirFlag | DirOneOf | DirAllOf]]): Option[OrErr[DirAllOf]] =
        indexedElements.find(_._2.matches(selector)) match
                case None => None 
                case Some((i, ele)) => 
                    update(ele).map(_.map(
                        updatedEle =>
                            copy(indexedElements = indexedElements.updated(i, (i, updatedEle)))
                    ))

    def replaceElement(selector: LabelSelector, element: DirOption | DirFlag | DirOneOf | DirAllOf): Option[OrErr[DirAllOf]] =
        updateElement(selector, _ => Some(Right(element)))


case class Argument(
    label: String,
    applications: List[Appl[Any]],
    private val genCanInvoke: Argument => Boolean,
    private val genInvoke: (Argument, String, Int) => OrErr[Argument],
    private val genParam: Argument => OrErr[Any],
    override val paramIndex: Int,
) extends RuntimeDirElement:
    val canInvoke: Boolean = genCanInvoke(this)
    val invoke: (String, Int) => OrErr[Argument] =
        (str, int) => genInvoke(this, str, int)
    override val param: OrErr[Any] = genParam(this)


case class CtxOption(
    labels: DoubleLabel,
    applications: List[Appl[Any]],
    private val genInvoke: (CtxOption) => (String, Int) => OrErr[CtxOption],
    private val genUpdate: CtxOption => Any => OrErr[Any],
) extends RuntimeCtxElement with RuntimeOptLike:
    override val matches: LabelSelector => Boolean = _.matches(labels)
    val invoke: (String, Int) => OrErr[CtxOption] = genInvoke(this)
    override val update: Any => OrErr[Any] = genUpdate(this)

case class CtxFlag(
    labels: DoubleLabel,
    application: Option[Int],
    private val genInvoke: CtxFlag => Int => OrErr[CtxFlag],
    private val genUpdate: CtxFlag => Any => OrErr[Any],
) extends RuntimeCtxElement with RuntimeOptLike:
    override val matches: LabelSelector => Boolean = _.matches(labels)
    val invoke: Int => OrErr[CtxFlag] = genInvoke(this)
    override val update: Any => OrErr[Any] = genUpdate(this)

case class CtxOneOf(
    override val matchesAndIsFlag: LabelSelector => Option[Boolean],
    selection: Option[CtxOption | CtxFlag],
    genInvoke: CtxOneOf => (LabelSelector, Option[String], Int) => OrErr[CtxOneOf],
    private val genUpdate: CtxOneOf => Any => OrErr[Any],
) extends RuntimeCtxElement with RuntimeMulti:
    val invokeFlag: (LabelSelector, Int) => OrErr[CtxOneOf] = (sel, int) => genInvoke(this)(sel, None, int)
    val invokeOpt: (LabelSelector, String, Int) => OrErr[CtxOneOf] = (sel, str, int) => genInvoke(this)(sel, Some(str), int)
    override val update: Any => OrErr[Any] = genUpdate(this)
