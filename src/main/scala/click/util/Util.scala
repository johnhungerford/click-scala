package click.util

import click.exception.ClickException.OrErr

object Util:
    type SameTwo[X] = (X, X)

    type IsLabel[N <: Types.Label] = N

    sealed trait Bool
    object Bool:
        type IsBool[B <: Bool] = B
        sealed trait True extends Bool
        sealed trait False extends Bool

    type If[Test <: Bool, TrueCase, FalseCase] = Test match
        case Bool.True => TrueCase
        case Bool.False => FalseCase

    type Or[A <: Bool, B <: Bool] <: Bool = A match
        case Bool.True => Bool.True
        case _ => B match
            case Bool.True => Bool.True
            case _ => Bool.False
    
    type And[A <: Bool, B <: Bool] <: Bool = A match
        case Bool.False => Bool.False
        case _ => B match
            case Bool.True => Bool.True
            case _ => Bool.False


    type Append[T, Tp <: Tuple] <: Tuple = Tp match
        case EmptyTuple => T *: EmptyTuple
        case head *: tail => head *: Append[T, tail]

    object Append:
        inline def apply[T, Tp <: Tuple](value: T, tup: Tp): Append[T, Tp] =
            inline tup match
                case EmptyTuple => Tuple1(value).asInstanceOf[Append[T, Tp]]
                case tup: (head *: tail) =>
                    (tup.head *: apply[T, tail](value, tup.tail)).asInstanceOf[Append[T, Tp]]

    sealed trait TypeExists[T]
    object TypeExists:
        given [T]: TypeExists[T] with {}


    def orErrSequence[A](seq: Seq[OrErr[A]]): OrErr[Seq[A]] =
        seq.foldLeft(Right(List.empty[A]): OrErr[List[A]]) { (currentSeq, nextOrErr) =>
            currentSeq.flatMap(seq => nextOrErr.map(_ :: seq))
        }.map(_.reverse)
