package click.util

sealed trait Nat
object Nat:
    sealed trait _0 extends Nat
    sealed trait Succ[N <: Nat] extends Nat

    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    type _4 = Succ[_3]
    type _5 = Succ[_4]
    type _6 = Succ[_5]
    type _7 = Succ[_6]
    type _8 = Succ[_7]
    type _9 = Succ[_8]
    type _10 = Succ[_9]
    type _11 = Succ[_10]
    type _12 = Succ[_11]
    type _13 = Succ[_12]
    type _14 = Succ[_13]
    type _15 = Succ[_14]
    type _16 = Succ[_15]
    type _17 = Succ[_16]
    type _18 = Succ[_17]
    type _19 = Succ[_18]
    type _20 = Succ[_19]
    type _21 = Succ[_20]
    type _22 = Succ[_21]

    type Of[I <: Int & Singleton] <: Nat = I match
        case 0 => _0
        case 1 => _1
        case 2 => _2
        case 3 => _3
        case 4 => _4
        case 5 => _5
        case 6 => _6
        case 7 => _7
        case 8 => _8
        case 9 => _9
        case 10 => _10
        case 11 => _11
        case 12 => _12
        case 13 => _13
        case 14 => _14
        case 15 => _15
        case 16 => _16
        case 17 => _17
        case 18 => _18
        case 19 => _19
        case 20 => _20
        case 21 => _21
        case 22 => _22

    type ToInt[N <: Nat] <: Int & Singleton = N match
        case _0 => 0
        case _1 => 1
        case _2 => 2
        case _3 => 3
        case _4 => 4
        case _5 => 5
        case _6 => 6
        case _7 => 7
        case _8 => 8
        case _9 => 9
        case _10 => 10
        case _11 => 11
        case _12 => 12
        case _13 => 13
        case _14 => 14
        case _15 => 15
        case _16 => 16
        case _17 => 17
        case _18 => 18
        case _19 => 19
        case _20 => 20
        case _21 => 21
        case _22 => 22

    infix type +[A <: Nat, B <: Nat] = A match
        case _0 => B
        case Succ[a] => Succ[a + B]
    
    infix type -[A <: Nat, B <: Nat] = B match
        case _0 => A
        case Succ[b] => A match
            case Succ[a] => a - b
    
    infix type *[A <: Nat, B <: Nat] = A match
        case _0 => _0
        case _1 => B
        case Succ[a] => B + (a * B)
