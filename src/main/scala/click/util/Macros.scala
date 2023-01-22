package click.util

object Macros:
    import scala.quoted.* // imports Quotes, Expr

    def inspectCodeMacro[T](x: Expr[T])(using Quotes): Expr[T] =
        println(x.show)
        x

    inline def inspectCode[T](inline x: T): T = ${inspectCodeMacro('x)}
