package click

import click.util.Nat
import click.util.Types.Maybe

import click.util.Types
trait CliElement[T]:
    def description: Option[String]

object CliElement:
    trait DirectCliElement[T] extends CliElement[T]
    trait ContextCliElement[Ctx] extends CliElement[Ctx]
    trait CommandCliElement[T] extends CliElement[T]

    sealed trait IsRequired
    sealed trait Required extends IsRequired
    sealed trait Optional extends IsRequired
    sealed trait HasDefault extends IsRequired

    type Default[T, Req <: IsRequired] <: Maybe[T] = Req match
        case HasDefault => T
        case _ => Unit
