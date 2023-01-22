package click.exception

sealed trait ClickException

object ClickException:
  final case class InvalidOption(msg: String) extends ClickException
  final case class InvalidArgument(msg: String) extends ClickException
  final case class ParseError(msg: String) extends ClickException
  final case class InternalStateError(msg: String) extends ClickException

  type OrBadOpt[T] = Either[InvalidOption, T]
  type OrBadArg[T] = Either[InvalidArgument, T]
  type OrParseErr[T] = Either[ParseError, T]
  type OrErr[T] = Either[ClickException, T]
