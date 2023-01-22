package click

enum InputElement:
  case Argument(value: String)
  case ShortOption(label: Char)
  case ShortOptionGroup(options: List[ShortOption])
  case LongOption(label: String)

object InputElement:
  val ArgumentPattern = """([^\-]+[a-zA-Z0-9\-]*)""".r
  val ShortOptionPattern = """-([a-zA-Z0-9]{1})""".r
  val ShortOptionGroupPattern = """-([a-zA-Z0-9]+)""".r
  val LongOptionPattern = """--([a-zA-Z0-9\-]+)""".r

object ParseArgs:
  def parse(args: List[String]): List[InputElement] =
    import InputElement.*
    args map {
      case ArgumentPattern(arg) => Argument(arg)
      case ShortOptionPattern(label) => ShortOption(label.head)
      case ShortOptionGroupPattern(allLabels) =>
         ShortOptionGroup(allLabels.toCharArray().toList.map(ShortOption.apply))
      case LongOptionPattern(label) => LongOption(label)
    }
