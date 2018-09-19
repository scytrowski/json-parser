package nullpointer.json

import nullpointer.json.JsonTokens.StringToken

object StringTokenProvider extends TokenProvider[StringToken] {
  override def provide(source: String): Option[FoundToken] =
    if (source.startsWith("\"")) {
      val initialFoundCharacterOption: Option[FoundCharacter] = Some(FoundCharacter.startOfString(source.tail))
      val foundCharacters = Stream
        .iterate(initialFoundCharacterOption) {
          case Some(FoundCharacter(sourceLeft, _, endOfString)) =>
            if (!endOfString)
              findStringCharacter(sourceLeft)
            else
              None
        }
        .tail
        .takeWhile(_.isDefined)
        .map(_.get)
      val lastFoundCharacterOption = foundCharacters.lastOption
      if (lastFoundCharacterOption.forall(_.endOfString)) {
        val stringValue = foundCharacters
          .map(_.character)
          .dropRight(1)
          .foldLeft("")((s, c) => s"$s$c")
        val sourceLeft = lastFoundCharacterOption.get.sourceLeft
        Some(FoundToken(sourceLeft, StringToken(stringValue)))
      } else
        None
    } else
      None

  private def findStringCharacter(source: String): Option[FoundCharacter] =
    source.headOption.flatMap {
      case '"' => Some(FoundCharacter(source.tail, '\0', endOfString = true))
      case '\\' => findEscapedCharacter(source.tail)
      case stringChar: Char => Some(FoundCharacter(source.tail, stringChar))
    }

  private def findEscapedCharacter(source: String): Option[FoundCharacter] =
    source.headOption.flatMap { sourceHead =>
      if (sourceHead == '"')
        Some(FoundCharacter(source.tail, '"'))
      else if (sourceHead == '\\')
        Some(FoundCharacter(source.tail, '\\'))
      else if (sourceHead == '/')
        Some(FoundCharacter(source.tail, '/'))
      else if (sourceHead == 'b')
        Some(FoundCharacter(source.tail, '\b'))
      else if (sourceHead == 'f')
        Some(FoundCharacter(source.tail, '\f'))
      else if (sourceHead == 'n')
        Some(FoundCharacter(source.tail, '\n'))
      else if (sourceHead == 'r')
        Some(FoundCharacter(source.tail, '\r'))
      else if (sourceHead == 't')
        Some(FoundCharacter(source.tail, '\t'))
      else
        None
    }

  private final case class FoundCharacter(sourceLeft: String, character: Char, endOfString: Boolean = false)

  private object FoundCharacter {
    def startOfString(source: String): FoundCharacter = FoundCharacter(source, '\0')

    def endOfString(source: String): FoundCharacter = FoundCharacter(source, '\0', endOfString = true)
  }
}
