package nullpointer.json

import nullpointer.json.JsonTokens.StringToken

import scala.util.{Failure, Success}

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
    source.headOption.flatMap {
      case '"' => Some(FoundCharacter(source.tail, '"'))
      case '\\' => Some(FoundCharacter(source.tail, '\\'))
      case '/' => Some(FoundCharacter(source.tail, '/'))
      case 'b' => Some(FoundCharacter(source.tail, '\b'))
      case 'f' => Some(FoundCharacter(source.tail, '\f'))
      case 'n' => Some(FoundCharacter(source.tail, '\n'))
      case 'r' => Some(FoundCharacter(source.tail, '\r'))
      case 't' => Some(FoundCharacter(source.tail, '\t'))
      case 'u' =>
        val sourceTail = source.tail
        val codeString = sourceTail.take(4)
        UnicodeParser.parse(codeString) match {
          case Success(unicodeCharacter) =>
            val sourceLeft = sourceTail.drop(4)
            Some(FoundCharacter(sourceLeft, unicodeCharacter))
          case Failure(_) => None
        }
      case _ => None
    }

  private final case class FoundCharacter(sourceLeft: String, character: Char, endOfString: Boolean = false)

  private object FoundCharacter {
    def startOfString(source: String): FoundCharacter = FoundCharacter(source, '\0')

    def endOfString(source: String): FoundCharacter = FoundCharacter(source, '\0', endOfString = true)
  }
}
