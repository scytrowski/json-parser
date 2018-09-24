package nullpointer.json.tokenizer

import nullpointer.json.JsonTokens.StringToken

private object StringTokenProvider extends TokenProvider[StringToken] {
  override def provide(source: String): Option[FoundToken] =
    if (source.startsWith("\""))
      findStringValue(source)
    else
      None

  private def findStringValue(source: String): Option[FoundToken] = {
    val foundCharactersStream = createFoundCharactersStream(source)
    getLastFoundCharacter(foundCharactersStream).map { lastFoundCharacter =>
      val stringValue = createStringValueFromFoundCharacters(foundCharactersStream)
      val sourceLeft = lastFoundCharacter.sourceLeft
      FoundToken(sourceLeft, StringToken(stringValue))
    }
  }

  private def createFoundCharactersStream(source: String): Stream[FoundCharacter] = {
    val initialFoundCharacterOption: Option[FoundCharacter] = Some(FoundCharacter.startOfString(source.tail))
    Stream
      .iterate(initialFoundCharacterOption)(findNextCharacter)
      .tail
      .takeWhile(_.isDefined)
      .map(_.get)
  }

  private def getLastFoundCharacter(foundCharacters: Seq[FoundCharacter]): Option[FoundCharacter] = {
    val lastFoundCharacterOption = foundCharacters.lastOption
    lastFoundCharacterOption.filter(_.endOfString)
  }

  private def createStringValueFromFoundCharacters(foundCharacters: Seq[FoundCharacter]): String =
    foundCharacters
      .dropRight(1)
      .map(_.character)
      .mkString

  private def findNextCharacter(previousFoundCharacterOption: Option[FoundCharacter]): Option[FoundCharacter] =
    for {
      previouslyFoundCharacter <- previousFoundCharacterOption
      foundCharacter <- GeneralCharacterProvider.provide(previouslyFoundCharacter.sourceLeft) if !previouslyFoundCharacter.endOfString
    } yield foundCharacter
}
