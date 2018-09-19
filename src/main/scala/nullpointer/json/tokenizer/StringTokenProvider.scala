package nullpointer.json.tokenizer

import nullpointer.json.tokenizer.JsonTokens.StringToken

object StringTokenProvider extends TokenProvider[StringToken] {
  override def provide(source: String): Option[FoundToken] =
    if (source.startsWith("\"")) {
      val initialFoundCharacterOption: Option[FoundCharacter] = Some(FoundCharacter.startOfString(source.tail))
      val foundCharacters = Stream
        .iterate(initialFoundCharacterOption) {
          case Some(FoundCharacter(sourceLeft, _, endOfString)) =>
            if (!endOfString)
              GeneralCharacterProvider.provide(sourceLeft)
            else
              None
          case None => None
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
}
