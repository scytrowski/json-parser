package nullpointer.json.tokenizer

import scala.util.{Failure, Success}

private object UnicodeCharacterProvider extends CharacterProvider {
  override def provide(source: String): Option[FoundCharacter] = {
    val codeString = source.take(4)
    UnicodeParser.parse(codeString) match {
      case Success(unicodeCharacter) =>
        val sourceLeft = source.drop(4)
        Some(FoundCharacter(sourceLeft, unicodeCharacter))
      case Failure(_) => None
    }
  }
}
