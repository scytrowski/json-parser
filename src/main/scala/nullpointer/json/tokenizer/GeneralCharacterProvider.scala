package nullpointer.json.tokenizer

object GeneralCharacterProvider extends CharacterProvider {
  override def provide(source: String): Option[FoundCharacter] =
    source.headOption.flatMap {
      case '"' => Some(FoundCharacter.endOfString(source.tail))
      case '\\' => EscapedCharacterProvider.provide(source.tail)
      case c: Char => Some(FoundCharacter(source.tail, c))
    }
}
