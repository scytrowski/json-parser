package nullpointer.json

object EscapedCharacterProvider extends CharacterProvider {
  override def provide(source: String): Option[FoundCharacter] =
    source.headOption.flatMap {
      case '"' => Some(FoundCharacter(source.tail, '"'))
      case '\\' => Some(FoundCharacter(source.tail, '\\'))
      case '/' => Some(FoundCharacter(source.tail, '/'))
      case 'b' => Some(FoundCharacter(source.tail, '\b'))
      case 'f' => Some(FoundCharacter(source.tail, '\f'))
      case 'n' => Some(FoundCharacter(source.tail, '\n'))
      case 'r' => Some(FoundCharacter(source.tail, '\r'))
      case 't' => Some(FoundCharacter(source.tail, '\t'))
      case 'u' => UnicodeCharacterProvider.provide(source.tail)
      case _ => None
    }
}
