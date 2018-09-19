package nullpointer.json.tokenizer

trait CharacterProvider {
  def provide(source: String): Option[FoundCharacter]
}
