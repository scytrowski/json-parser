package nullpointer.json.tokenizer

private trait CharacterProvider {
  def provide(source: String): Option[FoundCharacter]
}
