package nullpointer.json

trait CharacterProvider {
  def provide(source: String): Option[FoundCharacter]
}
