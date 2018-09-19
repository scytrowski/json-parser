package nullpointer.json.tokenizer

case class FoundCharacter(sourceLeft: String, character: Char, endOfString: Boolean = false)

object FoundCharacter {
  def startOfString(source: String): FoundCharacter = FoundCharacter(source, '\0')

  def endOfString(source: String): FoundCharacter = FoundCharacter(source, '\0', endOfString = true)
}
