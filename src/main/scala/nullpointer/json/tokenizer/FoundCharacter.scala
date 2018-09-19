package nullpointer.json.tokenizer

private case class FoundCharacter(sourceLeft: String, character: Char, endOfString: Boolean = false)

private object FoundCharacter {
  def startOfString(source: String): FoundCharacter = FoundCharacter(source, '\0')

  def endOfString(source: String): FoundCharacter = FoundCharacter(source, '\0', endOfString = true)
}
