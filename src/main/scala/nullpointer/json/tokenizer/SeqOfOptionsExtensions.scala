package nullpointer.json.tokenizer

object SeqOfOptionsExtensions {
  implicit class SeqOfOptionsWithAllDefined[+T](seq: Seq[Option[T]]) {
    def allDefined: Option[Seq[T]] = transformToAllDefined(seq)

    private def transformToAllDefined[E](seqToTransform: Seq[Option[E]]): Option[Seq[E]] =
      seqToTransform.headOption match {
        case Some(headElementOption) =>
          for {
            headElement <- headElementOption
            tailAllDefined <- transformToAllDefined(seqToTransform.tail)
          } yield headElement +: tailAllDefined
        case None => Some(Seq.empty)
      }
  }
}
