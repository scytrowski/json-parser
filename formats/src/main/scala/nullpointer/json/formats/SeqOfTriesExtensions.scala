package nullpointer.json.formats

import scala.util.{Failure, Success, Try}

private object SeqOfTriesExtensions {
  implicit class SeqOfTriesWithAllSucceeded[+T](seq: Seq[Try[T]]) {
    def allSucceeded: Try[Seq[T]] = transformToAllSucceeded(seq)

    private def transformToAllSucceeded[E](seqToTransform: Seq[Try[E]]): Try[Seq[E]] =
      seqToTransform match {
        case Success(element) :: seqTail =>
          transformToAllSucceeded(seqTail).map(element +: _)
        case Failure(exception) :: _ => Failure(exception)
        case Nil => Success(Seq.empty)
      }
  }
}
