package nullpointer.json.formats

import nullpointer.json.formats.SeqOfTriesExtensions.SeqOfTriesWithAllSucceeded
import nullpointer.json.testing.{CommonSpec, TryMatchers}
import org.scalamock.scalatest.MockFactory

import scala.util.{Failure, Success}

class SeqOfTriesWithAllSucceededSpec extends CommonSpec with TryMatchers with MockFactory {
  import SeqOfTriesWithAllSucceededSpec._

  describe("A SeqOfTriesWithAllSucceeded") {
    it("must succeed with sequence of elements if all tries succeeded") {
      val firstElement = mock[TestClass]
      val secondElement = mock[TestClass]
      val seqOfTries = Seq(Success(firstElement), Success(secondElement))
      val seqOfTriesWithAllSucceeded = new SeqOfTriesWithAllSucceeded(seqOfTries)
      seqOfTriesWithAllSucceeded.allSucceeded must succeedWith(Seq(firstElement, secondElement))
    }

    it("must fail with failed try exception") {
      val succeededTryElement = mock[TestClass]
      val failedTryException = new IllegalStateException("Something gone wrong")
      val seqOfTries = Seq(Success(succeededTryElement), Failure(failedTryException))
      val seqOfTriesWithAllSucceeded = new SeqOfTriesWithAllSucceeded(seqOfTries)
      val allSucceeded = seqOfTriesWithAllSucceeded.allSucceeded
      allSucceeded must failWith(failedTryException)
    }
  }
}

private object SeqOfTriesWithAllSucceededSpec {
  abstract class TestClass
}
