package net.flatmap.commonmark

import org.scalatest._
import org.scalatest.prop._

import scala.collection.immutable._
import scala.io.Source

class CommonMarkSpec extends FunSuite with Matchers with SpecLoader {
  val commonMarkVersion = "0.26"

  specExamples.zipWithIndex.foreach { case ((in,out),i) =>
    test("example" + (i+1)) {
      val p = new CommonMark(in)
      val res = p.document.run()
      assert(res.isSuccess)
      res.foreach { res =>
        Synthesis.html(res) shouldBe out
      }
    }
  }
}