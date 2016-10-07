package net.flatmap.commonmark

import org.scalatest._
import org.scalatest.prop._

import scala.collection.immutable._
import scala.io.Source

class CommonMarkSpec extends FunSuite with Matchers {
  val commonMarkVersion = "0.26"

  val specExamples = Source
    .fromURL(s"https://raw.githubusercontent.com/jgm/CommonMark/$commonMarkVersion/spec.txt")
    .getLines()
    .foldLeft((Seq.empty[(String,String)],(Option.empty[String],Option
      .empty[String])))({
      case ((samples,(in,out)),line) =>
        line match {
          case "```````````````````````````````` example" =>
            (samples,(Some(""),None))
          case "." if in.isDefined && out.isEmpty =>
            (samples,(in,Some("")))
          case "````````````````````````````````" if in.isDefined && out.isDefined =>
            (samples :+ (in.get.drop(1).replace("→","\t"),out.get.drop(1)),
              (None, None))
          case other if in.isDefined && out.isEmpty =>
            (samples,(in.map(_ + "\n" + other),out))
          case other if in.isDefined && out.isDefined =>
            (samples,(in,out.map(_ + "\n" + other)))
          case other =>
            (samples,(None,None))
        }
    })._1

  specExamples.zipWithIndex.foreach { case ((in,out),i) =>
    test("example" + (i+1)) {
      val p = new CommonMark(in)
      val res = p.document.run()
      println(in)
      assert(res.isSuccess)
      res.foreach { res =>
        Synthesis.html(res) shouldBe out
      }
    }
  }
}