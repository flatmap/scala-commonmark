package net.flatmap.commonmark

import scala.collection.immutable.Seq
import scala.io.Source

/**
  * Created by martin on 10/10/2016.
  */
trait SpecLoader {
  def commonMarkVersion: String
  def specExamples = /*{
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.open( "GET", theUrl, false ); // false for synchronous request
    xmlHttp.send( null );
    return xmlHttp.responseText;
  }*/
    Source
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
            (samples :+ (in.get.drop(1).replace("→","\t"),out.get.drop(1).replace("→","\t")),
              (None, None))
          case other if in.isDefined && out.isEmpty =>
            (samples,(in.map(_ + "\n" + other),out))
          case other if in.isDefined && out.isDefined =>
            (samples,(in,out.map(_ + "\n" + other)))
          case other =>
            (samples,(None,None))
        }
    })._1

}
