package net.flatmap.commonmark

/**
  * Created by martin on 07/10/2016.
  */
object Test extends App {
  println(new CommonMark("---").document.run().get)
}
