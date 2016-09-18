package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite2 extends FunSuite {

  test("boxBlurKernel check bounds") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(0, 0, 0, 0)

      assert(boxBlurKernel(src, 0, 0, 1) === rgba(0, 0, 0, 0),
        "boxBlurKernel(_,_,0) should be identity.")

      assert(boxBlurKernel(src, 4, 4, 1) === rgba(0, 0, 0, 0),
        "boxBlurKernel(_,_,0) should be identity.")
}


}
