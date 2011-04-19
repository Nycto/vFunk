package test.scala.vfunk.filter

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.filter._

@RunWith(classOf[JUnitSuiteRunner])
class FilterDataTests extends Specification with JUnit {

    "An EMail filter" should {
        val filter = new EMail
        "Remove invalid email characters from a string" in {
            val data = FilterHelper.build(0 to 255)
            filter.filter(data) must_== "!#$%&'*+-/0123456789" +
                "=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                "^_`abcdefghijklmnopqrstuvwxyz{|}~"
        }
    }
    "A URL filter" should {
        val filter = new URL
        "Remove invalid URL characters from a string" in {
            val data = FilterHelper.build(0 to 255)
            filter.filter(data) must_== "!#$&'()*+,-./0123456789:;=?@" +
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ[]" +
                "_abcdefghijklmnopqrstuvwxyz~"
        }
    }
}

