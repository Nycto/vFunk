package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.filter._

@RunWith(classOf[JUnitSuiteRunner])
class PlumbingTests extends Specification with JUnit {

   "An Identity filter" should {
        "Return the value it is given" in {
            val filter = new Identity
            filter.filter("data") must_== "data"
        }
    }
    "A Callback filter" should {
        "Apply the given lambda and return the results" in {
            val filter = new Callback("input: " + _)
            filter.filter("data") must_== "input: data"
        }
    }
}

