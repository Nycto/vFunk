package test.roundeights.vfunk

import org.specs2.mutable._
import scala.concurrent._
import scala.concurrent.duration._

import com.roundeights.vfunk._

class FormTest extends Specification  {

    /** Blocks while waiting for the given future */
    def await[T] ( future: Future[T] ): T
        = Await.result( future, Duration(1, "second") )

    "A Form" should {

        val form = Form(
            TextField( "one" ),
            TextField(
                "two",
                Filter callback( _ match {
                    case "correct" => "filtered"
                    case _ => "fail"
                } ),
                Validate in( "filtered" )
            ),
            TextField( "three", Filter.numeric, Validate >= 0 )
        )

        val valid = await(form.process(
            "one" -> "unchanged",
            "two" -> "correct",
            "three" -> "123"
        ))

        val invalid = await(form.process(
            "one" -> "unchanged",
            "two" -> "wrong",
            "three" -> "-5"
        ))

        "Preserve the order of its fields" in {
            val fields = form.toList
            fields(0).name must_== "one"
            fields(1).name must_== "two"
            fields(2).name must_== "three"
        }

        "provide access to whether a form is valid" in {
            valid.isValid must_== true
            invalid.isValid must_== false
        }

        "provide access to the filtered values" in {
            valid("one") must_== "unchanged"
            valid("two") must_== "filtered"
            valid("three") must_== "123"

            invalid("one") must_== "unchanged"
            invalid("two") must_== "fail"
            invalid("three") must_== "-5"
        }

        "provide access to the optional filtered values" in {
            valid.get("one") must_== Some("unchanged")
            valid.get("two") must_== Some("filtered")
            valid.get("three") must_== Some("123")
            valid.get("four") must_== None

            invalid.get("one") must_== Some("unchanged")
            invalid.get("two") must_== Some("fail")
            invalid.get("three") must_== Some("-5")
            invalid.get("four") must_== None
        }

        "provide access to the original values" in {
            valid.original("one") must_== Some("unchanged")
            valid.original("two") must_== Some("correct")
            valid.original("three") must_== Some("123")

            invalid.original("one") must_== Some("unchanged")
            invalid.original("two") must_== Some("wrong")
            invalid.original("three") must_== Some("-5")
        }

        "provide access to list of errors produced" in {
            valid.errors must_== Nil

            invalid.errors must_== List(
                Err("GREATERTHANEQUALS", "Must be greater than or equal to 0"),
                Err("OPTION", "Invalid Option")
            )
        }

        "provide access to list of errors produced" in {
            valid.errors must_== Nil

            invalid.errors must_== List(
                Err("GREATERTHANEQUALS", "Must be greater than or equal to 0"),
                Err("OPTION", "Invalid Option")
            )
        }

        "provide access to first error" in {
            valid.firstError must_== None
            invalid.firstError must_== Some(
                Err("GREATERTHANEQUALS", "Must be greater than or equal to 0")
            )
        }

        "provide access to first error message" in {
            valid.firstMessage must_== None
            invalid.firstMessage must_== Some(
                "Must be greater than or equal to 0"
            )
        }

        "provide access to the first invalid field result" in {
            valid.firstInvalid must_== None
            invalid.firstInvalid must_== Some( invalid.results("two") )
        }

        "Provide a map of error messages indexed by field" in {
            valid.fieldMessages must_== Map()
            invalid.fieldMessages must_== Map(
                "two" -> List("Invalid Option"),
                "three" -> List("Must be greater than or equal to 0")
            )
        }

        "Provide a map of error messages indexed by field" in {
            valid.fieldErrors must_== Map()
            invalid.fieldErrors must_== Map(
                "two" -> List( Err("OPTION", "Invalid Option") ),
                "three" -> List(Err(
                    "GREATERTHANEQUALS", "Must be greater than or equal to 0"))
            )
        }

        "Throw an InvalidFormException" in {
            invalid.require must throwA[InvalidFormException]
        }

        "Allow errors to be added to specific fields" in {
            valid.addError("one", Err("ONE", "First")).fieldErrors must_== Map(
                "one" -> List( Err("ONE", "First") )
            )

            invalid.addError("one", Err("ONE", "First"))
                .addError("one", Err("TWO", "Second"))
                .addError("two", Err("THREE", "Third"))
                .fieldErrors must_== Map(
                    "one" -> List(
                        Err("TWO", "Second"),
                        Err("ONE", "First") ),
                    "two" -> List(
                        Err("THREE", "Third"),
                        Err("OPTION", "Invalid Option") ),
                    "three" -> List(
                        Err("GREATERTHANEQUALS",
                            "Must be greater than or equal to 0"))
                )
        }

        "Throw when trying to add an error to a field that isn't defined" in {
            valid.addError("oops", Err("ONE", "First")) must
                throwA[NoSuchElementException]
        }

        "Fail a future if it doesn't validate" in {
            await(form.require(
                "one" -> "unchanged",
                "two" -> "correct",
                "three" -> "123"
            )).isValid must_== true

            await(form.require(
                "one" -> "unchanged",
                "two" -> "wrong",
                "three" -> "-5"
            )) must throwA[InvalidFormException]
        }
    }
}


