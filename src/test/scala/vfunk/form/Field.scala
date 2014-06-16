package test.roundeights.vfunk

import org.specs2.mutable._

import com.roundeights.vfunk._
import com.roundeights.vfunk.validate._
import com.roundeights.vfunk.filter._

class FieldTest extends Specification  {

    "A Field" should {

        val field = TextField(
            "fieldName",
            new Callback( _ match {
                case "correct" => "filtered"
                case _ => "fail"
            } ),
            new In( "filtered" )
        )

        "Validate and filter a passing value" in {
            val result = field.process( "correct" )

            result.field must_== field
            result.name must_== "fieldName"
            result.original must_== "correct"
            result.value must_== "filtered"
            result.isValid must_== true
            result.errors must_== List()
            result.firstError must_== None
        }

        "Validate and filter a failing value" in {
            val result = field.process( "wrong" )

            result.field must_== field
            result.name must_== "fieldName"
            result.original must_== "wrong"
            result.value must_== "fail"
            result.isValid must_== false
            result.errors must_== List( Err("OPTION", "Invalid Option") )
            result.firstError must_== Some( Err("OPTION", "Invalid Option") )
        }

        "Produce an Either" in {
            field.process("correct").either must_== Right("filtered")
            field.process("wrong").either must_== Left(
                Validated( "fail", List(Err("OPTION", "Invalid Option")) )
            )
        }

        "Produce an Option" in {
            field.process("correct").option must_== Some("filtered")
            field.process("wrong").option must_== None
        }

        "Require a value" in {
            field.require("correct").option must_== Some("filtered")
            field.require("wrong") must throwA[InvalidFormException]
        }

        "Produce a future" in {
            field.process("correct").future must ===("filtered").await
            field.process("wrong").future.failed must
                beAnInstanceOf[InvalidFormException].await
        }

        "Allow filters and validators to be added" in {
            val field = TextField("test")
                .andFilter( Filter.callback(_ => "Test Value") )
                .andFilter( Filter.upper )
                .andValidator( Validate.in("TEST VALUE") )
                .andValidator( Validate.manual("TEST", "Test Error") )
                .andValidator( Validate.manual("TEST", "Second Error") )

            val result = field.process("Something")
            result.value must_== "TEST VALUE"
            result.firstMessage must_== Some("Test Error")
        }
    }

}


