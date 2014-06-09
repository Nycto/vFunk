package test.roundeights.vfunk

import org.specs2.mutable._
import scala.concurrent._
import scala.concurrent.duration._

import com.roundeights.vfunk._
import com.roundeights.vfunk.validate._
import com.roundeights.vfunk.filter._

class FieldTest extends Specification  {

    /** Blocks while waiting for the given future */
    def await[T] ( future: Future[T] ): T
        = Await.result( future, Duration(1, "second") )

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
            val result = await( field.process( "correct" ) )

            result.field must_== field
            result.name must_== "fieldName"
            result.original must_== "correct"
            result.value must_== "filtered"
            result.isValid must_== true
            result.errors must_== List()
            result.firstError must_== None
        }

        "Validate and filter a failing value" in {
            val result = await( field.process( "wrong" ) )

            result.field must_== field
            result.name must_== "fieldName"
            result.original must_== "wrong"
            result.value must_== "fail"
            result.isValid must_== false
            result.errors must_== List( Err("OPTION", "Invalid Option") )
            result.firstError must_== Some( Err("OPTION", "Invalid Option") )
        }

        "Produce an Either" in {
            await(field.process("correct")).either must_== Right("filtered")
            await(field.process("wrong")).either must_== Left(
                Validated( "fail", List(Err("OPTION", "Invalid Option")) )
            )
        }

        "Produce an Option" in {
            await(field.process("correct")).option must_== Some("filtered")
            await(field.process("wrong")).option must_== None
        }

        "Require a value" in {
            await(field.require("correct")).value must_== "filtered"
            await(field.require("wrong")) must throwA[InvalidFormException]
        }

        "Produce a future" in {
            await(field.value("correct")) must_== "filtered"
            await(field.value("wrong")) must throwA[InvalidFormException]
        }
    }

}


