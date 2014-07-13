package com.roundeights.vfunk

import scala.collection.immutable.ListMap
import scala.concurrent.{Future, ExecutionContext}


/**
 * A companion for the Form class
 */
object Form {

    /** Creates a form from a list of fields */
    private[vfunk] def toMap[F <: CommonField] (
        fields: Traversable[F]
    ): ListMap[String, F] = {
        fields.foldLeft ( ListMap[String, F]() ) {
            (accum, field) => accum + ((field.name, field))
        }
    }

    /** Creates a form from a list of fields */
    def apply ( fields: Traversable[Field] ): Form = new Form( fields )

    /** Creates a form from a list of fields */
    def apply ( fields: Field* ): Form = new Form( fields )
}


/**
 * Shared form methods
 */
abstract class CommonForm[S <: CommonForm[_, F], F <: CommonField] (
    val fields: ListMap[String, F]
) extends Traversable[F] {

    /** Creates a new form including a new field */
    def add( field: F ): S

    /** Creates a new form including a new field */
    def + ( field: F ): S = add(field)

    /** {@inheritDoc} */
    override def foreach[U] ( callback: F => U ): Unit
        = fields.foreach( pair => callback( pair._2 ) )

    /** Returns this form as an async form */
    def async: AsyncForm
}


/**
 * A form
 */
class Form (
    fields: ListMap[String, Field]
) extends CommonForm[Form, Field](fields) {

    /** Creates a form from a list of fields */
    def this ( fields: Traversable[Field] ) = this( Form.toMap(fields) )

    /** Creates a form from a list of fields */
    def this ( fields: Field* ) = this( fields )

    /** {@inheritDoc} */
    override def add( field: Field ): Form
        = new Form( fields + ((field.name, field)) )

    /** Adds an async field and returns an async form */
    def add( field: AsyncField ): AsyncForm = async.add(field)

    /** Creates a new form including a new field */
    def + ( field: AsyncField ): AsyncForm = add(field)

    /** Validates a map against this form */
    def process ( values: Map[String,String] ): FormResults = {
        fields.foldLeft( FormResults() ) {
            (accum, pair) => accum + ((
                pair._1,
                pair._2.process( values.getOrElse( pair._1, "" ) )
            ))
        }
    }

    /** Validates a list of tuples */
    def process ( values: (String, String)* ): FormResults
        = process( Map( values:_* ) )

    /** Validates a map against this form and fails if it doesn't validate */
    def require ( values: Map[String, String] ): FormResults
        = process( values ).require

    /** Validates a map against this form and fails if it doesn't validate */
    def require ( values: (String, String)* ): FormResults
        = require( Map(values:_*) )

    /** {@inheritDoc} */
    override def async: AsyncForm
        = new AsyncForm( fields.map( pair => (pair._1 -> pair._2.async) ) )
}


/** @see AsyncForm */
object AsyncForm {

    /** Creates a form from a list of fields */
    def apply ( fields: Traversable[CommonField] ): AsyncForm
        = new AsyncForm(fields)

    /** Creates a form from a list of fields */
    def apply ( fields: CommonField* ): AsyncForm
        = new AsyncForm( fields )
}

/**
 * A form
 */
class AsyncForm (
    fields: ListMap[String, AsyncField]
) extends CommonForm[AsyncForm, AsyncField](fields) {

    /** Creates a form from a list of fields */
    def this ( fields: Traversable[CommonField] )
        = this( Form.toMap( fields.map(_.async) ) )

    /** Creates a form from a list of fields */
    def this ( fields: CommonField* ) = this( fields )

    /** {@inheritDoc} */
    override def add( field: AsyncField ): AsyncForm
        = new AsyncForm( fields + ((field.name, field)) )

    /** Validates a map against this form */
    def process
        ( values: Map[String, String] )
        ( implicit ctx: ExecutionContext )
    : Future[FormResults] = {

        // Kick off requests to validate all the fields
        val futures: Iterable[Future[(String, FieldResult)]] =
            fields.map(pair => {
                pair._2.process( values.getOrElse(pair._1, "") )
                    .map( pair._1 -> _ )
            })

        Future.fold(futures)(FormResults())(_ + _)
    }

    /** Validates a list of tuples */
    def process
        ( values: (String, String)* )
        ( implicit ctx: ExecutionContext )
    : Future[FormResults]
        = process( Map( values:_* ) )

    /** Validates a map against this form and fails if it doesn't validate */
    def require
        ( values: Map[String, String] )
        ( implicit ctx: ExecutionContext )
    : Future[FormResults]
        = process( values ).map( _.require )

    /** Validates a map against this form and fails if it doesn't validate */
    def require
        ( values: (String, String)* )
        ( implicit ctx: ExecutionContext )
    : Future[FormResults]
        = require( Map(values:_*) )

    /** {@inheritDoc} */
    override def async: AsyncForm = this
}

/**
 * Thrown when a required validation does not pass
 */
case class InvalidFormException (
    val formResults: FormResults
) extends ValidationException {

    /** Alternate constructor from a list of results */
    def this ( fields: (String, FieldResult)* )
        = this( new FormResults( fields: _* ) )

    /** {@inheritDoc} */
    override def errors: Seq[Err] = formResults.errors

    /** {@inheritDoc} */
    override def toString = "InvalidFormException(%s)".format(formResults)
}

/**
 * The results of a validation run
 */
case class FormResults (
    val results: ListMap[String,FieldResult] = ListMap()
) extends Traversable[FieldResult] with Errable {

    /** Constructs from a list of field tuples */
    def this ( fields: (String, FieldResult)* ) = this( ListMap(fields:_*) )

    /** {@inheritDoc} */
    def foreach[U] ( callback: FieldResult => U ): Unit
        = results.foreach( value => callback( value._2 ) )

    /** Adds a new result to thie map */
    def + ( newElem: (String, FieldResult) ): FormResults
        = FormResults( results + newElem )

    /** {@inheritDoc} */
    override def isValid: Boolean
        = results.forall( result => result._2.isValid )

    /** Returns the value of a field */
    def apply ( field: String ): String = results( field ).value

    /** Returns the value of a field as an option */
    def get ( field: String ): Option[String]
        = results.get( field ).map( _.value )

    /** Returns the original value from a form */
    def original ( field: String ): Option[String]
        = results.get( field ).map( _.original )

    /** Adds an error to this result set */
    def addError ( field: String, err: Err ): FormResults = {
        val updated = err +: results( field )
        FormResults( results.foldLeft( ListMap[String,FieldResult]() )(
            (accum, pair) => pair match {
                case (name, _) if field == name => accum + (name -> updated)
                case pair => accum + pair
            }
        ))
    }

    /** Adds an error to this result set */
    def addError ( field: String, code: String, message: String ): FormResults
        = addError( field, Err(code, message) )

    /** Returns the results of the first invalid field */
    def firstInvalid: Option[FieldResult]
        = results.find( ! _._2.isValid ).map( _._2 )

    /** {@inheritDoc} */
    override def errors: Seq[Err] = results.foldLeft( List[Err]() ) {
        (accum, pair) => pair._2.errors.toList ::: accum
    }

    /** Returns a map of field names to error messages */
    def fieldErrors: Map[String,Seq[Err]] = {
        results.foldLeft( Map[String,Seq[Err]]() ) { (accum, pair) =>
            if ( pair._2.isValid )
                accum
            else
                accum + ( pair._1 -> pair._2.errors )
        }
    }

    /** Returns a map of field names to error messages */
    def fieldMessages: Map[String,Seq[String]]
        = fieldErrors.mapValues( _.view.map(_.message) )

    /** {@inheritDoc} */
    override def require: this.type = {
        if ( !isValid )
            throw InvalidFormException( this )
        this
    }
}


