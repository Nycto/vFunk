/**
 * Specific Data type filters
 */

package com.roundeights.vfunk.filter

import com.roundeights.vfunk.Filter

/**
 * Filters out invalid email address characters
 */
class EMail extends Characters (
    Set[Char]() ++ ('a' to 'z') ++ ('A' to 'Z')
        ++ ('0' to '9') ++ ("!#$%&'*+-/=?^_`{|}~@".toList)
) {

    /** {@inheritDoc} */
    override def toString = "Filters(EMail)"
}

/**
 * Filters out invalid URL characters
 */
class URL extends Characters (
    Set[Char]()
        ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
        ++ ("-._~:/?#[]@!$&'()*+,;=".toList)
) {

    /** {@inheritDoc} */
    override def toString = "Filters(URL)"
}

/**
 * Filters out invalid IPv4 characters
 */
class IPv4 extends Characters (
    Set[Char]() ++ ('0' to '9') ++ List('.')
) {

    /** {@inheritDoc} */
    override def toString = "Filters(IPv4)"
}

/**
 * Filters out invalid IPv6 characters
 */
class IPv6 extends Characters (
    Set[Char]()
        ++ ('0' to '9') ++ List('.', ':')
        ++ ('a' to 'f') ++ ('A' to 'F')
){

    /** {@inheritDoc} */
    override def toString = "Filters(IPv6)"
}

