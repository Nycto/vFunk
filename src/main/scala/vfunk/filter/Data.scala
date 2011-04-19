/**
 * Specific Data type filters
 */

package main.scala.vfunk.filter

import scala.collection.immutable.HashSet

/**
 * Filters out invalid email address characters
 */
class EMail extends Characters (
    HashSet[Char]() ++ ('a' to 'z') ++ ('A' to 'Z') ++
        ('0' to '9') ++ ("!#$%&'*+-/=?^_`{|}~@".toList)
) {}

/**
 * Filters out invalid URL characters
 */
class URL extends Characters (
    HashSet[Char]() ++ ('a' to 'z') ++ ('A' to 'Z') ++
        ('0' to '9') ++ ("-._~:/?#[]@!$&'()*+,;=".toList)
) {}

/**
 * Filters out invalid IPv4 characters
 */
class IPv4 extends Characters (
    HashSet[Char]() ++ ('0' to '9') ++ List('.')
) {}

/**
 * Filters out invalid IPv6 characters
 */
class IPv6 extends Characters (
    HashSet[Char]() ++ ('0' to '9') ++ List('.', ':')
        ++ ('a' to 'f') ++ ('A' to 'F')
) {}

