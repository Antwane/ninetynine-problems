/**
 *
 */
package com.learning_scala.ninetynine_problems.lists

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._


/**
 * @author antonio
 *
 */
@RunWith(classOf[JUnitRunner])
class FunctionsSpec extends Specification 
{
  "Functions" should 
  {
    val functions = new Functions()
    "find last element in a list" in
    {   
      val numbers = List( 1, 2, 3, 4, 5 ) 
      val last = functions.last( numbers )
      last must be equalTo(5)
    }   
    "throws NoSuchElementException when providing empty list of Int" in  
    {   
      val numbers = List[Int]() 
      functions.last(numbers) must throwA[NoSuchElementException]
    }   
    "find penultimate element in a list" in
    {   
      val numbers = List[Int]( 1, 2, 3, 4, 5 ) 
      val penultimate = functions.penultimate(numbers)
      penultimate must be equalTo(4)
    }
    "throws NoSuchElementException when providing a list with one element" in
    {   
      val numbers = List[Int]( 5 ) 
      functions.penultimate(numbers) must throwA[NoSuchElementException]
    }
    "find penultimate element in a list" in
    {   
      val numbers = List[Int]( 1, 2, 3, 4, 5 ) 
      val penultimate = functions.penultimateRec( numbers )
      penultimate must be equalTo(4)
    }
    "find the nth element in a list" in
    {   
      val pos = 0
      val numbers = List[Int]( 1 ) 
      val penultimate = functions.nth( pos, numbers )
      penultimate must be equalTo(1)
    }
    "find the length of a list" in
    {
      val numbers = List[Int]( 1, 2, 3, 4, 5 ) 
      val length = functions.length( numbers )
      length must be equalTo(5)
    }
    "reverse a List" in
    {
      val numbers = List[Int]( 1, 2, 3, 4, 5 )
      val reversed = List[Int]( 5, 4, 3, 2, 1 )
      val actual = functions.reverse( numbers )
      actual must be equalTo(reversed)
    }
    "A list is palindrome" in
    {
      //It can be read in both ways
      val numbers = List[Int]( 1, 2, 3, 2, 1 )
      val reversed = functions.reverse( numbers )
      numbers must be equalTo(reversed)
    }
    "flatten should flat a list" in
    {
      //It can be read in both ways
      val numbers = List( "a", List("b1", "b2", "b3"), List( "c1", List( "c21", Nil, "c22" ), Nil, "d" ) )
      val expected = List( "a", "b1", "b2", "b3", "c1", "c21", "c22", "d" )
      val actual = functions.flatten( numbers )
      actual must be equalTo(expected)
    }
    "flattenMap should flat a list" in
    {
      //It can be read in both ways
      val numbers = List( "a", List("b1", "b2", "b3"), List( "c1", List( "c21", Nil, "c22" ), Nil, "d" ) )
      val expected = List( "a", "b1", "b2", "b3", "c1", "c21", "c22", "d" )
      val actual = functions.flattenMap( numbers )
      actual must be equalTo(expected)
    }
    "compress should take off duplicates from a list" in
    {
      val numbers = List( 'a, 'a, 'a, 'a, 'b, 'c, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e )
      val expected = List( 'a, 'b, 'c, 'a, 'd, 'e )
      val actual = functions.compressRecursive( numbers )
      actual must be equalTo(expected)
    }
  }    
}