package com.learning_scala.ninetynine_problems.lists

class Functions 
{
  def last[A]( list: List[A] ) :A = 
  {
    list match 
    {   
      case ( h :: Nil ) => h
      case ( _ :: tail ) => last(tail)
      case ( _ ) => throw new NoSuchElementException
    }   
  }

  def penultimate[A]( list: List[A] ) :A = 
  {
    list match 
    {   
      case h :: Nil => throw new NoSuchElementException
      case h :: t =>  
        if( t.length == 1 ) 
          h   
        else 
          penultimate( t ) 
      case _ => throw new NoSuchElementException
    }
  }
  
  def penultimateRec[A]( ls: List[A] ) :A = 
  {
    ls match 
    {
      case h :: _ :: Nil => h
      case _ :: tail => penultimateRec(tail)
      case _ => throw new NoSuchElementException
    }
  }
  
  def nth[A]( pos: Int, list: List[A] ) :A = 
  {
    (pos, list) match 
    {
      case( 0, h :: _ ) => h
      case( pos, h :: t ) => nth( pos - 1, t )
      case( _, Nil ) => throw new NoSuchElementException
    }
  }
  
  def nthBuiltIn[A]( pos: Int, list: List[A] ) =
  {
    if( pos >= 0 ) list(pos)
    else throw new NoSuchElementException
  }
  
  def length[A]( list: List[A] ) :Int = 
  {
    list match 
    {
      case Nil => 0
      case head :: tail => length( tail ) + 1
    }
  }
  
  def reverse[A]( list: List[A] ) :List[A] = 
  {
    list match
    {
      case h :: Nil => List( h )
      case h :: tail => reverse( tail ) ::: List( h )
    }
  }
  
  def isPalindrome[A]( list: List[A] ) :Boolean =
    list == reverse(list)
  
  def flatten( list: List[_]) :List[_] = 
  {
    list flatMap 
    {
      case head :: tail => head :: flatten( tail )
      case Nil => Nil
      case x => List(x)
    }
  }
  
  def flattenMap( ls: List[Any] ) :List[Any] = 
  {
    ls flatMap 
    {
      case ms: List[_] => flatten( ms )
      case e => List(e)
    }
  }
      
  def compressRecursive[A]( list: List[A] ) :List[A] = 
  {
    list match 
    {
      case Nil => Nil
      case h :: tail => h :: compressRecursive( tail.dropWhile( i => i == h ) )
    }
  }
  
  def compressFunctional[A]( ls: List[A] ) :List[A] = 
  {
    ls.foldRight( List[A]() ) 
    {
      ( h, r ) =>
        if ( r.isEmpty || r.head != h ) h :: r
        else r          
    }
  }

}