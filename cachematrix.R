## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a vector containing four functions for setting and getting matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

  m<- NULL
  set<-function(y){     ## set values of the matrix to cache
    x<<-y
    m<<-NULL
  }
  
  get<-function() x  ##retrives matrix data
  setinverse<- function(Inverse) m<<-Inverse   ##  Set values of inverse to cache
  getinverse<-function() m  ## retrieve inverse value
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function

## Function to retriev inverse of a matrix if already calculated or if not, then calculate inverse and set its value into cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<- x$getinverse()      
  if (!is.null(m)){             ## inverse already calculated
    message("getting cached data")
    return (m)
  }
  
  data<- x$get()      ## inverse not calculated
  m<-solve(data,...)
  x$setinverse(m)
  m
}
