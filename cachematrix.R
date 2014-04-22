## Two functions are created in order to get the inverse of an invertible  matrix.
## Has the inverse already been calculated, it is returned without extra computing.
## If the inverse hasn't been calculated before, the function "solve" is used and the inverse is cached.

## 1st funcion: makeCacheMatrix. It returns a list containing:
## set() defines the matrix
## get() read the matrix
## setinverse() defines the inverse of the matrix
## getinverse() read the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
  inverse<-NULL ##inverse is reset to null
  set<- function(y) {  
    x<<-y ##x is reset to the input (in the parent environment, insted of <- which is a local assignment)
    inverse<<-NULL 
  }
  get<-function() x
  setinverse<-function(y) inverse<<-y ##in parent environment
  getinverse <-function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## 2nd function: CacheSolve. Input: x output of makeCacheMatrix
## It reads the "getinverse" value within the final list in makeCacheMatrix.
## If null, it calculates the inverse, returns it and caches it.
## If not null, it returns the cached data in makeCacheMatrix (no additional computing).

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  # if not null
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #if null
  x$setinverse(solve(x$get(),...))
  x$getinverse()
}
