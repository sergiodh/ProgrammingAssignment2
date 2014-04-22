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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          s <- x$getsolve()
          if(!is.null(s)) {
                message("getting cached data")
                return(s)
          }
          data <- x$get()
          s <- solve(data, ...)
          x$setsolve(s)
          s
}
