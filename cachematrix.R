## The makeCacheMatrix contains 4 functions: set, get, setmatrix, getmatrix.
## get is a function that returns the matrix x stored in the main function.
##  et is a function that changes the matrix stored in the main function.
## setmatrix and getmatrix are functions very similar to set and get. 
## They don't calculate the inverse, they simply store the value of the input 
## in a variable m into the main function makeCacheMatrix (setmatrix) and return it (getmatrix).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  
  get <- function () x
  setmatrix <- function(solve) inv<<-solve
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## Input of cacheSolve is the object where makeCacheMatrix is stored. 
## The first thing cacheSolve does is to verify the value inv, 
## stored previously with getmatrix, exists and is not NULL. 
## If it exists in memory, it simply returns a message and the value inv, 
## that is supposed to be the inverse, but not necessarily.

cacheSolve <- function(x, ...) {
  inv<-x$getmatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setmatrix(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}