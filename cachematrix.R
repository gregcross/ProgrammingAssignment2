## These two functions alow you to cache the inverse of a matrix so youdon't have to recompute it

## makeCacheMatrix creates a specialmatrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the current inverse has already been calculated, then cacheSolve should retrieves inverse from the cache.

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached matrix")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
}
