setwd("C:/Users/user/Desktop/OLC/Coursera/R Programming/")

##Caching the inverse of a mtrix reduces computation time and effort avoiding the need to repeat the process again and again.


## This function creates a special "matrix" object that can cache its inverse.

## x is the input matirx

makeCacheMatrix<- function(x=matrix()){
  inv<-NULL
  set<- function(y){
    x<<-y
    inv<<- NULL
  }
  get<- function(){x}
  setInverse<- function(inverse) {inv<<- inverse}
  getInverse<- function(){inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
### This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## then cacheSolve returns the inverse from the cache.

## Notes:
##   1. Assumes that the matrix is invertible
##   2. Once inverse is cached, its not invalidated on further changes
##      to matrix

cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}
