## Matrix invesion is usally a costly computation
## and their may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.
## The next two functions cache the invers of the matrix
## and calculate it if dosen't exist

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the matrix
## 4- get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # inv will store cache matrix, ini. set NULL
  inv <- NULL
  # set the matrix 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  # get the matrix
  get <- function () x
  # set the inverse
  setinverse <- function(inverse) inv <<- inverse
  # get the inverse
  getinverse <- function() inv
  # return a list that is a matrix with the values of the output functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


##  TcacheSlove computes the inverse of the special "matrix" if the inverse 
##  hasn't been already calculated, else returns the cahed inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Check if inverse is calculated
  # if it's calculated return it
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # if it's not calculate, calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  # cache the inverse
  x$setinverse(inv)
  # return inverse
  inv
}
