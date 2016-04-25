
## the following two functions calculate/report the inverse of a matrix in an efficient 
# manner. when the matrix x has been inverted previously and x has not changed since 
# last time was called, then stored values of its inverse are reported. See example 
# on how to use these functions at the very end of this script.




## makeCacheMatrix
  ## this is virtually the same as in the example: seting and getting
  ## the matrix we want to invert, and then setting and getting the 
  ## value of the inverser we want to calculate based on x. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}





## cacheSolve

  ## cacheSolve returns a matrix that is the inverse of 'x'
  ## first check if a previous inverse has been stored AND
  ## that the stored inverse is for an identical matrix as 'x'
  ## if so returns the cached inverser
  ## if not (if not, makeCache not been run or x has changed) then calculates
  ## the inverse of x from scratch and reports it

cacheSolve <- function(list, x, ...) {
  
  m <- list$getInverse()
  
  if( !is.null(m) & identical(x, list$get()) ) {
    
    message("getting cached data, x has not changed the same")
    return(m)
  }
  
  message("x has changed since you run makeCacheMatrix(x)")

  data <- x    # old version: data <- list$get()

  m <- solve(data, ...)
  list$setInverse(m)
  
  m
}






# Example:
# > c=rbind(c(1, -1/4), c(7, 3))
# > list <- makeCacheMatrix(c)
# > cacheSolve(list, c)
# getting cached data, x has not changed the same
# [,1]       [,2]
# [1,]  0.6315789 0.05263158
# [2,] -1.4736842 0.21052632
# >  c=rbind(c(1, -1/4), c(7, 4))
# >  cacheSolve(list, c)
# x has changed since you run makeCacheMatrix(x)
# [,1]       [,2]
# [1,]  0.6956522 0.04347826
# [2,] -1.2173913 0.17391304



