## The two functions given below are used to create a special object 
## that stores a matrix and cache's its inverse.
#
## The first function 'makeCacheMatrix' creates 
## a special “matrix” object which is really a list 
## containing a function to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

#
makeCacheMatrix <- function(x = matrix()) {
 I <- NULL
 set <- function(y) {
 x <<- y
 I <<- NULL
 }
 get <- function() x
 setInverse <- function(solve) I <<- solve
getInverse <- function() I
list(set = set, get = get,
 setInverse = setInverse,
 getInverse = getInverse)
}
#
## The following function calculates the 
## Inverse of the special "matrix" created with the 
## above function. However, it first checks to see 
## if the inverse has already been found. 
## If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it finds 
## the inverse of the matrix and puts the 
## value of the inverse in the cache via the setInverse function.
#
cacheSolve <- function(x, ...) {
 I <- x$getInverse()
 if(!is.null(I)) {
 message("getting cached data")
 return(I)
 }
 data <- x$get()
 I <- solve(data, ...)
 x$setInverse(I)
 I
}
