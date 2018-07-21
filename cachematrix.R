## These functions together catch in inverse of a matrix. When the inverse of the matrix has previously been 
# calculated, makeCacheMatrix stores the inverse so that it can later be retrieved. 

## makeCacheMatrix creates a special matrix that can cache the inverse of the matrix x. The set function allows 
## you to set a new matrix. The get function just reports the matrix x. The getinverse shows if the inverse has
## previously been computed (useful for the followup functions) and the setinverse defines the setter for the 
## inverse matrix. The list function makes it possible to call the names of the functions in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inverse <<- solve
     getinverse <- function() inverse
     list(set = set, get = get, 
          setinverse = setinverse,
          getinverse = getinverse)

}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above (inverse <- solve(data, ...) does this) 
## If the inverse has already been calculated (aka if !is.null is TRUE), 
## then cacheSolve should retrieve the inverse from the cache with the attached message. 
## It then lets the previous function know that there is a new inverse set (x$setinverse (inverse)), 
## so that next time the function is called x$getinverse() gives a positive response and not NULL. 

cacheSolve <- function(x, ...) {
     inverse <- x$getinverse()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data, ...)
     x$setinverse(inverse)
     inverse
}