

## this creates a special matrix sets the value of the matrix, gets the value of the matrix, sets the value of the inverse, and gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




## calculates inverse of special matrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
	  message("getting cached data")
        return(inv)
    }
    matrx <- x$get()
    inv <- solve(matrx, ...)
    x$setInverse(inv)
    inv
}



