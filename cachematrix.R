#The first function, makeCacheMatrix creates a special "matrix",
#which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inversion
#4. get the value of the inversion

#The second function calculates the inversion of the special "matrix" created with the above function.
#However, it first checks to see if the inversion has already been calculated. If so, it gets the inversion from the cache and skips the computation.
#Otherwise, it calculates the inversion of the data and sets the value of the inversion in the cache via the setinversion function.

#creates a special "matrix" and implement the functions
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#cache the inversion if it has calculated and the matrix hasn't been modify, otherwise, use solve() to calculate it
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data");
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
