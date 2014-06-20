## The two functions take a matrix and calculate its inverse using the solve() function. In case the matris hasn't
## been changed, the cacheSolve() function takes the previously cached solution instead of calculating the inverse
## again.

## The first function creates a list of functions to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function() inv <<- solve(x)
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function using the list created by the former either returns the cached result or calculates the inverse
## in case the matrix has been changed and inv is therefor NULL.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv()
        inv
}
