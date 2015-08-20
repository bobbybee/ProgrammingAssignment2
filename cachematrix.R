# This file provides a pair of functions to perform matrix inversion,
# taking advantage of the memoization technique
# makeCacheMatrix will create a light container object for a matrix,
# using getters and setters to store the matrix along with its inverse in the object
# cacheSolve will actually perform the matrix inversion,
# but only if needed

# prepares a matrix to be inverted,
# with the result of the inversion cached

makeCacheMatrix <- function(x = matrix()) {
    # the result to be computer
    inverted <- NULL
    
    # when a new matrix is given,
    # set that,
    # but also clear the cached result

    set <- function(newMatrix) {
        x <<- newMatrix
        inverted <<- NULL
    }

    # get the matrix
    get <- function() x

    # setters/getters for the inverted result

    setResult <- function(r) {
        inverted <<- r
    }

    getResult <- function() inverted

    # return a list of the methods
    # do I get points for lispyness? :P

    list(set = set, 
         get = get,
         setResult = setResult,
         getResult = getResult)

}

# actually perform the matrix inversion
# X should be the result of makeCacheMatrix
# standard memoization routine

cacheSolve <- function(x, ...) {
    cachedResult <- x$getResult()

    # a non-null result means we can reuse the cached answer
    # null means we need to compute it on the fly

    if(!is.null(cachedResult)) {
        return(cachedResult)
    }

    # compute it
    m <- x$get()
    result <- solve(m, ...)

    # cache it for later
    x$setResult(result)

    # return the value
    result
}
