## These two functions together allow the user to cache the inverse of a matrix
## so that the result can be reused rather than computed repeatedly when it is needed.
## makeCacheMatrix() creates a list of functions that allow the user to get and set the
## value of a matrix and its inverse. cacheSolve() will use the getinv() function in
## makeCacheMatrix to check whether the matrix has been previously solved. If so, it will return
## the cached solution. If not, cacheSolve() will compute the inverse and then use setinv() to 
## cache it.

## makeCacheMatrix first clears the value of inv from the parent environment. Then it defines four
## functions: 
##
## set() allows the user to set a new input matrix. When called, it will clear the value of inv.
## get() is used by cacheSolve() to retrieve the current input matrix
## setinv() is used by cacheSolve() to cache the solution that it has computed
## getinv() is used by cacheSolve() to retrieve the cached solution, if one exists
##
## makeCacheMatrix returns a list of the four functions

makeCacheMatrix <- function(x = matrix()) {
    inv <<- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() takes the makeCacheMatrix object as an input. 
## It will first check whether a previously cached solution to the matrix exists using getinv().
## If the cached solution exists, it will return the cached solution. 
## If there is no cached solution, it will compute the matrix inverse using solve(), cache
## the solution using setinv(), and return the computed matrix inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
