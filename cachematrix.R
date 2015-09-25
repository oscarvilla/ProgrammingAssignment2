## This pair of functions cache the inverse of a matrix
##If matrix inverse has been calculated, cached it. If not, calculate it

## This function creates a special list of objetcs wich elements are
##function to set and get the matrix and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
        ##'m' is the inverse of the matrix
        m <- NULL ##'m' is stated as NULL
        set <- function(y) {
                ##the value of 'y' is passed to 'x' on the external
                ##environment to be calculate by the another function
                x <<- y 
                m <<- NULL
        ##then 'm' and 'x' are setup NULL out of this environment
        }
        ##this functions will be passed out as a list
        get <- function() x
        ##the inverse of matrix x is calculate through an anonymous fun
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        ##arguments calculations are passed as a list wich elements
        ##can be called as'm$set', 'm$get', 'm$getinv' and 'm$setinv'
}

##This function cache the inverse of the matrix 'x' is this has been
##calculated. If not, calculate it calling to another functions

cacheSolve <- function(x, ...) {
        ##the value of the inverse of the matrix 'x' is cached as 
        ##argument
        m <- x$getinv()
        ##if the inverse is no NULL, that means already was calculated
        ##and is cached and returned with a statement message
        if(!is.null(m)) {
                message("getting cached matrix data")
                return(m)
        }
        ##if has not been calculated, it is done
        data <- x$get() ##captures 'x' calling to another function
        m <- solve(data, ...) ##calculates the inverse of 'x'
        x$setinv(m) ##pass the inverse to the function wich states it
        m
}
