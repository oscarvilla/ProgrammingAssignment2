## This pair of functions cache the inverse of a matrix
##If matrix inverse has been calculated, cached it. If not, calculate it

## This function creates a special "matrix" object that can cache its
## inverse if these has been calculated

makeCacheMatrix <- function(x = matrix()) {
        ##'m' is the inverse of the matrix
        m <- NULL ##'m' is stated as NULL
        set <- function(y) {
                ##the value of 'y' is passed to 'x' on the external
                ##environment to be calculate by the another function
                ##wich call to m$set
                x <<- y 
                m <<- NULL ##then 'm' is set up NULL
        }
        ##'y' has been received as 'x' from the previous function
        get <- function() x
        ##the inverse of matrix x is calculate through an anonymous fun
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        ##arguments calculations are passed as a list 'm$set', 'm$get'
        ##'m$getinv' and 'm$setinv'
}


##This function cache the inverse of the matrix 'x' is these has been
##calculated. If not, calculate it

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
