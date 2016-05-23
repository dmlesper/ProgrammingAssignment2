makeCacheMatrix <- function(x= matrix()) {
        nada <- NULL
        set <- function(y) {
                x<<- y
                nada <<- NULL
        } #1 set the value of the matrix
        get <- function () x #2 get the value of the matrix
        setinv <- function(inverse) nada <<- inverse #3 set value of inverse
        getinv <- function() nada #4 get value of inverse
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
        } 
## This second function will obtain the results of the first function
# and calculate out the inverse of the matrix 
cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        m<- x$get()
        inverse <- solve(m, ...)
        x$setInverse(nada)
        nada
}
