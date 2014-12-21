## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that return a list which contains functions as follow:
##set : function which sets the value of the matrix
##get : function which returns the value of matrix
##setinv : function which sets the value of the matrix inverse
##getinv : function which returns the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
          }
        get <- function() x
        setinv <- function(z) inv <<- z
        getinv <- function() inv
        list( set = set, get= get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function which returns the matrix inverse from makeCacheMatrix function
##if the matrix inverse is avaialble in cache, it'll take the value from the cache.
##It'll compute the inverse of matrix if the value is unavalable in chace.
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        inv<- x$getinv()
        if (!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
