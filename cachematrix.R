# Name: Rachel
# Time: 06/27/2021

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv

        list(set=set, get=get, 
                setinverse=setinverse, getinverse=getinverse)
}


## Get the inverse of matrix; if the inverse already exist, get the inverse directly; otherwise calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return (inv)

}
