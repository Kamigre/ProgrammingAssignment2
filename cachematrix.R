## Function 1

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Function 2

cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

## Return a matrix that is the inverse of 'x'

matrix1 <- makeCacheMatrix(matrix(1:4,2,2))
matrix1$get()
cacheSolve(matrix1)
