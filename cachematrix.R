## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(inverse) m <<- inverse
    matrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         matrix = matrix) 
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$matrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

test = c(1,2,3,4,5,6,8,9,0)
dim(test)=c(3,3)
solve(test)

test1 <- makeCacheMatrix(test)
test2 <- cacheSolve(test1)
test2

