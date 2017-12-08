## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix with the functionality of caching
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## this function checks whether we have already calculated the inverse of the matrix, if yes, it retrieaves 
## previously calculated inverse. if it is not already calculated, the function calculates it and stores it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

#sample code to check the functionality
Mat1 <- matrix(rnorm(100), ncol=4)
Mat2 <- matrix(rnorm(100), ncol=4)
Mat <- crossprod(Mat1,Mat2)

y = makeCacheMatrix(Mat)
s = cacheSolve(y)
#second run. This will print "getting cached data"
cacheSolve(y)

