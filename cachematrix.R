## Matrix inversion is usually a costly computation. The following two 
## functions cache the inverse of a matrix and retrieves it from cache, 
## if already computed.

## makeCacheMatrix :: returns a list containing functions to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of inverse of the matrix
##      4. get the value of inverse of the matrix
## this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## cacheSolve ::  for a given square invertible matrix x , 
## returns the inverse of the matrix. It first checks if the inverse has 
## already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.
cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}

## Tests :: 
# > source("ProgrammingAssignment2/cachematrix.R")
## Test simple 4 X 4 matrix
# > m1 <- matrix(1:4, 2, 2)
# > testMatrix1 <- makeCacheMatrix(m1)
# > cacheSolve(testMatrix1)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(testMatrix1)
# getting cached data.
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
## Test with Larger matrices
## 1. 100 X 100 (store result in variable )
# > m2 <- matrix(rnorm(10000), 100, 100)
# > testMatrix2 <- makeCacheMatrix(m2)
# > result1 <- cacheSolve(testMatrix2)
# > result1 <- cacheSolve(testMatrix2)
# getting cached data.
# >
## 2. 1000 X 1000 (store result in variable )
# > m3 <- matrix(rnorm(1000000), 1000, 1000)
# > testMatrix3 <- makeCacheMatrix(m3)
# > result3_1 <- cacheSolve(testMatrix3)
# > result3_2 <- cacheSolve(testMatrix3)
# getting cached data.
# > 
## result3_2 returns faster than result3_1 
