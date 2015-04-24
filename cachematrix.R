# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a functionsthat will:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The cacheSolve function will first check to see if an inverse has already been computed. 
##If it has, it will get the cashed value and not do the computation.  If there isn't a 
##cached value, it will compute the inverse and set the value in the cache by using 
##the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Retrieved from cached data.")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m)
        x$setinverse(inv)
        inv
}

## Test Run
## > m<-matrix(c(4,3,3,2),nrow=2, ncol=2)
## >test<-makeCacheMatrix(m)
##> test$get()
##[,1] [,2]
##[1,]    4    3
##[2,]    3    2

## First time using cacheSolve with nothing stored in cache
##> cacheSolve(test)
##[,1] [,2]
##[1,]   -2    3
##[2,]    3   -4

## Second time when there are values in cashe
##> cacheSolve(test)
##Retrieved from cached data.
##[,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
