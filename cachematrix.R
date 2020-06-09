## The pair of functions below are used to cache the inverse of a matrix.

## The first function, makeCacheMatrix, creates a special "matrix" object,
## which is really a list containing a function to:
## 1. set the value of the matrix;
## 2. get the value of the matrix;
## 3. set the value of the inverse of the matrix;
## 4. get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function above. However, if first checks 
## whether the inverse has already been calculated before. If so, it retrieves
## the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets its value in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
