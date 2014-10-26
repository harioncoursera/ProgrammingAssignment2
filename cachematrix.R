## These functions essentially help in reducing time in calculating the inverse of matrix by creating a cache copy
## and extracting the inverse matrix from cache

## This function is a getter and setter function which gets or sets the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        xinverse <- NULL
        set <- function(y) {
                originalMatrix <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(z) xinverse <<- z
        getinverse <- function() xinverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will retrieve the inverse from cache or calculates the inverse and sets the inverse in cache if not already done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        data <- x$get()
        xinverse <- solve(data, ...)
        x$setinverse(xinverse)
        xinverse
}
