## Inverts matrix. has a caching component to reduce unnecessary computing.

## Cache function

makeCacheMatrix <- function(x = matrix()) {
        cachedmatrix <- NULL
        cachedinverse <- NULL
        set <- function(y) 
        {
                x <<- y
                cachedinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedinverse <<- inverse
        getinverse <- function() cachedinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Matrix inversion function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        cachedinverse <- x$getinverse()
        if(!is.null(cachedinverse)) 
        {
                message("getting cached data")
                return(cachedinverse)
        }
        data <- x$get()
        cachedinverse <- solve(data, ...)
        x$setinverse(cachedinverse)
        message("getting non-cached data")
        return(cachedinverse)     
}