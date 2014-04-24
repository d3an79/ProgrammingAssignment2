## Functions to cache the inverse of a given matrix the first time it is passed.
## If the same matrix is passed again then the already computed inverse is returned.

## Creates a cacheMatrix object containing fuctions as list items that can
## be accessed to initialise the object, get the pre-cached inverse,
## set the inverse or get the inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

## Function to return a matrix that is the inverse of 'x'. First checks the
## cache for answer. If the cache is null then the matrix is inverted and the
## assigned to the cacheMatrix object
cacheSolve <- function(x, ...) 
{        
    i <- x$getInverse()
    if(!is.null(i)) 
    {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    print(i)
}
