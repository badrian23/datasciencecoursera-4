## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(inverse) m <<-inverse
        getmean <- function() x
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
         m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        m <- solve(x$get())
        x$setinverse(m)
        m
}
