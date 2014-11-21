##This scripts attempts to reduce the computational cost of calculating 
##the inverse of a matrix by creating a special matrix which caches allows
##the chaching of the inverse


## makeCacheMatrix creates a special 'matrix' which is a list of functions that
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y){
               x <<- y
               v <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) v <<- inverse
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve returns the cached inverse of the special 'matrix' created
## by makeCacheMatrix. If a cache inverse is not found, an inverse is
## computed and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinverse()
        if(!is.null(v)){
            message("getting cached data")
            return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
}
