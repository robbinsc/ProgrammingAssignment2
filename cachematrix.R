
##R Programming Coursera, Programming Assignment 2

##make a special vector, which is a list containing functions to set and get
## values for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<-inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## takes a matrix as its input, and based on the command given, sets or gets the matrix and its inverse 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'       
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinverse(inv)
        inv
}


