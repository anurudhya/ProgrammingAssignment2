## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(my_mat){
                x <<- my_mat
                matrix_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_inv <<- inverse
        getinverse <- function() matrix_inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        my_data <- x$get()
        inverse <- solve(my_data, ...)
        x$setinverse(inverse)
        inverse
}
