## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #initially sets the matrix_inv variable to NUll. It is the variable where inverse of the matrix will be stored
        matrix_inv <- NULL
        #takes the matrix and setting that value to the local variable x
        set <- function(my_mat){
                x <<- my_mat
                matrix_inv <<- NULL
        }
        #gets the stored matrix from the local variable x
        get <- function() x
        #sets the inverse of the given matrix to a local variable called matrix_inv
        setinverse <- function(inverse) matrix_inv <<- inverse
        #gets the inverse of the given matrix
        getinverse <- function() matrix_inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        # checks if the inverse variable contains data or not. If it does contain some data, then that value is retrieved.
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        #gets the data from the previously defined function
        my_data <- x$get()
        #solve() function to find the inverse of the matrix being given as the input
        inverse <- solve(my_data, ...)
        #setting the inverse value using the defined function above
        x$setinverse(inverse)
        #final statement as a return value
        inverse
}
