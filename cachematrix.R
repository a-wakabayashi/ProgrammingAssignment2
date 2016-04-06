# This pair of functions will cache the inverse of a matrix. 

# This first function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix contains a list of functions that do the following:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # cinverse stores the cached inverse
        cinverse <- NULL 
        # set the matrix
        set <- function(y) {
                x <<- y
                cinverse <<- NULL
        }
        # gets the matrix
        get <- function() x
        # sets the inverse
        setinverse <- function(inverse) cinverse <<- inverse
        # gets the inverse
        getinverse <- function() cinverse
        # creates a list that includes the 4 functions required to define the matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## if the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        # gets the cached inverse
        cinverse <- x$getinverse()
        # if the inverse matrix was cached, returns the cached matrix
        if(!is.null(cinverse)) {
                message("getting cached data")
                return(cinverse)
        }
        # if there is no cached matrix, calculates the inverse
        # gets matrix
        data <- x$get()
        # gets inverse
        cinverse <- solve(data, ...)
        # caches the result
        x$setinverse(cinverse)
        ## returns a matrix that is the inverse of 'x'
        cinverse
}

# Example
#> matrix1 <- makeCacheMatrix(matrix(1:4, 2, 2))
#> matrix1$get()
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4          Gets the matrix
#> matrix1$getinverse()
#NULL                    No cache yet
#> cacheSolve(matrix1)
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5          Calculates inverse of matrix
# 2nd run looking for the cached inverse
#> cacheSolve(matrix1)
#getting cached data
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5          Gets the inverse from the cache