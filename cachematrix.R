## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function : 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL # m = Inverse of the matrix x 
    set <- function(y) {
            x <<- y # x = Matrix 
            m <<- NULL
    }
    get <- function() x
    setInversematrix <- function(InvMat) m <<- InvMat
    getInversematrix <- function() m
    list(set = set, get = get,
             setInversematrix = setInversematrix,
             getInversematrix = getInversematrix)

}


## Write a short comment describing this function
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInversematrix()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    InvMat <- solve(data, ...) # X is a square invertible matrix, then solve(X) returns its inverse
    x$setInversematrix(InvMat)
    InvMat
}

# We can test these fonction with this command : 
# > test <- matrix(1:4, 2,2)
# > cacheSolve(makeCacheMatrix(test))
