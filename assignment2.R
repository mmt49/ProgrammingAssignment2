# The following function creates a special "matrix" object that can cache its 
# inverse. This is essenctially a list containing a function to (1) set the  
# value of the matrix, (2) get the value of the matrix, (3) set the value of the 
# inverse, (4) get the value of the inverse. 

makeCacheMatrix <- function(M = matrix()) {
        INV <- NULL
        set <- function(Y) {
                M <<- Y
                INV <<- NULL
        }
        get <- function() M
        setinv <- function(inv) INV <<- inv
        getinv <- function() INV
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The following function calculates the inverse of the special "matrix" created 
# with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the matrix and sets 
# the value of the inverse in the cache via the setinv function.

cacheSolve <- function(M, ...) {
        INV <- M$getinv()
        if(!is.null(INV)) {
                message("getting cached data")
                return(INV)
        }
        mat <- M$get()
        INV <- solve(mat, ...)
        M$setinv(INV)
        INV
}