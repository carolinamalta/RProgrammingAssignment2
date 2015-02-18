## The functions intend to apply the inverse of a matrix while retrieving a value stored 
## in the cache that corresponds to the inverse of a square matrix x. 

## Here I defined the type of matrix to use, along with values set to NULL

makeCacheMatrix <- function(x = matrix(C(1,2,3,4),nrow=2, ncol=2)) { ## assume that my matrix is a square matrix
         d <- NULL
         set<- function(y) {
                 x <<- y        
                 d <<- NULL
         }
         get <- function() x
         setinverse <- function(solve) d <<- inverse
         getinverse <- function() d
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (!is.null(d) & det(x) == 0) { ## determinant of x cannot be 0
                message("Cannot calculate inverse of matrix; determinant equals 0")  
                return(d)}
        else if (!is.null(d) & det(x) != 0)## if value isn't null and determinant not 0 then retrieve what's in the cache
                {
                message("getting cached data")
                return(d)
        }
           
        inverse<- x$get()   ##invoking get function inside x which retrieves stored value in makeCacheMatrix
        d <- solve(inverse)  ## executing function
        x$setinverse(d)         ## setting the value retrieved as the new value
        d
}

