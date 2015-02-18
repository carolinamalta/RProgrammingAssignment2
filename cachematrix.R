## The functions intend to apply the inverse of a matrix while retrieving a value stored 
## in the cache that corresponds to the inverse of a square matrix x. 

## Here I defined the type of matrix to use, along with values set to NULL

makeCacheMatrix <- function(x = matrix(C(1,2,3,4),nrow=2, ncol=2)) { ## assume that my matrix is a square matrix
         d <- NULL              ## value originally set as NULL
         set<- function(y) {
                 x <<- y        
                 d <<- NULL     ##value in cache set to NULL
         }
         get <- function() x
         setinverse <- function(solve) d <<- inverse  ## to set the inverse variable we create a solve function 
         ##and assign inverse value in the cache that revolves around the get function
         getinverse <- function() d
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## the cacheSolve function attempts to check for the value of d, whether it is null or not; the if function evaluates
## the condition of not being NULL and the matrix's determinant not being = 0 since that theoretically would make the
## matrix not inversible; the opposite outcome is set in the function above as default value

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

