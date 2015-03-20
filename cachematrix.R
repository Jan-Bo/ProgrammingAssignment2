## The purpose of these functions is to calculate the inverse of a matrix only once,
## storing the result in a cache. In subsequent occasions, the inverse does not 
## need to be computed again, but it will be retrieved directly from the cache.


## The first function will create a list of functions to contain the matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {        
        cachei <- NULL                                          ## i is the variable that will contain the cached
                                                                ## inverse later. It is initialized to NULL.
	
        set <- function(y) {                                    ## set will store the input matrix (called y here)
                x <<- y                                         ## in the function environment and set the cached inverse i to NULL
                cachei <<- NULL                                 ## You could call this to initualize the object although
        }                                                       ## calling just makeCacheMatrix does essentially the same (!).
                                                                ## I suppose it is quicker than makeCacheMatrix, 
                                                                ## so it may be useful if the object needs to be initialized
                                                                ## multiple times in a loop...										

        get <- function() x                                     ## delivers the matrix x stored inside the function

        setinv <- function(replacei) {cachei <<- replacei}      ## setinv sets the cached inverse to a new value

        getinv <- function() cachei                             ## getinv retrieves the cached inverse

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## The second function needs a "makeCacheMatrix" object as an argument. It will
## first check whether the inverse has alrady been calculated. If yes, it just returns
## the inverse that was previously stored. If not, it computes the inverse using
## the 'solve' function.

cacheSolve <- function(x) {
        cachei <- x$getinv()                                    ## retrieve the cached inverse from the
                                                                ## "makeCacheMatrix" object x

        if(!is.null(cachei)) {                                  ## if the cache is not empty (i.e. not NULL)
                message("getting cached data")                  ## and return the pre-calculated inverse 
                return(cachei)
        }
	  else {                                                ## else (if the cache IS still empty)
        	data <- x$get()	                                ## then get the data matrix stored inside the x object,
        	i <- solve(data)                                ## compute its inverse
        	x$setinv(i)                                     ## store it inside the x object	
        	return(i)                                       ## and return it
	  }
}


