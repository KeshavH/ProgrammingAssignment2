## These functions first create the matrix cache, and then solve for its inverse, then
## caching that, and checking and simply using the cached answer in repeated uses.

## this function creates the functions to be used after its own use, and sets 
##m in a higher lexical scope with its intended value

makeCacheMatrix <- function(x = matrix()) { ## Based on example code given in assignment
        m <- NULL    ##Nulls m in case of previous value set
        set <- function(y) {   #creates the set function
                x <<- y
                m <<- NULL
        }
        get <- function() x      #creates the get function, which must be used after execution for program use
        setsolve <- function(solve) m <<- solve  #creates setsolve function to set m as solve in higher state (lexical scoping)
        getsolve <- function() m        #Creates getsolve function to return m
        list(set = set, get = get,     #returns a list of the previous
             setsolve = setsolve,
             getsolve = getsolve)
}


## this either uses the previously established functions to compute the inverse
## of the the matrix, or retrieves the cached result, should it detect there to be one

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()   #makes use of the previously established getsolve function
        if(!is.null(m)) {      ##checks for cached value
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m             ## returns m
}