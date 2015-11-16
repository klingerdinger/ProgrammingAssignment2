#_____________________________
#---- A function which creates an extended matrix object,
#---- containing it's own cached inverse
makeCacheMatrix <- function(theMatrix = matrix()) {
    theInverse <- NULL
    
    #---- Setting functions
    set <- function(newMatrix) {
        theMatrix <<- newMatrix
        theInverse <<- NULL
    }
    setinverse <- function(solve){ 
        theInverse <<- solve
    }
    
    #---- Getting functions
    getinverse <- function() theInverse
    get <- function() theMatrix

    #---- A list of functions
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

#_____________________________
#---- Function to calculate/retrieve the inverse 
#---- of a cacheMatrix object
cacheSolve <- function(theMatrix, ...) {
    #---- Check whether the inverse has already been calculated
    theInverse <- theMatrix$getinverse()
    if(!is.null(theInverse)) {
        message("getting cached data")
        return(theInverse)
    }
    #---- Otherwise, calculate the inverse
    data <- theMatrix$get()
    theInverse <- solve(data, ...)
    theMatrix$setinverse(theInverse)
    theInverse
}
