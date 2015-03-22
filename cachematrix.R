## The pair of functions below are used to cache the inverse of a matrix and return this cached value 
## if it has already been calculated



## The makeCacheMatrix function creates a list containing a function to set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    stored_inv <- NULL
    
    set <- function(y) {                            ## Sets the value of the matrix
        x <<- y
        stored_inv <<- NULL
    }
    
    get <- function() {                             ## Returns the value of the matrix
        return (x)
    }
    
    setinv <- function(sent_replacement_inv){       ## Sets the calculated inverse of the matrix
        stored_inv <<- sent_replacement_inv
    }
    
    getinv <- function() {                          ## Returns the cached inverse of the matrix
        return(stored_inv)
    }
    
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The cacheSolve function computes the inverse of the special "matrix" returned by the makeCacheMatrix function.
## If the inverse has already been calculated then the inverse from the cache is returned

cacheSolve <- function(x, ...) {
    
    local_inv <- x$getinv()                 ## Get the cached inverse of the matrix
    
    if(!is.null(local_inv)) {               ## Checks if inverse has been cached (is not null)
        message("getting cached data")
        return(local_inv)                   ## returns cached result
    }
    else {
        local_data <- x$get()
        local_inv <- solve(local_data, ...) ## calculates the inverse of the matrix
        
        x$setinv(local_inv)                 ## caches the result
        return(local_inv)                   ## returns the calculated inverse
    }
}
