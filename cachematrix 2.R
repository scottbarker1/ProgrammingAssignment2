## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL   # set inverse to NULL
    
    set <- function(y) {    # create function call set, to assign null to the inv
        
        x <<- y   #  This assigns "Y" value to "x"  as a global variable using the special assignment operation "<<".
        
        inv <<- NULL   # Sets "inv" to NULL and as a global variable.
        
    }
    
    get <- function() x   # create function called get()  This function set the inverse value and get the inverse value
    
    setinverse <- function(inverse) inv <<- inverse   # Create setinverse function.  This set's the inverse of the matrix
    
    getinverse <- function() inv   # Set the inverse value
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    # Create a list using the Set/Get Inverse in cache
    
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()  # creates a function that get's the inverse of a matrix.
    
    if(!is.null(inv)) {   # If the matrix "inv" is not null, the data is returned from cache.
        
        message("getting cached data.")
        
        return(inv)  # End the function returning inverse of the matrix.
        
    }
    
    data <- x$get() # Function to $get "x" data.  Returns the value of the matrix.
    
    inv <- solve(data)   # Call solve function to invert data. Retrieve data from cache.
    
    x$setinverse(inv)    # Set inverse of data "x"
    
    inv     # print data
    
}