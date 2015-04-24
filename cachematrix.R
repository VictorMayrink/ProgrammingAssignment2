#Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse.

#The first function, makeCacheMatrix creates a "special matrix" (called 'x'), which is really a list containing a function to
# set the matrix 'x'
# get the matrix 'x'
# set the inverse matrix of 'x'
# get the inverse matrix of 'x'

makeCacheMatrix <- function(x = matrix()) {
       
        inv <- NULL; #inverse of x (initially NULL)
        
        #Functions
        setmat <- function(m) {
                x <<- m; #change the current matrix
                inv <<- NULL; #since the matrix was changed, the inverse is not the same 
        }
        getmat <- function() x; #return the current matrix
        setinv <- function(minv) inv <<- minv; #updates the cached inverse matrix
        getinv <- function() inv; #return the current cached inverse matrix
        
        list(setmat = setmat, getmat = getmat, setinv = setinv, getinv=getinv);

}


# The following function calculates the inverse matrix of the "special matrix" created with the above function. 
# However, it first checks to see if the inverse matrix has already been computed. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it computes the inverse matrix of our "special matrix" 
# and updates the inverse matrix in the cache via the 'setinv' function.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv(); #gets the current inverse matrix
        
        if(!is.null(inv)) { #if the inverse has already been computed, use the cached matrix
                message("getting cached matrix");
                return(inv);
        }
        
        #...else, computes the inverse matrix and stores the result
        m <- x$getmat();
        inv <- solve(m);
        x$setinv(inv)
        inv
}
