## For a square matrix A, the inverse is written A-1. 
## When A is multiplied by A-1 the result is the identity matrix I. Non-square matrices do not have inverses.

# makeCacheMatrix and cacheSolve functions work in tadem on a square Matrix inversion. 

# makeCacheMatrix creates a special "matrix" object and will cache its inverse.

makeCacheMatrix <- function(xMatrix = matrix()) {    # define the argument xMatrix with default mode of "matrix"

    invMatrix <- NULL                                # initialize invMatrix as NULL; invMatrix will hold value of xMatrix inverse
    set <- function(newOne)                          # define the set function to assign new value to xMatrix
        xMatrix <<- newOne                           # Set the xMatrix as newOne, value of xMatrix in containing/parent environment
        invMatrix <<- NULL                           # if there is a new matrix, reset invMatrix to NULL

    get <- function() {xMatrix}                      # define the get function - returns value of the xMatrix argument
    
    setInvMatrix <- function(inv) invMatrix <<- inv  # assigns value of inv in parent environment
    getInvMatrix <- function() {invMatrix}           # gets the value of inv where called

    list(set = set, get = get,                       # Returns a list allowing the the use of the $ operator for 
                                                     # functions set and get of invMatrix
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


# cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(xMatrix,...) {              # Return a matrix that is the inverse of 'xMatrix'
    
    invMatrix <- xMatrix$getInvMatrix()            # get the inverse of the matrix.
    
    if(!is.null(invMatrix)) {                      # check if there is the matrix, if yes: return invMatrix.
        print("Inverted Matrix exists")
        return(invMatrix)
    }
    
    # if not: then invert newMatrix.
    print("Inverted Matrix does not exist, newMatrix will be inverted")
    
    newMatrix <- xMatrix$get()                    # get the matrix
    invMatrix <- solve(newMatrix,...)             # invert the matrix
    xMatrix$setInvMatrix(invMatrix)               # set the inverse of the matrix.
    invMatrix
}

