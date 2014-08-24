makeCacheMatrix <- function(x = matrix()) {
    ## Store the inverse matrix
    inversedMatrix <- NULL
    
    ## Set the matrix
    set <- function(newMatrix){
        x <<- newMatrix
        inversedMatrix <<- NULL
    }
    
    ## Get stored matrix
    get <- function(){
        x
    }
    
    ## Set inverse matrix
    setInversed <- function(inverse){
        inversedMatrix <<- inverse
    }
    
    ## Get stored inverse matrix
    getInversed <- function(){
        inversedMatrix
    }

    list(set = set, get = get,
         setInversed = setInversed,
         getInversed = getInversed)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInversed()
    
    ## Check for stored result
    if(!is.null(inverseMatrix)) {
        message("getting cached inverse of the matrix")
        return(inverseMatrix)
    }
    
    ## Calculation if inverse matrix
    matrix <- x$get()
    inverseMatrix <- solve(matrix)
    
    ## Store inverse matrix
    x$setInversed(inverseMatrix)
    
    inverseMatrix
}
