## There are two functions here:
## makeCacheMatrix and cacheSolve
## makeCacheMatrix returns a list that caches the matrix and its inverse, and can provide those values on demand
## cacheSolve returns the inverse of the matrix, either by reading it from cache or by direct calculation

## Function makeCacheMatrix takes in a nonsingular matrix object
## and caches this matrix object and its inverse
## makeCacheMatrix returns a list of four functions
## that allow to call and set the values of the matrix object and its inverse and cache them

makeCacheMatrix <- function(x = matrix()) {
        
        ## set up a variable to store the inverse of the input matrix
        inverseOfX <- NULL
        
        ## function that changes the value of the originally cached matrix
        setMatrix <- function(y) {
                
                ## value stored in the variable x (original matrix) is changed
                ## to the value of variable y
                x <<- y
                
                ## consequently the value of the matrix inverse is erased
                inverseOfX <<- NULL
        }
        
        # function that calls the original matrix
        getMatrix <- function() x
        
        ## function that sets the inverse of the matrix to its known value,
        ## provided that this value was previously calculated, and caches it
        setInverse <- function(inversedMatrix) inverseOfX <<- inversedMatrix
        
        ## function that calls the inverse of the original matrix
        getInverse <- function() inverseOfX
        
        ## makeCacheMatrix returns a list object as follows:
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}

 
## Function cacheSolve takes the list object created by makeCacheMatrix 
## from original matrix 'x' as input
## then it checks if the inverse of the matrix is stored in cache;
## if the inverse is not stored in cache, it calculates the inverse
## and stores its value in cache by using the functions of the list object

cacheSolve <- function(listInCache, ...) {
        
        ## Try to return the inverse of original matrix 'x'
        ## if it is stored in cache
        inverseOfX <- listInCache$getInverse()
        if(!is.null(inverseOfX)) {
                message("Getting cached data ...")
                
                ## return the inverse to console
                return(inverseOfX)
        }
        
        ## if the inverse of the original matrix 'x'
        ## is not stored in cache yet
        ## then call original matrix ...
        originalMatrix <- listInCache$getMatrix()
        
        ## ... calculate its inverse ...
        inverseOfX <- solve(originalMatrix, ...)
        
        ## ... and store this inverse of the original matrix 'x' in cache
        ## by using the list object function
        listInCache$setInverse(inverseOfX)
        
        ## return the inverse to console
        inverseOfX
}
