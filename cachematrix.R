## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
    ## mat:  a matrix 
    ## mInv: a inverse of mat
    ## Return: a list containing functions to
    ##              1. set the matrix   => set
    ##              2. get the matrix   => get
    ##              3. set the inverse  => setMatInv
    ##              4. get the inverse  => getMatInv
    ##  
    ## Purpose: Create a special "matrix" object that can cache its inverse, that is used as 
    ##          input to the function cacheSolve
    ##    
    
    ## Clear the matrix inverse value
    mInv <- NULL
    
    ## set the matrix value
    set <- function(f) {
        mat <<- f
        mInv <<- NULL
    }
    
    ## Get the matrix value
    get <- function() mat
    
    ## Function to set the inverse
    setMatInv <- function(solve) mInv <<- solve
    ## Function to get the inverse
    getMatInv <- function() mInv
    
    ## Return the functions 
    list(set = set, 
         get = get,
         setMatInv = setMatInv,
         getMatInv = getMatInv)
}



## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
    ## mInv   : inverse of the original matrix
    ## datos  : matrix value
    ## Return : inverse mInv value 
    ##  
    ## Purpose: computes the inverse of the special "matrix" returned by 
    ##          makeCacheMatrix
    ##          If the inverse has already been calculated 
    ##          (and the matrix has not changed), then cacheSolve return the inverse 
    ##          previously computed
    
    
    ## Get the previously calculated inverse (from the cache)      
    mInv <- mat$getMatInv()
    
    ## If not empty ?
    if(!is.null(mInv)) {
        # Then return the cache value 
        message("Data from the cache")
        return(mInv)
    }
    
    ## The cache was empty, get the matrix value 
    datos <- mat$get()
    ## Compute  the inverse
    mInv <- solve(datos)
    
    ## Set the inverse value to the cache
    mat$setMatInv(mInv)
    
    ## Return the inverse matrix
    return (mInv) 
    
}