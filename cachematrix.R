## A pair of functions that calculate the inverse of a matrix 
## and caches the result

## This function takes a matrix as an arguement and creates a special matrix 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        ## Define variables 'x' and 'inv' in the global environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<-inverse
    getinverse <- function() inv
    ## Returns a list of 4 functions associated with the special matrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function searches for chached inverse matrix and returns its value
## If no cached data is found, this function computes the inverse of the matrix
## and returns and chaches the result

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)  ## Returns a matrix that is the inverse of 'x' from cache
                     ## and exits the function
    }
    data<- x$get()  ## Assigning the matrix from MakeCacheMatrix() as 'data'
    inv<- solve(data, ...) ## Computes the inverse of the matrix 'data'
    x$setinverse(inv) ## Calls on setinverse() to cache the value of 'inv' 
    inv
}
