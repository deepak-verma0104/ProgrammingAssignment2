## Overall description of functions

### The functions display the way one can cache the result of time consuming computations and use the cached value later.
### Not Repeating the computations and using the cached value helps in improving the performance of R code. 
### This program takes advantage of scoping rules of R Language and illustrate the use of an R Object and preserved state inside R Object.
### Concretely, these functions are calculating the inverse of a matrix and caching it for future use.

## Short comment describing makeCacheMatrix function

### makeCacheMatrix function, creates a special "matrix", which is really a list containing functions to
#### 1. set the matrix
#### 2. get the matrix
#### 3. set the inverse of the matrix
#### 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        #m to store / cache the inverse of the matrix
        m <- NULL
        
        #set the matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        
        #get the matrix
        get <- function() x
        
        #set the inverse of the matrix
        set_inverse <- function(inverse) m <<- inverse
    
        #get the inverse of the matrix
        get_inverse <- function() m
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
}


## Short comment describing cacheSolve function

### cacheSolve function calculates the inverse of the "matrix" created with makeCacheMatrix function above. 
### assumption - the matrix created in makeCacheMatrix is always an invertible matrix  
### It first checks to see if the inverse has already been calculated and cached
#### - If inverse already been calculated then it gets the Inverse from the cache and skips the computation. 
#### - Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
        
        ## Function Returns a matrix that is the inverse of 'x'
        
        # Retrieves value of m for matrix object x set by makeCacheMatrix function 
        m <- x$get_inverse()
        
        #if inverse already calculated and cached then m will have a non-null value ( assuming the matrix created in makeCacheMatrix is always an invertible matrix )
        #if so get the inverse from cached value in m and return from the function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #if inverse not calculated and cached, then calculate inverse 
        data <- x$get()
        m <- solve(data, ...)
        
        #set/cache value of m in object x with inverse matrix calculated  
        x$set_inverse(m)
        m
}
