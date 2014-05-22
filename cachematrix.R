## Overall description of functions
### The functions display the way one can cache the result of time consuming computations and use it later.
### Not Repeating the computations and using the cached value helps in improving performance R code. 
### This program takes advantage of scoping rules of R Language and illustrate the use of an R Object and preserrved state inside R Object.
### Concretely, these functions are calculating the inverse of a matrix and caching it for future use.

## Short comment describing makeCacheMatrix function

### makeCacheMatrix function, creates a special "matrix", which is really a list containing functions to
### 1. set the value of the matrix
### 2. get the value of the matirx
### 3. set the value of the inverse
### 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
}


## Short comment describing cacheSolve function

### cacheSolve function calculates the inverse of the "matrix" created with makeCacheMatrix function above. 
### assumption - the matirx created in makeCacheMatrix is always an invertible matrix  
### It first checks to see if the inverse has already been calculated. 
#### - If inverse already been calculated then it gets the Inverse from the cache and skips the computation. 
#### - Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
