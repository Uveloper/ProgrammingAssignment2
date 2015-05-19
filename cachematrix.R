##The first function, makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to

## 1. set the value of the matrix x  
## 2. get the value of the matrix x 
## 3. set the value of m using solve 
## 4. get the value of m that was solve 
## 5. returns a list of functions


makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	    
	## sets the value of y to x, but set was not called  
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
        
    ## returns x
    get <- function() {
        x
    }
     
    ## sets the matrix m   
    setsolve <- function(value){
         m <<- value
    }
        
    ## returns m 
    getsolve <- function(){
         m
    }
        
    ## returns a list of functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
}


## The second function returns a inverse matrix using the solve function

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
        
    ## assigns the get function to m
    m <- x$getsolve()
        
    ## checks if m is not null, if it is not, it returns m, and displays the message 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## assign the matrix x to data
    data <- x$get()
        
    ## use the solve function to get an inverse of the data, and assign it to m 
    m <- solve(data, ...)
        
    ## calls the set solve function, and passes it the argument m just populated 
    x$setsolve(m)
        
    ## returns the inversed matrix
    m
}


## Test data:
## source("cachematrix.R")
## B = matrix(c(2, 4, 3, 1, 5, 7, 6, 4, 3), nrow=3, ncol=3)
## val <- makeCacheMatrix(B)
## cacheSolve(val)