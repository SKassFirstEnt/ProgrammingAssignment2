## Put comments here that give an overall description of what your
## functions do

## This first function gets the matrix passed in as an argument, it has a set function that can cache an inversed matrix to x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y){
        x <<- solve(y)
        m <<- NULL
    }
    
    get <- function(){
        x
    } 
    
    wasSet <- function() m
    
    setCache <- function() m <<- 1

    list(get = get, set = set, wasSet = wasSet, setCache = setCache)
}


## we retrieve the matrix and see if wasSet is null, if yes then the matrix hasn't changed. Else we 
## create the new one and setCache to 1

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$wasSet()
    print(m)
    if(!is.null(m)){
        
        v <- x$get()
    }
        
        b <- x$get()
        v <- solve(b)
        x$setCache()
    
    v
}
