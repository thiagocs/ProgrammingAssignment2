

## The function below creates a "matrix" object which caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL 
        
        set <- function(y) {
                x <<- y
                xinv <<- NULL 
        }
        
        get <- function() x 
        setInv <- function(inv) xinv <<- inv 
        getInv <- function() xinv 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The function belown computes the inverse of the special "matrix" returned by the function above. If the inverse is already done
## cacheSolve will return its previous result from cache therefore saving computational efforts.

cacheSolve <- function(x, ...) {
        m <- x$getInv() 
        if(!is.null(m)) { 
                
                return(m) 
        }
        data <- x$get() 
        m <- solve(data) 
        x$setInv(m) 
        m 
}