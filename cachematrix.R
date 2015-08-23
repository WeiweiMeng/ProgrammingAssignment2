makeCacheMatrix <- function(x = matrix()) {
    CMInv <- NULL
    set <- function(y){
        x <<- y
        CMInv <- NULL
    }
    get <- function() x
    setInv <- function(inverse) CMInv <<- inverse
    getInv <- function() CMInv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
}




cacheSolve <- function(x, ...) {
    CMInv <- x$getInv()
    if(!is.null(CMInv)){
        message("getting cached data")
        return(CMInv)
    }
   data <- x$get()
   CMInv <- solve(data,...)
   x$setInv(CMInv)
   CMInv
}
