## The following functions allow a matrix to be inverted and then
## cached, if the inverse matrix is already cached, it will not
##be inverted again

## This function creates the cached matrix
makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<- function(y) {
        x<<- y
        m<<- NULL
    }
    get <-function()x
    setinvert<-function(solve) m<<-solve
    getinvert<-function()m
    list(set = set, get= get,
         setinvert = setinvert,
         getinvert = getinvert)
}
## This function inverts the matrix or pulls from cache
cacheSolve <- function(x, ...) {
    m<-x$getinvert()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinvert(m)
    m
    }## Return a matrix that is the inverse of 'x'