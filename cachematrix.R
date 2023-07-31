## This function creates a special "matrix" object 
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
         x <<- y
         invrs<<- NULL
}
  get <- function() x
  set_invrs <- function(inverse) x <<- inverse
  get_invrs <- function() x
  list(set = set, get = get,
       set_invrs = set_invrs,
       get_invrs = get_invrs)  
}
## The function below  computes the inverse of the special matrix returned by 
## the makeCacheMatrixfunction above. The cachesolve function
## should retrieve the inverse in case the inverse has already 
## been calculated and the matrix has not been changed


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <-x$get_invrs()
        
        # inverse has been calculated already
        if(!is.null(invrs)) {
              message ("obtaining cached data")
              return(invrs)
        }
        
        # inverse is calculated, if it has not been already
        invrs_matrix <- x$get()
        invrs <- mean(data, ...)
        x$set_invrs(invrs)
        return (invrs)
        
        
        }
