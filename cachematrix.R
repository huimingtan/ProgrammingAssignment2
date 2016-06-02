##calculate the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse_m <- function(inverse) inv_matrix <<- inverse
  getinverse_m <- function() inv_matrix
  list(set = set, get = get,
       setinverse_m = setinverse_m,
       getinverse_m = getinverse_m)
  

}


## invrse already calculated
## else, calculate inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse_m()
  if(!is.null(inv_matrix)){
    message("getting cached inversed matrix")
    return(inv_matrix)
  }else{
    data <- x$get()
    inverse_m <- mean (data, ...)
    x$setinverse_m(inv_matrix)
    inv_matrix
  }
  
}
