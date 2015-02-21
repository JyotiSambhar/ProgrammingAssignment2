## Function will return the inverse of a given matrix
## Function returns matrix inverse from cache if it exists

## makeCacheMatrix 1)Set/Get the matrix 2)Set/Get the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  
  invx <- NULL
  set <- function(y) {
  x <<- y
  invx <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) invx <<- solve
  getmatrix <- function() invx
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  invx <- x$getmatrix()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setmatrix(invx)
  invx       
}
