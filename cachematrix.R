## 1)get() is a function that returns the matrix stored in the main function.It does not 
## require any input.
## 2)set is a function that changes the matrix stored in the main function.
## 3)setinv and getinv are very similar to set() and get().They do not calculate the inverse 
## but they simply store the value of the input in variable m in the main function(setinv)
## and return it(getinv).

## makeCacheMatrix is a function which contains a list of 4 functions.

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinv <- function(inv) m <<- inv
getinv <- function() m
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)

}



## cacheSolve verifies if the value of m exists and is not NULL.If it exists then
## it returns the value.
## If it doesnt exist it extracts the matrix(x$get()),calulates the inverse and
## then sets the value(setinv)
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
