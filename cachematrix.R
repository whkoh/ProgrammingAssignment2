## Date:    19 January 2015
## Course:  R Programming 
##
## Testing procedures
## Step 1: q <- makeCacheMatrix()
## Step 2: q$set(matrix(1:9,3,3))
## Step 3: cacheSolve(q)
## This returns the inverse of the matrix and caches it.
## Step 4: cacheSolve(q)
## This returns the cached solution with a message 'Getting cached data!'
##
## makeCacheMatrix function description 
##
## makeCacheMatrix is a function that creates a 'matrix' object
## that can cache its inverse. Solving is not handled here.
## makeCacheMatrix saves the matrix to variable x and its inverse
## to m. The returned list contains: 
## set: sets matrix & resets cache of inverse
## get: returns matrix
## setsolution: saves solve value
## getsolution: returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolution <- function(solve) m <<- solve
  getsolution <- function() m
  list(set = set, get = get, 
         setsolution = setsolution, getsolution = getsolution
       )
}
## cacheSolve function description
##
## cacheSolve is a function to get inversed matrix from 
## the object created by the previous function. It takes
## the object as argument x, and checks if the inverse value is 
## already cached. If cached, it returns the cached value. If
## not cached, it calculates the inverse for the matrix in x
## and saves into the x cache using setsolution. The result
## is then returned.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getsolution()
  if(!is.null(m)) {
    message("Getting cached data!")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolution(m)
  m
}
