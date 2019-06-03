

## Put comments here that give an overall description of what your
## functions do

## In the makeCacheMatrix function, the x argument is the matrix that is input into the environment. 
## M is set to null so that it clears the way for new cached matrices. 
## The set function mutates the data within the parent environment 
## The get function retrieves the data from the parent environment because it is not defined. 
## and <<- forces the function to look in the parent environment as well. 
## The setmatrix and getsolve vectors define who the gett-ed and setted, taking advantage of lexical scoping  
## the list function assigns each function as an element and returns it to the parent environment. 
## This allows you to us '$' to access the functions 


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x 
  setmatrix <- function(matrix) m <<- matrix
  getsolve <- function() m
  list(set = set, get = get, 
       setmatrix = setmatrix, 
       getsolve = getsolve)

}


## cacheSolve first attempts to retrieve the matrix from the cache. 
## If it exists, then the value is pulled from it. 
## If it does not, the inverse matrix is calculated and set into the parent environment. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() ##attempts to retrieve the matrix
  if(!is.null(m)) { ##if the value is not equal to null then it pulls the cache 
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setmatrix(m)
  m
  
}

aMatrix <- makeCacheMatrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
