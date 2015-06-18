## These function permit us to handle the matrix caching asociayted with
## the inverse matrix computation using the environment capabi;lities of R
##
##  makeCacheMatrix: Generate the function structure to save the inverse matriz
##  cacheSolce: Recover the inverse matrix in cache if it is exist, in other 
##                    case calculate it 
##   
##   Author:  Jose Mendez
##   Date:    18 jun 2015
##   version: 1.2

## makeCacheMatrix:
## This Function Generate the internal function structure to establish the cash in memory  
## to save in this case a inverse matrix, defining relevante actions via the following functions:
##           setsolve, getsolve, get and set functions 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

##  cacheSolve:
##  Througth this function you can recuperate from the environment
##  the matriz saved in the function structure created by the makecacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## Test it


m1=matrix(c(7,2,3,1,5,6,7,8,9),nrow=3,ncol=3)
m2=matrix(c(1,2,3,0,5,1,7,2,9),nrow=3,ncol=3)
m3=matrix(c(4,2,3,1,5,6,0,8,9),nrow=3,ncol=3)

cm1=makeCacheMatrix(m1)
cm2=makeCacheMatrix(m2)
cm3=makeCacheMatrix(m3)

startt=Sys.time()
scm1=cacheSolve(cm1)
scm1
stopt=Sys.time()
dift=stopt-startt
dift

startt=Sys.time()
sm2=cacheSolve(cm2)
sm2
stopt=Sys.time()
dift=stopt-startt
dift

startt=Sys.time()
cacheSolve(cm3)
stopt=Sys.time()
dift=stopt-startt
dift