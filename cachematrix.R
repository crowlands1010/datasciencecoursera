## This assignment is to write a pair of functions that will cache
## the inverse of a matrix. The purpose of this assignment is to
## fulfill the requirements set forth in the second assignment in 
## the R Programming course on Coursera -- Data Specialization --
## Johns Hopkins University

## Instructions from README.md (for convenience)
## 1.  Fork the GitHub repository containing the stub R files at
## [https://github.com/rdpeng/ProgrammingAssignment2](https://github.com/rdpeng/ProgrammingAssignment2)
## to create a copy under your own account. 
## 2.  Clone your forked GitHub repository to your computer so that you can
## edit the files locally on your own machine.
## 3.  Edit the R file contained in the git repository and place your
## solution in that file (please do not rename the file).
## 4.  Commit your completed R file into YOUR git repository and push your
## git branch to the GitHub repository under your account.
## 5.  Submit to Coursera the URL to your GitHub repository that contains
## the completed R code for the assignment.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
  }

  

      

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if(!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      data <- x$get()
      i <- inverse(data, ...)
      x$setInverse(i)
      i
}
