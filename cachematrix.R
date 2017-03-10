## Nathan Engquist's function
## based on modifications to Richard Peng's cachemean.R and
## makeVector.R


## Basically all the "m"'s were changed to "inv" 
## and getmean() and setmean() became
## getinv() and setinv().  The word "mean"
## became the word "inverse" in the nameless function call
## that gets assigned to setinv()

## This function doesn't work in terms of actually
## creating an inverse matrix without the next function
## below it - cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL  
     set <- function(y) {
          x <<- y 
          inv <<- NULL 
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function returns the inverse of a square matrix by
## first checking to see if the inverse already exists
## in the cache.  If it exists, then the cache is simply
## returned.  If the inverse has not been calculated yet,
## then the function will use solve() to find the inverse.
## It should be noted that when I created a square 10 by
## 10 matrix and populated it with 1:100, I got an error
## because the matrix was singular.  Then I tested it with
## rnorm(100) and got no error.  I don't remember how to invert
## a matrix from Linear Algebra, but I know that not all
## matrices are invertable and I think that's what
## happened.

## cacheSolve() needs to have the variable that was assigned to makeCacheMatrix
## (i.e. a matrix itself with access to get(), set(), getinv(), and setinv())
## passed through it's argument in order to work properly.  If that same
## matrix variable gets passed twice in a row, the second call should
## generate the "getting cached data" message along with the inverted
## matrix being printed out to the screen.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
