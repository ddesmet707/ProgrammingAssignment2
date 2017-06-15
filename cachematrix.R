## This script very much follows the structure of the vector function provided as an example. First what this script does is establishing an object that is, within the parent environment of the function, called x.  Then, it provides useful functions within the parent function that can be done on this object, allowing the user to set the matrix, get the matrix, set the inverse, and finally get the inverse.

## Start off establishing x

makeCacheMatrix <- function(x=numeric()) {
   inv <- NULL # initiate the inverse var
   set <- function(y){ # This sets "x" as an arbitrary input
      x <<- y
      inv <<- NULL # If the matrix has been newly supplied, then the previous inverse matrix should be removed lest it be accidentally supplied.
   }
   get <- function() x # This will provide the matrix currently in the function's data
   setinverse <- function(inverse) inv <<- inverse #This allows the inverse to be set to an arbitrary matrix.
   getinverse <- function() inv #This will tell you what the current inverse matrix is.
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse) # This allows every function to be called from outside the function's environment by the specific titles on the left of every statement.
}


## Once a matrix has been provided, this can be used to find the inverse.

cacheSolve <- function(x, ...) { #ellipsis so more than one argument can be supplied
   inv <- x$getinverse() # This refreshes what's currently in the memory for the inveres matrix for the function
   if(!is.null(inv)) {
      message("getting cached solution")
      return(inv) # If there's something stored already, this checks for that and the supplies that inverse matrix via return
   }
   data <- x$get() 
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
   # Otherwise, this function gets the supplied matrix to be solved, solves it, sets the function's inv variable to this solution, and then finally supplies the solution to the console.
}