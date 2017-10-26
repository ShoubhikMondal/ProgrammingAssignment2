## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.
> makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
+ inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
+ set <- function(y) {                    ## define the set function to assign new 
+ x <<- y                             ## value of matrix in parent environment
+ inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
+ }
+ get <- function() x                     ## define the get fucntion - returns value of the matrix argument   
+ setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
+ getinverse <- function() inv                     ## gets the value of inv where called
+ list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
+                                                                                  ## to the functions with the $ operator
+ }
## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.
> cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
+ inv <- x$getinverse()
+ if(!is.null(inv)) {
+ message("getting cached data")
+ return(inv)
+ }
+ data <- x$get()
+ inv <- solve(data, ...)
+ x$setinverse(inv)
+ inv
+ }
