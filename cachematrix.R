## All right, just as a reminder, this is what Dr. Peng wants us to accomplish:
## Write these two functions:
  ## 1. makeCacheMatrix: This function creates a special "matrix"
  ##  	                 object that can cache its inverse.
  ## 2. cacheSolve: This function computes the inverse of the special "matrix"
  ##		            returned by makeCacheMatrix above. 
  ##	            	If the inverse has already been calculated
  ##	             	(and the matrix has not changed),
  ##            		then the cachesolve should retrieve the inverse from the cache.

## Here is the first function - makeCacheMatrix. It creates a list of four elements:
## 'Subfunctions' that set a matrix's value, then get it, then set the value of inverted matrix
## & then get it as well. 

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(u) {
    x <<- 
    invert <<- NULL
  }
  get <- function() x
  setinvert <- function(inverse) invert <<- inverse
  getinvert <- function() invert
  list(set=set, get=get, setinvert=setinvert, getinvert=getinvert)
}

## If I'd run it, say, for a 2 & 10, 2x2 matrix:

x = rbind(c(2, 10), c(10, 2))
m = makeCacheMatrix(x)
m$get()

## The R program that we've learnt to love & hate by now, would return this:

[,1] [,2]
[1,]    2   10
[2,]   10    2

## It would probably do it, only after we've re-written the function 90 times. Or just 30,
## if we're very smart. I've learnt that I'm not so smart. 


## Now, after a run, shower, night clubbing... We are attempting to write the second function.
## It triest to give you an inverse of a matrix. It starts by checking if it's already inversed, if so, it returns it,
## if not it computes the inverse and then returns it. 

cacheSolve <- function(x, ...) {
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("already cached!")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data)
  x$setinverse(invert)
  invert
}

## If we run it, it'll first torture us with errors - for example: "Error in cacheSolve(u) : object 'u' not found", 
## and later, it'll give us better errors: "Error in cacheSolve(m) : attempt to apply non-function"
## Until finally it'll do the job and it'll look like something like that:

cacheSolve(m)
           [,1]      [,2]
 [1,] 1.0333333 0.2333333
 [2,] 0.2333333 1.0333333


        
