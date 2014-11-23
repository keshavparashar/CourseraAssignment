## The functions makeCacheMatrix and cacheSolve take a matrix as an argument and 
##  compute the inverse of that matrix and cache it

matt <- c(1:4)
dim(matt)<-c(2,2)

## makeCacheMatrix function stores the matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) { 
  
  m <- NULL     #  m is the inverse of the matrix and will be reset to NULL 
  
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }   # this function returns the original matrix
  
  setinverse <- function(inverse)  { m <<- inverse }
  # this is called by cacheinverse() during the first cacheinverse()
  #  access and it will store the value using superassignment
  
  getinverse <- function() { m } # this will return the cached value to cacheinverse() on
  #  subsequent accesses
  
  list(get = get, set = set,           
       setinverse = setinverse,  
       getinverse = getinverse) 
}


## cacheSolve function will compute or retrieve the inverse of the matrix depending on
## whether the value has been previously cached
cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  if(ncol(matt)!=nrow(matt)){message("inverse cannot be computed for non-square matrices")}
    
  m <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
  if(!is.null(m)) {              # if inverse was already cached (not NULL) ...
    
    message("getting cached data")  
    return(m)                       # returns the inverse matrix
    
  }
  data <- x$get()       # if the inverse matrix wasn't cached we reach this part
  m <- solve(data)   
  x$setinverse(m)           # store the calculated inverse matrix in x 
  m               # return the inverse matrix
}