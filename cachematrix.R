## Put comments here that give an overall description of what your
## functions do
## Write 2 functions
## 1.makeCacheMatrix creates an vector (a list of function) able to store a matrix and its reverse
## 2.cacheSolve compute the reverse of a matrix stored in a CacheMatrix vector. it first checks
##	if this was already computed and return the value from the cache. 
## both function are explained below with an example how to use them.


## makeCacheMatrix:
## this function is creating a pseudo vector of function 
## allowing to store and get the source matrix, as well as store and get the reverse matrix.
## there is no calculation logic in this function. 
## furthermre, the object created with this function provide the ability to store in memory the reverse matrix
## usage: 
## 	for a matrix, e.g.
##	> MyMatrix <- matrix(0:3, 2,2)
## 	> myCacheMatrix <- makeCacheMatrix(MyMatrix)


makeCacheMatrix <- function(x = matrix()) {
        ## define the matrix variable which may be cached.
	  m <- NULL
	  ## define the Set function which will store
	  ## the value of the original matrix	
        ## if a matrix is stored, the cache needs to be reset
	  ## note the cache is created in the environment of the calling function, 
 	  ## i.e., in the env of makeCacheMatrix itself
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	  ## return the matrix itself
        get <- function() x
	  ## Set the reverse matrix into the variable m (the cache). 
        setreverse <- function(reverse) m <<- reverse
        ## Get the reverse from the cache (return null in case it was not set)
        getreverse <- function() m


        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)


}


## cacheSolve
## This function will compute the reverse of a matrix stored within an object created with makeCacheMatrix
## before computing it will first check whether or not the reverse was already computed and stored.
## if stored it will return the computed reversed matrix, otherwise it will call the function "solve" and store the result in the cache. 
## usage: 
## > cacheSolve(myCacheMatrix)
## where myCacheMatrix is created with the function makeCacheMatrix. This will return
##    [,1] [,2]
##[1,] -1.5    1
##[2,]  0.5    0
## reruning the same calculation will take it from the cache: 
## > cacheSolve(myCacheMatrix)
## getting cached data
##      [,1] [,2]
## [1,] -1.5    1
## [2,]  0.5    0



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getreverse() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setreverse(m)
        m



}
