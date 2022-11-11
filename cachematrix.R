## The objective behind writing these two functions is to cache 
## the inverse of a matrix. Caching the inverse of a matrix is preferrable 
## over computing it again and again.

## makeCacheMatrix function creates a special matrix object that can cache 
## its inverse

install.packages("matlib")
library(matlib)

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set<- function(y){
			x<<-y
			i<<-NULL
	}
		get<-function()x
		setinverse<-function (inverse)i<<-inverse
		getinverse<-function ()i

		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve fuction computes the inverse of the matrix returned by 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
      	i <- x$getinverse()
		if (!is.null(i)){
			message("getting cached data")
			return (i)
	}

	data<-x$get()
	i<-solve(data,...)
	x$setinverse(i)
	i

}	
