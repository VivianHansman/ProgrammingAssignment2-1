## These two functions designed to cache the inverse of a given matrix in order to save
## time and cost in computing. 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
	set<-function(y){
		x<<-y
		inverse<<-NULL
	}
	get<-function() x
	setInverseMatrix<-function(solve) inverse<<-solve
	getInverseMatrix<-function() inverse
	list(set=set,get=get,setInverseMatrix=setInverseMatrix,
	     getInverseMatrix=getInverseMatrix)
}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       inverse<-x$getInverseMatrix()
	if(!is.null(inverse)){
		message("getting cachedd data")
		return(inverse)
	}
	data<-x$get()
	solve<-solve(data,...)
	x$setInverseMatrix(solve)
	solve
}
