## Similar to the provided cache mean function, makeCacheMatrix do the following
## set value of matrix
## get value of matrix
## set its inverse matrix
## get its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y){
		x<<-y
		xinv<<-NULL
	}
	get <- function() x
	setinv<-function(v){
		xinv<<-v
	}
	getinv<-function()xinv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve checks whether inverse matrix is calculated. If not, solve matrix
## and store the value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	xinv <- x$getinv()
	if(!is.null(xinv)){
		message("getting cached inverse matrix")
		return(xinv)
	}
	m<-x$get()
	xinv<-solve(m)
	x$setinv(xinv)
	xinv
}
