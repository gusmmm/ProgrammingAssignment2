## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # ERROR MESSAGES
        #1 if matrix is not square
        if(nrow(x)!=ncol(x)) {stop("The matrix is not square")}
        #2 if matrix is all NA
        if(all(is.na(x))){stop("Cannot compute inverse of NA matrix")}
        #3 if matrix has determinant 0
        if(det(x)==0){stop("The matrix has a determinant of 0")}
        
        
        # creates a square matrix with all NA as a null matrix
        null_matrix <- function(x){
                nm <- matrix(NA,nrow(x),ncol(x))
                return(nm)
        }
        
        inv <- null_matrix(x)
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!all(is.na(inv))){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data,...)
        
        x$setinv(inv)
        
        return(inv)
        
}
