
makeCacheMatrix <- function(x = matrix()){
        
        ## init inverse property
        i <- NULL
        
        ## set the matrix x
        set <- function(m){
                x <<- m
                i <<- NULL
        }
        
        ## get the matrix x
        get <- function(){
                x               
        } 
        
        ## set the inverse matrix of x
        setInverse <- function(i) {
                I <<- i
        }
        
        ## get the inverse matrix of x
        getInverse <- function() {
                I
        }
        
        ## list of internal methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function( x, ...){
        ## Return a matrix that is the inverse of 'x'
        
        ## the inversion matrix IM of the matrix x
        InverseMatrix <- x$getInverse()
        
        ## calculation of the inversion matrix IM 
        if (is.null(InverseMatrix)) {
                message('Calculation the inverse...')
                
                data <- x$get()
                InverseMatrix <- solve(data, ...)
                x$setInverse(InverseMatrix)
        } else {
                message('Cached inverse...')
        }
        
        InverseMatrix
}
