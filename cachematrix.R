## Function makeCacheMatrix, to create a squared Matrix.
## The function creates a CacheMatrix.

## makeCacheMatrix sets a Matrix, that later can be gotten.

makeCacheMatrix <- function(x = matrix()) {
             m<-NULL
             set<-function(y){
                         x<<-y
                         m<<-NULL
                     }
             get<-function() x
             setmatrix<-function(solve) m<<- solve
             getmatrix<-function() m
             list(set=set, get=get,
                             setmatrix=setmatrix,
                             getmatrix=getmatrix)
}


## The function created previously, creates a Matrix and returns the inverse of it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
             if(!is.null(m)){
                         message("getting cached data")
                         return(m)
                     }
             matrix<-x$get()
             m<-solve(matrix, ...)
             x$setmatrix(m)
             m
}
