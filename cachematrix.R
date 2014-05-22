## Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
##Your assignment is to write a pair of functions that cache the inverse of a matrix.

##         Assume that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {
        ###        makeCacheMatrix: This function creates a special "matrix" object 
        ##         that can cache its inverse.
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
        
        cacheSolve <- function(x, ...) {
                ###        cacheSolve: This function computes the inverse of the special "matrix" returned 
                ##         by makeCacheMatrix above. If the inverse has already been calculated 
                ##         (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached inverse matrix")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }


##      Here is an example usage
##
##      > mm <- matrix(rnorm(16), nrow=4)
##      > yy<-makeCacheMatrix(mm)
##      > zz<-cacheSolve(yy)
##      > zz
##      [,1]        [,2]       [,3]        [,4]
##      [1,]  0.68921616 -0.05083364  0.6840792  0.19867264
##      [2,] -0.08971262 -2.09877710  0.3899548  0.96600575
##      [3,]  0.26721262 -1.05449020 -0.4017902  0.05620066
##      [4,] -0.05792911 -0.42022243  0.1600425 -0.18559464
##      > zz<-cacheSolve(yy)
##      getting cached inverse matrix
##      > mm
##      [,1]       [,2]       [,3]       [,4]
##      [1,] 0.959834266 -0.3852064  0.9904364 -0.6775819
##      [2,] 0.054194944 -0.1411338 -0.3572984 -0.7847719
##      [3,] 0.496985241  0.1998392 -0.9353463  1.2889172
##      [4,] 0.006263524  0.6121137 -0.3067191 -2.2882558
##      > round(mm %*% zz, 4)
##      [,1] [,2] [,3] [,4]
##      [1,]    1    0    0    0
##      [2,]    0    1    0    0
##      [3,]    0    0    1    0
##      [4,]    0    0    0    1
##      > source('~/Desktop/Coursera_R/ProgrammingAssignment2/cachematrix.R')
        



