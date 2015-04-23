## makeCacheMatrix
## EN: make cacheable matrix from normal matrix
## ID: buat matriks cacheable dari matrix normal

## cacheSolve
## EN: return inverse of matrix, if already have, take the old cache value
## ID: menghasilkan invers dari matrix, jika sudah ada, ambil nilai yg ter-cache

## ex:
## B = matrix(c(2, 4, 3, 1), nrow=2,ncol=2)
## A <- makeCacheMatrix(B)
## cacheSolve(A)

## assume that the matrix supplied is always invertible
## take argument of normal matrix, return a cacheable matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse  <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## take argument of cacheable matrix, return inverse result of matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}