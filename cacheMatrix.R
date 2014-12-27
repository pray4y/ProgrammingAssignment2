## The first function will cache the inverse of an n by n matrix 'z', 
## while the second function will compute the inverse of matrix 'z'.


## makeCacheMatrix, similar to cachemean, will creates a a list containing a 
## function to set/get the matrix and set/get its inverse matrix.

makeCacheMatrix <- function(z = matrix()) {
        m <- NULL    
        set <- function(y) {
                z <<- y
                m <<- NULL
        }
        get <- function() {
                z
        }
        setmat <- function(mat) {
                m <<- mat
        }
        getmat <- function() {
                m
        }
        list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## cacheSolve first detects whether the the inverse of a given n by n invertible
## matrix 'z' has been calculated. If the inverse has already been calculated, 
## cacheSolve will get the cached inverse and print it. If the inverse has not 
## already been calculated, cacheSolve will then calculate it and print it.

cacheSolve <- function(z, ...) {
        m <- z$getmat()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        x <- z$get()
        
        n <- nrow(x)
        id <- diag(nrow = n) ## create an n by n identity matrix
        xid <- cbind(x, id)

        for (i in 1:n) {
                for (j in (1:n)[-i]) {
                    
                    ## if xid[i, i] is a zero, then use row calculations
                    ## to remove zero
                    k <- 1 
                    while (k <= n) {
                            if (identical(0, xid[i, i]) == TRUE) {
                                    if (i + k <= n) {
                                            xid[i,] <- xid[i,] + xid[i+k,]
                                            k <- k + 1
                                    } else {
                                            k <- n + 1
                                    }  
                            } else {
                                    k <- n + 1    
                            } 
                    }
                    
                    xid[i,] <- xid[i,] / xid[i, i]
                    
                    ## if xid[j, i] is a zero, then use row calculations
                    ## to remove zero
                    k <- 1
                    while (k <= n) {              
                            if (identical(0, xid[j, i]) == TRUE) {
                                    if (i + k <= n) {
                                            xid[j,] <- xid[j,] + xid[k,]
                                            k <- k + 1
                                    } else {
                                            k <- n + 1
                                    }  
                            } else {
                                    k <- n + 1    
                            } 
                    }
                    
                    xid[j,] <- xid[j,] - xid[i,] * xid[j, i]
                }
        }
        m <- xid[, 4:6]
        z$setmat(m)
        m
}
