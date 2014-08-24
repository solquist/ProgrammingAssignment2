##
## Pair of functions 'makeCacheMatrix()' and 'cacheSolve()' that together can be
## used to cache the inverse of a matrix (calculated using 'solve()')
##
## Example usage:
##
## m <- matrix(c(1, 2, 3, 4), nrow = 2) ## create a 2x2 matrix
## m.cache <- makeCacheMatrix(m) ## create a 'makeCacheMatrix' object
## cacheSolve(m.cache) ## calculates the inverse and caches the first time
## cacheSolve(m.cache) ## subsequent calls will fetch from the cache
## 

## makeCacheMatrix(x): object with the ability to cache the inverse of 'x'
## Arguments:   x - square matrix
## Funtions:    set() - sets the matrix 'x' and clears the cache
##              get() - returns the matrix 'x'
##              setinverse() - caches the inverse
##              getinverse() - returns the cached inverse
## Returns:     list of 'set' and 'get' functions
## Assumptions: 'x' is a square, invertible matrix
##
## Note: makeCacheMatrix() does not calculate the inverse, but is the mechanism to
## cache the inverse with the input matrix. Use the function cacheSolve() to calculate
## and cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve(x): Returns the inverse of 'x' (caching the reults the first time)
## Arguments:   x   - Function list from a 'makeCacheMatrix()' call.
##              ... - Optional arguments to pass to 'solve()'. This matches
##                        'solve()' documentation with the exception of the
##                         right-hand side argument, which will always be set
##                         to the identity.
## Returns:     Inverse of the matrix, caching the inverse in 'x' if not
##              already cached. Message is printed when retrieving from cache.
## Assumptions: 'x' is the result of a 'makeCacheMatrix()' call.
##
## Note: We need to create the identity for the right-hand side, otherwise,
## we could pass the right-hand side as one of the optional arguments and end
## up with incorrect results because the right-hand side is not remembered. To
## not stray too far from the assignment, functions for cacheing a solution of a
## linear system are presented below to show how to have a more general solution
## than just inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    if(is.complex(a)) { ## handle complex numbers
        b <- diag(1 + (0 +0i), nrow(a))
    } else {
        b <- diag(1, nrow(a))    
    }
    inv <- solve(data, b, ...)
    x$setinverse(inv)
    inv
}

##
## Pair of functions 'makeCacheSystem()' and 'cacheSolveSystem()' that together 
## can be used to cache the solution of the linear system 'a %*% x = b'. If 'b'
## is not set, the solution will be the inverse (as with 'solve()').
##
## Example usage:
##
## a <- matrix(c(1, 2, 3, 4), nrow = 2) ## create a 2x2 matrix of coefficients
## b <- c(1, 1) ## right-hand side of the equation
## s.cache <- makeCacheSystem(a, b) ## create a 'makeCacheSystem' object
## cacheSolveSystem(s.cache) ## calculates the solution and caches the first time
## cacheSolveSystem(s.cache) ## subsequent calls will fetch from the cache
## 

## makeCacheSystem(a, b) - function with the ability to cache the solution of
##     the linear system 'a %*% x = b'.
## Arguments:   a   - coefficient matrix for linear system
##              b   - optional right-hand side vector or matrix for linear system
##              ... - Optional arguments to pass to 'solve()'. This matches
##                        'solve()' documentation with the exception of the
##                         right-hand side argument, which will always come from
##                         the 'makeCacheSystem' object.
### Funtions:   seta() - sets the coefficient matrix 'a' and clears the cache
##              setb() - sets the vector or matrix 'b' and clears the cache
##              geta() - returns the coefficient matrix 'a'
##              getb() - returns the right-hand side vector or matrix 'b'
##              setsolution() - caches the solution to the linear system
##              getsolution() - returns the solution to a linear system
## Returns:     list of 'set' and 'get' functions
## Assumptions: 'a' is a square, invertible matrix of coefficients and 'b' is
##               the right-hand side for a linear system.
##
## Note: 'makeCacheSystem()' does not calculate the solution, but is the
## mechanism to cache the solution with the inputs. Use the function
## 'cacheSolveSystem()' to calculate and cache the solution.

makeCacheSystem <- function(a = matrix(), b = NULL) {
    s <- NULL
    seta <- function(y) {
        a <<- y
        s <<- NULL
    }
    setb <- function(y) {
        b <<- y
        s <<- NULL
    }
    geta <- function() a
    getb <- function() { ## assumed to be identity if not set
        if(is.null(b)) {
            if(is.complex(a)) { ## handle complex numbers
                diag(1 + (0 +0i), nrow(a))
            } else {
                diag(1, nrow(a))    
            }
        } else {
            b
        }
    }
    setsolution <- function(solution) s <<- solution
    getsolution <- function() s
    list(seta = seta, geta = geta,
         setb = setb, getb = getb,
         setsolution = setsolution,
         getsolution = getsolution)
}

## cacheSolveSystem(s) - Returns the solution of 'a %*% x = b' (caching the
##     reults the first time). If 'b' is NULL, the solution will be the
##     inverse.
## Arguments:   s - Function list from a 'makeCacheSystem()' call.
## Returns:     Solution of the linear system 'a %*% x = b', caching the
##              solution in 's' if not already cached. Message is printed when
##              retrieving from cache. If b is not set, the solution is the
##              inverse of 'a'.
## Assumptions: 's' is the result of a makeCacheSystem() call.

cacheSolveSystem <- function(s, ...) {
    ## Return the solution to the linear system 'a %*% x = b' (inverse if 'b'
    ## is NULL)
    solution <- s$getsolution()
    if(!is.null(solution)) {
        message("getting cached data")
        return(solution)
    }
    data.a <- s$geta()
    data.b <- s$getb()
    solution <- solve(data.a, data.b, ...)
    s$setsolution(solution)
    solution
}