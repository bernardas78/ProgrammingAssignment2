## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## returns a list of 4 functions:
##  get - returns the matrix
##  set - updates matrice's data; invalidates the inverse
##  setInv - sets the inverse matrice's data
##  getInv - returns an inverse matrix (which must have been set before)


makeCacheMatrix <- function(mtr = matrix()) {
    inv_mtr <- NULL
    
    get<-function() {mtr}
    
    set<-function(mtr_param){
        mtr<<-mtr_param
        inv_mtr<<-NULL
    }
    
    setInverse<-function(inv_mtr_param){
        inv_mtr<<-inv_mtr_param
    }
    
    getInverse<-function() inv_mtr
    
    list(get=get,set=set,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve - produces an inverse of a matrix; prints a message if from cache
## assumption: matrix is square and invertible 
## sample usage:
##   mtr_cached <- makeCacheMatrix()
##
##   mtr <- matrix(rnorm(9),3,3)
##   mtr_cached$set(mtr)
##   cacheSolve(mtr_cached)
##
##   ##calling the second time should produce a message "from cache"
##   cacheSolve(mtr_cached)
##
##   ##after calling set() function, message "from cache" should not appear
##   mtr_cached$set(mtr)
##   cacheSolve(mtr_cached)

cacheSolve <- function(x, ...) {
        inv_mtr <- x$getInverse()
        
        if (is.null(inv_mtr)){
            inv_mtr <- solve(x$get())
            x$setInverse(inv_mtr)
            ##retrieve just to make sure it is set
            inv_mtr <- x$getInverse()
        }
        else
            message("from cache")
        
        inv_mtr
}
