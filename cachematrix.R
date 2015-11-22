#Goal: Matrix inversion is usually a costly computation and 
#there may be some benefit to caching the inverse of a matrix rather than 
#compute it repeatedly (there are also alternatives to matrix inversion 
#that we will not discuss here). Your assignment is to write a pair of 
#functions that cache the inverse of a matrix.

#Write the following functions:
  
#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

## Put comments here that give an overall description of what your
## functions do>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## makeCacheMatrix makes a inverse matrix.
## The function will cache after calculate the inverse matrix optimately.
## When there is no cached inverse, it will return saved cache info
## >>>>>>>>>>>>>>>>>>>>>>>>

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # NULL Not a value, create a local scope inv variable in scope 
              # of the matrix
  set <- function(new) { #seting the inverse and matrix x
    x <<- new #change the matrix obj MakeCacheMatrix return value using set FUN to 
              #reassign the matrix x
    inv <<- NULL #reset the inv so it will new inv of the new matrix
  }
  get <- function() {
    return (x)
  } #x matrix stored in cache matrix obj, declare get function to return matrix x
  setinverse<- function(newinv) {
    inv <<-newinv #Pattern of set function- superassigning newinv matrix to original obj inv
  }
  getinverse <- function() {
    return (inv) #Set and get pairing patter, now get the inv value
  } 
  
  return (list (set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)#for list built in function 
                                      #function must equal its own function to return
                                      #an obj (in this case a list of functions - set, get
                                      #setinverse, getinverse)
  )
}
####################### TESTING makeCacheMatrix #########################################
######################declare a matrix ##################################################

b = matrix( 
  c(2, 4, 3, 1, 5, 7), 
    nrow=3, 
    ncol=2) 
h <- makeCacheMatrix (b)
h$get() #getting the matrix b
############################End of TEST ################################################

## Write a short comment describing this function: cachesolve returns inverse of matrix x
## from the earlier function makeCacheMatrix
## cachesolve fun if invmatrix is NULL then calc >>cache the return value 
## to set inside of cache matrix obj

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() #$getinverse $sign is to see if there cached inv value exist
                        #this gets the cached inverse value
  if (is.null(inv)) {   #if stmt to check if cache iv is NULL
    inv <- solve(x$get()) #if NULL get matrix & solve it, $sign makes R go inside of declared
                          #obj (get function from earlier function) not in global envmt
    x$setinverse(inv)   #sets inverse matrix of the object from declared setinverse function
    return(inv)          
  } 
  else {

    return(inv)  #if inv is not NULL then don't calculate invers
  }
}
####################### TESTING makeCacheMatrix #########################################
######################declare a matrix ##################################################

z = matrix( 
  c(4, 3, 3, 2), 
  nrow=2, 
  ncol=2) 
d <- makeCacheMatrix (z) #cach matrix
q <- cacheSolve (d) #new inverse

d$get() #getting the matrix 
q2 <- cacheSolve(d) #running cacheSolve second time to see if it remembers its result
                    #look inside table of q and q2.  They are equal 

############################End of TEST ################################################
