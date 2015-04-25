
##in makeCacheMatrix:-
##set function sets values to the matrix and thereby set the inverse value to null
##get function is used to retrieve the matrix with its values
##setinverse function is used to set the inverse value to the matrix
##getinverse function is use to retrieve the inverse value of the matrix 

##in cacheSolve:- 
## I am accessing the getinverse function to retrieve the inverse of the matrix
##if the inverse is null then I am setting the inverse by using the solve function
## if inverse is not null then i am displaying it and returning from the function


##returns a list of functions to set a matrix, to setInverse of it, get the matrix and get the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  if(!is.matrix(x))   # if the argument passed is not a matrix coercing it to be a matrix
    x<-as.matrix(x)
  set<-function(x1=0,row=0,col=0){ 
    #to change the values of the matrix call this function, if no argument is passed
    # it creates a dummy matrix
    x<<-matrix(x1,row,col)
    inverse<<-NULL
  }
  setinverse<-function(i){
    inverse<<-i
    
  }
  getinverse<-function(){
    inverse
  }
  get<-function(){
    x
  }
  
  list("set"=set,"get"=get,"setInverse"=setinverse,"getInverse"=getinverse)

}


##returns the inverse of the matrix after checking whether the inverse exists in the cache or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  value<-x$getInverse()
  if(!(is.null(value))){
    message ("Displaying cached value...")
    return(value)
    
  }
  else {
    matrix1<-x$get()
    if(nrow(matrix1)==ncol(matrix1)){
      inv<-solve(matrix1)
      x$setInverse(inv)
      return (inv)
    }
    
    return(message("Inverse not possible."))
    
  }
}
