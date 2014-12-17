##  Matrix inversion is usually a costly computation and makeCacheMatrix & cacheSolve 
##  a pair of functions that cache the inverse of a matrix rather than compute it repeatedly.


##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#1  x=matrix()is a formal argument and has default value of null matrix 
#1  when we will value of said formal through "makeCacheMatrix"("Matrix name"); 
#1  makeCacheMatrix will work on that formal argument.
makeCacheMatrix<-function(x=matrix()){
  
  
  #2  "m" is local variable which is assigned NULL value.
  m<-NULL
  
  #3  "set"is funtion within "makeCacheMatrix" function which takes formal argument
  #3  which is different from formal argument of "makeCacheMatrix" function.
  #3  -----not understood-------why "set" function is used
  #4  "x"&"m"  are made variables which can be used in other environments as well.
  set<-function(y){
    x<<-y
    m<<-NULL    
  }
  
  #5  "get"function does not take any formal arguments and return value of formal argument "x"
  #5  which can be checked using "y$get()" here "y" is used because result returned by 
  #5  "makeCacheMatrix" is stored in "y". Refer the list expression of "makeCacheMatrix" fuction.
  get<-function()x
  
  #6  "setsolve" function -----not understood-------why it is used.---
  #6  this function will be used in following statment "y$setsolve(m)" 
  #6  which is second last statement in "cacheSolve" function
  setsolve<-function(solve)m<<-solve
  
  #7  "getsolve" function does not take any formal arguments and return value of formal argument "m"
  #7  which can be checked using "y$getsolve()" here "y" is used because result returned by 
  #7  "makeCacheMatrix" is stored in "y". Refer the list expression of "makeCacheMatrix" fuction.
  #7  till the time we have not used "cacheSolve" function, "y$getsolve()" will return NULL.
  #8  Once we have used "cacheSolve" function, "y$getsolve()" will return "solve value"  
  #8  calculated by "cacheSolve" function.
  getsolve<-function()m
  
  
  #9  assign the value of list in variable "y". 
  #9  if we want to use variable "y" in global environment then 
  #9  we have to use "<<-"  assignment operator.
  #9  ulternatively we can use "y<-makeCacheMatrix(x)"insted of "makeCacheMatrix(x)"
  #9  and we get the list stored in variable y and 
  #9  thereby variable "y" will be created in global environment
  y<<-list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)
}


##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.


#10 once we have used makeCacheMatrix(x) and stored list of functions in variale "y"
#10 we can use that variable "y" with cacheSolve(y). 
cacheSolve<-function(y,...){
  
  #11 when we first time use the cacheSolve(y); then because "y$getsolve()" is NULL 
  #11 so variable "m" will also be NULL
  #11-1 when we second time used the cacheSolve(y); then because "y$getsolve()" has the inverse matrix
  #11-1 so variable "m" will be assigned that inverse matrix.
  #11-2 Because cacheSolve(y) takes formal argument "y" and use only "y$getsolve", 
  #11-3 so variable "m" can not take value otherthen from 
  #11-3 second element "y$getsolve" of list output of "makeCacheMatrix" fuction.
  #11-4 but how to control program to use "makeCacheMatrix" fuction for vactor having same value.
  
  m<-y$getsolve()
  #12 and thereby in the fist time, the logical test will be failed and 
  if(!is.null(m)){message("getting cached data...")
                  return(m)}
  #13 "data" variable will be get value from "y$get()
  data<-y$get()
  
  #14 maean of "data" will be calculated and stored in variable "m"
  m<-solve(data,...)
  
  #15 inverse matrix will be used as formal argument in y$setsolve() 
  #15 which is  "the third function used in "makeCacheMatrix"function. 
  #15 and assign the inverse matrix to y$getsolve()
  y$setsolve(m)
  m
}