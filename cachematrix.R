##MakeCacheMatrix consists of four functions 1.create_matrix(cmat) 2.get_matrix(gmat) 3.set_inverse(sinv) 4.get_inverse(ginv)
## cmat stores the input(values entered) in an variable
## gmat converts the input (if its not by type matrix) and prints it in the matrix form ( row filling)
## sinv stores the argument passed into this function as the inverse in a variable.( By default the value of inverse is null)
## ginv converts the argument passed into sinv into matrix form if its possible ...otherwise it returns null by default


#cacheSolve function gets input from the object of makeCacheMatrix function and returns inverse if it already exists 
##otherwise computes an inverse for the matrix and returns it


## makeCacheMatrix function creates a matrix ( even if you don't input a matrix ) and sets in an inverse if given(stores whateve the user gives as inverse)

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
                                           
                                           create_matrix<-function(y){
                                             x<<-y
                                             inv<<-NULL
                                             
                                           }
                                           get_matrix<-function(){
                                             if(is.matrix(x)){x}
                                             else {
                                               dim(x)<-c(sqrt(length(x)),sqrt(length(x)))
                                               x}
                                           } 
                                           set_inverse<-function(inverse) inv<<-inverse
                                           get_inverse<-function() {
                                             if(length(inv)==0)
                                             {inv}
                                             else if (is.matrix(inv)){inv}
                                             else {
                                               dim(inv)<-c(sqrt(length(inv)),sqrt(length(inv)))
                                               inv
                                             }
                                             
                                           }
                                           
                                           list(cmat=create_matrix,gmat=get_matrix,sinv=set_inverse,ginv=get_inverse)
                                           
}


##cacheSolve function gets its input from makeCacheMatrix function and checks if an inverse has been set for the matrix.
##If it finds that an inverse has been set, it jus returns that value of the inverse without computing anything
##If no inverse has been set, this function computes the inverse for the matrix using the solve function and returns it


cacheSolve <- function(x, ...) {
  inv<-x$ginv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$gmat()
  inv<-solve(matrix) ##computes inverse
  inv
}
