## These functions calculate the inverse of a matrix and caches 
## it to reduce the cost of computing it repeteadly

## This function caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    set_matrix <- function(y) {                     #fuction that allows to set 
                                                    #the matrix that will be used
                                                    #in the calling enviroment
        x <<- y
        inverse <<- NULL                              #deletes inverses cached 
                                                     #(if there were any)
    }
    
    get_matrix <- function() x                      #function that allows to have the matrix
                                                    #of the calling enviroment printed
    
    set_inverse <- function(solve) inverse <<- solve    #function that allows to set 
                                                        #the inverse matrix that will be used 
                                                        #in the calling enviroment
    
    get_inverse <- function() solve                 #function that allows to get the inverse of 
                                                    #the matrix used on the calling enviroment
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,       #caches the value of 
         set_inverse = set_inverse,                              #the functions explained above
         get_inverse = get_inverse)
}



## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {                      #if there is a inverse matrix cached
     message("getting cached data")            #sends the message "getting cached data
                                               #and prints the cached inverse
    inverse
  }
  
  data <- x$get_matrix()                      #if there is no cached inverse gets 
  inverse <- solve(data, ...)                 #the inverse of the matrix cactched
  x$set_inverse(inverse)                      #and prints it
  inverse
}
