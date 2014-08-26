#===================================================================================================
#              Coursera Specialization: 
#              Data Science - John Hopkins Universtity
#---------------------------------------------------------------------------------------------------
# Course:      rprog-006 - R Programming   
# Assignment:  PROJECT 2 - CACHING THE INVERSE OF A MATRIX
# Date:        Aug 23, 2014.
# 
# Objective:   Based on a set of example functions provided:
#              To write a pair of functions that cache the inverse of a matrix.
#              Write the following functions:
#              1. makeCacheMatrix: This function creates a special "matrix" object 
#              that can cache its inverse.
#              2. cacheSolve: This function computes the inverse of the special "matrix" 
#              returned by makeCacheMatrix above.
#
# System:      ACER - Aspire 7250
#              AMD E-350 Processor 1.60 GHz
#              Windows 7 - 64 bit
#              R 3.1.1
#              RStudio 0.98.994
#
#
# Student:     Francisco Jaramillo 
#---------------------------------------------------------------------------------------------------
# 2014.08.25 - FJ - Update of original file cachematrix.R. 
#                   Requirements specified computing "inverse" of a matrix.
#                   Original submission was programmed to calculate "transposed" matrix.
#                   Somehow the programmer read "inverse" and tought "transpossing".
#                   It was determined the programmer needed another cup of coffee.
#===================================================================================================


makeCacheMatrix <- function(x = matrix()){
     ## This function creates a special "matrix" object that can cache its inverse.     
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(i) i <<- i
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'     
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}