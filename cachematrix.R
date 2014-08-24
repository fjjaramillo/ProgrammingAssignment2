
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
#===================================================================================================


makeCacheMatrix <- function(x = matrix()){
     ## This function creates a special "matrix" object that can cache its inverse.     
     t <- NULL
     set <- function(y) {
          x <<- y
          t <<- NULL
     }
     get <- function() x
     settranspose <- function(t) t <<- t
     gettranspose <- function() t
     list(set = set, get = get,
          settranspose = settranspose,
          gettranspose = gettranspose)
}



cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'     
     t <- x$gettranspose()
     if(!is.null(t)) {
          message("getting cached data")
          return(t)
     }
     data <- x$get()
     t <- t(data, ...)
     x$settranspose(t)
     t
}
