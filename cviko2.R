library(Biostrings)
library("seqinr")

#Task2

IndexOfMin <- function(array, first, last) {
  # Step 1: Initialize index to the first position
  index <- first
  
  # Step 2: Loop from first + 1 to last
  for (k in (first + 1):last) {
    # Step 3: If the current element is smaller than the element at 'index'
    if (array[k] < array[index]) {
      # Step 4: Update 'index' to the current position
      index <- k
    }
  }
  
  # Step 5: Return the index of the smallest element
  return(index)
}


lst <- c(4, 2, 8, 1, 5)
first <- 1
last <- 4
IndexOfMin(lst, first, last)


SelectionSort <- function(integers, count){
  for (i in 1:(count-1)){
    j <- IndexOfMin(integers, i, count)
    tmp <- integers[i]
    integers[i] <- integers[j]
    integers[j] <- tmp
  }
  integers
}

SelectionSort(lst, 5)


RecursiveSelectionSort <- function(integers, first, last){
  if (first < last){
    index <- IndexOfMin(integers, first, last)
    tmp <- integers[first]
    integers[first] <- integers[index]
    integers[index] <- tmp
    first <- first +1
    integers <- RecursiveSelectionSort(integers, first, last)
  }
  integers
}

RecursiveSelectionSort(array, 1, 5)








