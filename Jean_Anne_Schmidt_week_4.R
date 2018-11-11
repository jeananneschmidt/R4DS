#20.3.5 #1-4

typeof(letters)

x <- c(0, NA, NaN, Inf, -Inf)

is.finite(x) #considers non-missing numeric values to be finite
!is.finite(x) #does the opposite

dplyr::near
#it checks to see if 2 numbers are within a certain tolerance defined by ".Machine$double..."
?integer 
#Note that current implementations of R use 32-bit integers for integer vectors, 
#so the range of representable integers is restricted to about +/-2*10^9: doubles can hold 
#much larger integers exactly.
?double
#For double vectors, R uses a 64-bit representation. This means that they can hold
#up to \(2^{64}\) values exactly. 

#four functions that convert double to integer
#convert doubles to integers by rounding
x <- seq(-10, 10, by = 0.5)
x
(round(x))

#use the as.integer function
(as.integer(x))

#ceiling
(ceiling(x))

#trunc to truncate the values towards 0
(trunc(x))



#20.4.6 #1-6

install.packages("purrr")
library(purrr)
#1
#mean(is.na(x)) tells you the proportion of missing values
#sum(!is.infinite(x)) tells you the number of items that are not infinite

#2
#is.vector returns TRUE if x is a vector of the specified mode having no 
#attributes other than names. It returns FALSE otherwise.

#is.atomic returns TRUE if x is of an atomic type (or NULL) and FALSE otherwise.
x <- c(1, NA, 2, 3, 4, Inf, -Inf)

is.vector(x)
is.vector(x, "integer")

y <- c(1:10)
is.vector(y, "integer")

z <- c(TRUE, FALSE, TRUE, TRUE)
is.vector(z, "logical")

a <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
is.vector(a)  # returns true
is.atomic(a)  # returns false

#Compare and contrast setNames() with purrr::set_names()
#The function set_names() will name an object with itself if no nm argument is provided 
#(the opposite of setNames() behavior).
#in purr::set_names objects can be named in the same was as setNames or as unnamed arguments
purrr::set_names(1:4, c("a", "b", "c", "d"))
# or
purrr::set_names(1:4, "a", "b", "c", "d")
#The set_names() function also checks that the length of the names argument is the same length 
#as the vector that is being named, and will raise an error if it is not. The setNames() 
#function will allow the names to be shorter than the vector being named, and will set the 
#missing names to NA.

#Last value function
#The function uses [[ in order to extract a single element.
last_value <- function(x) {
  # check for case with no length
  if (length(x)) {
    x[[length(x)]]  
  } else {
    x
  }
}
last_value(x)
last_value(1:10)

#Return elements at even number position
even_indices <- function(x) {
  if (length(x)) {
    x[seq_along(x) %% 2 == 0]
  } else {
    x
  }  
}
even_indices(1:10)
even_indices(letters)

#every element except last
not_last <- function(x) {
  n <- length(x)
  if (n) {
    x[-n]
  } else {
    # n == 0
    x
  }
}
not_last(1:3)


#only even numbers
even_numbers <- function(x) {
  x[x %% 2 == 0]
}
even_numbers(1:10)

# 5. Why is x[-which(x > 0)] not the same as x[x <= 0]?
x <- c(-5, -1, 0, 3, 8, NA, Inf, -Inf, NaN, 0/0)
x[-which(x > 0)]
x[x <= 0] #returns NA because you cannot compare "not a number" to a number

# 6. What happens when you subset with a positive integer that's bigger than 
#the length of the vector? What happens when you subset with a name that doesn't exist?

x[11] #returns NA
set_names(x, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

x[11] #returns NA

#20.5.4 1-2
(a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5)))

(a$a)
a$c

#Draw the following lists as nested sets:
#list(a, b, list(c, d), list(e, f))
  # ----------
  
  # | a      |
  
  # | b      |
  
  # | [c, d] |
  
  # | [d, e] |
  
  # ----------

#list(list(list(list(list(list(a))))))

# [[[[[[a]]]]]]


#Subsetting a tibble works the same way as a list; a data frame can be thought of as a list
#of columns. The key difference between a list and a tibble is that all the elements 
#(columns) of a tibble must have the same length (number of rows). 

#21.2.1 1-4

mtcars

mtmeans <- vector("double", ncol(mtcars))  # 1. output
for (i in seq_along(mtcars)) {            # 2. sequence
  mtmeans[[i]] <- mean(mtcars[[i]])      # 3. body
}
mtmeans


output <- vector("list", ncol(flights))
names(output) <- names(flights)
for (i in names(flights)) {
  output[[i]] <- class(flights[[i]])
}
output

data("iris")
iris_uniq <- vector("double", ncol(iris))
names(iris_uniq) <- names(iris)
for (i in names(iris)) {
  iris_uniq[i] <- length(unique(iris[[i]]))
}
iris_uniq

x <- 10
means <- c(-10, 0, 10, 100)

normals <- vector("list", length(means))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(10, mean = means[i])
}
normals

# 2. Eliminate the for loop in each of the following examples by taking
#    advantage of an existing function that works with vectors:
# String of letters

library(stringr)
str_c(letters, collapse = "") 

# Standard deviation
x <- sample(100)

(sd(x))

# Cumulative sum
cumsum(x)

#21.3.5 1-3
#1
setwd(users...)
hypothetical_files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
  df <- vector("list", length(hypothetical_files))
for (fname in seq_along(hypothetical_files)) {
  df[[i]] <- read_csv(hypothetical_files[[i]])
}
df <- bind_rows(df)

#2
library(stringr)
x <- c(1:5)
names(x)  # Returns null because no names
for (nm in names(x)) {
  print (nm)  # Doesn't reach this point because nm is empty
}

names(x) <- c( "a", "b", "c", "d")
for (nm in names(x)) {
  print (nm)  # Prints NA for the element without a name
}

names(x) <- c( "a", "b", "b", "d", "e")
for (nm in names(x)) {
  print (nm)  # Prints the name "b" twice because two items are named "b"
}

#3
show_mean <- function (df) {
    for (nm in names(df)) { 
      if (is.numeric(df[[nm]])) { 
        mean <- mean(df[[nm]])
        output <- str_c(nm, " is ", mean, sep = " ")
        print(output)
      }
    }
  }

show_mean(iris)

#21.5.3 #1
map_dbl(mtcars, mean)

map_chr(flights, typeof)

map_dbl(iris, ~ length(unique(.)))

map(c(-10, 0, 10, 100), ~ rnorm(n = 10, mean = .))
