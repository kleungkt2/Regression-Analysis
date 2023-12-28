## define variables in R
a=3
b=4
sqrt(a^2+b^2)


## Basic calculations
3+2
3-2
3*2
3/2
3^2
2^(-3)
100^(0.5)
sqrt(110)

pi    ## mathematical constants
exp(1)  ## natural number


## Getting help
?log
?sin
?rep
?lm




## install packages
install.packages("ggplot2")
library(ggplot)   ##load the package



## vector and create vector
x=c(1,2,3,4,5)
x=rep(2,10)
x=seq(1,10)
x=seq(from=1.5, to=4.2, by=0.1)

x=c(42,"Statistics", TRUE)    ## vector of multiple types
x=rep("A",times=10)

x=c(rep(seq(1,9,2),3),42,2:4)    ## concatenate several vectors
length(x)            ## the length of vector x



## subsetting of a vector
x=seq(1,10)
x[1]  ## the first entry in vector x
x[-2] ## returns all the entries except the 2nd entry
x[1:3] ## returns the first, 2nd and 3rd entries
x[c(1,3,5)] ## returns the 1st, 3rd and 5th entries



## operations on the whole vector
x=1:10
x+1 ## add 1 to every entry of x
2*x ## multiple every entry of x by 2
2^x ## exponential of every entry of x
sqrt(x) ## square root of every entry of x
log(x)  ## log of every entry of x


## logic operators
x=seq(1,9,by=2)
x>3 ## check whether the entries are larger than 3
x==3 ## check whether the entries are equal to 3
x==3 & x>2 
x==3 | x>2
x[x>3] ## returns the entries of x which are larger than 3
x[x!=3] ## returns the entries of x which are unequal to 3
sum(x>4) ## returns the number of entries larger than 4
as.numeric(x>4)  ## covert logic variables to binary ones
which(x>4) ## returns the indices of x whose entries are larger than 4
x[which(x>4)]
max(x)   ## the maximum entry of x
min(x)
which(x==max(x)) ## which entry is the maximum
which.max(x)

x=seq(3,11,by=2)
y=1:5
x+y    ## entrywise addition of two vectors




######### Matrices
X=matrix(1:9, nrow=3, ncol=3)  ## arragne a vector into a matrix, by default, column by column
X=matrix(1:9, nrow=3, ncol=3,byrow=TRUE)
Z=matrix(0,2,4) ## a 2 by 4 zero matrix
X[1,2]  ## the entry in the row 1 and column 2
X[1,]   ## the row 1 of X
X[,2]   ## the column 2 of X
X[c(1,3),]  ## the row 1 and row 3 of X
X[,c(2,3)]  ## the column 2 and column 3 of X

x=1:9
rev(1:9)   ## the reverse of a vector
rbind(1:9, rev(1:9),rep(3,9))    ## combine vectors as rows to create a amtrix
cbind(col_1 = x, col_2 = rev(x), col_3 = rep(1, 9))  ## combine vectors as columns to create a matrix



##### matrix algebra
x=1:9
y=9:1
X=matrix(x,3,3)
Y=matrix(y,3,3)
X+Y   ## entry-wise addition of matrices
X-Y   ## entry-wise subtraction
X*Y   ## entry-wise multiplication of matrices
X/Y   ## entry-wise division of matrices
X %*% Y  ## the usual matrix multiplication
t(X)     ## the transpose of a matrix

Z= matrix(c(9, 2, -3, 2, 4, -2, -3, -2, 16), 3, byrow = TRUE)
solve(Z)   ## the inverse of Z
diag(4)    ## an 4x4 identity matrix
dim(Z)     ## the dimension of a matrix
rowSums(Z)
colSums(Z)
rowMeans(Z)
colMenas(Z)
diag(Z) ## the diagonal entries of a matrix








###   Data strucutre 1:  Lists
# A list is a one-dimensional heterogeneous data structure. So it is indexed like a vector with a single integer value, but each element can contain an element of any type.
list(42, "Hello", TRUE)
ex_list = list(
  a = c(1, 2, 3, 4),
  b = TRUE,
  c = "Hello!",
  d = function(arg = 42) {print("Hello World!")},
  e = diag(5)
)
ex_list[[1]]   ## the first element in the list, which is a vector
ex_list$a      ## the element a in the list, which is also the first element
ex_list[1:2]   ## the first and second element in the list, which is a vector and a logic variable
ex_list[c("e","a")]
ex_list$d(arg=1)  ## the element d is a function, this runs the function




### Data structure 2: Data Frames
# Data frame is the most common way that we store and interact with data.
example_data = data.frame(x = c(1, 3, 5, 7, 9, 1, 3, 5, 7, 9), y = c(rep("Hello", 9), "Goodbye"),
                          z = rep(c(TRUE, FALSE), 5))
# Unlike a matrix, which can be thought of as a vector rearranged into rows and columns, a data frame is not required to have the same data type for each element. A data frame is a list of vectors. 
# So, each vector must contain the same data type, but the different vectors can store different data types.
# Data Frame can be viewed as a generalization of matrix
example_data$x
dim(example_data)  ## dimension of the data frame, number of rows and number of columns
nrow(example_data)
ncol(example_data)

## read csv file into R as a data frame
library(readr)  ## needs this library to read csv file
example_data_from_csv = read_csv("example-data.csv")
names(example_data_from_csv)   ## returns the column names
head(example_data_from_csv, n=5)  ## returns the first 5 rows



############# load data from installed packages
library(ggplot2)    ## load the library first, this library has th data package mpg
mpg  ## show the data
summary(mpg) ## summary of this data frame
names(mpg) ## returns the column names
head(mpg)  ## returns the first 10 rows
?mpg ## get information of this data frame
mpg$year ## the data of the variable year, or just the column of the variable year
dim(mpg)

# Subsetting data frames can work much like subsetting matrices using square brackets, [,]. 
# Here, we find fuel efficient vehicles earning over 35 miles per gallon and only display manufacturer, model 
# and year.
mpg[mpg$hwy > 35, c("manufacturer", "model", "year")]
# An alternative would be to use the subset() function, which has a much more readable syntax.
sub_mpg <- subset(mpg, subset = hwy > 35, select = c("manufacturer", "model", "year"))




#####  Control Flow 1: if and else
x = 1
y = 3
if (x > y) {
  z = x * y
  print("x is larger than y")
} else {
  z = x + 5 * y
  print("x is less than or equal to y")
}

# R also has a special function ifelse() which is very useful. It returns one of two specified values based on a conditional statement.
ifelse(4 > 3, 1, 0)
fib = c(1, 1, 2, 3, 5, 8, 13, 21)
ifelse(fib > 6, "Foo", "Bar")  ## ifelse can be applied entry-wisely to a vector


# for loop
x = 11:15
for (i in 1:5) {
  x[i] = x[i] * 2
}







#####  define a function
standardize = function(x) {
  m = mean(x)              ## the sample mean
  std = sd(x)              ## the sample standard deviation
  result = (x - m) / std
  result
}
test_sample = rnorm(n = 10, mean = 2, sd = 5)   ## generate a random sample of normal random variables of size 10 with mean 2 and variance 25
standardize(x = test_sample)                    ## standardize the random sample



## When specifying arguments, you can provide default arguments.
power_of_num = function(num, power = 2) {
  num ^ power
}
power_of_num(10)
power_of_num(num = 10, power = 2)
power_of_num(power = 2, num = 10)



## define a function to calculate the sample variance
get_var = function(x, biased = FALSE) {
  n = length(x) - 1 * !biased
  (1 / n) * sum((x - mean(x)) ^ 2)
}
get_var(test_sample)
get_var(test_sample, biased = FALSE)
get_var(test_sample, biased = TRUE)




















