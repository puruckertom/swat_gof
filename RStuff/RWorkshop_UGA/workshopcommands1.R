# \\ works as /
setwd("/Users/katieprice/Dropbox/ktp/R_workshop/") #set wd, same as Rgui Misc -> set working directory
? persp
help(persp) #same as ? persp
?? log
help.search("log") #same as ?? log

#install and load package, detach
install.packages("MatchIt")
library("MatchIt")
detach(package:MatchIt)

# variable names should begin with letter or period. Var names may include letters, numbers, periods, underscores, etc.
beta.0 <- 3; beta.1 <- 2
beta.0 +1

rnorm(100) # to generate 100 random numbers

# R commands are case-sensitive
a <- 1
A <- 2
a==A # will return FALSE

ls() # list objects stored in current database
source("script_file_name_here.R") # will run complete code, without printing results to screen

#######
#Data Types: 

#Vector
x <- c(1,2,3,4,5)
y <- c("Clarke", "Oconee", "Barrow")
x
y
x[3]
y[3]
length(x)
length(y)

#Array
z <- array(1:20, dim=c(4,5)) #array has values 1 to 20 - shortcut for c(1,2,...,20), dim(c(4,5)) means 4 rows, 5 columns
z
z[4,5] #value at location row 4, column 5

#Matrix
Z <- matrix (1:20, 4, 5, byrow=T) #compare byrow=T in Z to default in array
A <- matrix (2, 4, 5)  # "? matrix" for info on labeling rows and columns
Z
A

#List
Jeong <- list(first.name="Seok-Oh", age = 40", citizenship="South Korea")

#Data Frame
x <- c("Jeong", "Lee", "Rojewski")
y <- c(40, 35, 53)
z <- data.frame(first.name=x, age=y)
z

#Factor
z <- c("LD","BD","BD","LD","Non","Non","Non","Non")
z <- factor(z)
levels(z)

#Vectors, Concatenation
a <- c(2,2,2,2,2,2)
              a <- c(1,2,3); b <- c(5,6)
x <- c(a,4,b) # x <- c(1,2,3,4,5,6)

#Vectors, Sequence
x <- seq(from=0, to=1, by=0.1)
x
z <- 1:10
z
p <- rep(1,10)
p

# Vectors, Arithmentic Componentwise
              #Vectors, logical vectors
              x <- 1:10; y<- rep(5,10)
              z <- x<5
              sum(z)
              x<=5 #less than or equal to
              x==5 #equal
              x!=5 #not equal
              (x>5)&(y<2) #and
              (x<5)|(y<2) #or

# Vectors, Missing Values
              x <- c(1,2,3,NA,5)
              is.na(x) #returns FALSE FALSE FALSE TRUE FALSE
              
# Vectors, Index Vectors
        x <- -10:10
              x[3]
              x[1:3]
              x[c(1,3,5)]
              y <- x[x<0]
              y
              x[x<0] <- -x[x<0] # replaces negative values in vector x with positive values
              x
              x <- c(1,2,3,NA,5)
              x[!is.na(x)] #
              x[is.na(x)] <- 4 # fills missing value with 4
              x
              fruit <- c(5,3,2)
              names(fruit) <- c("apple", "orange", "peach")
              fruit[c("apple","peach")]
              
# Arrays and Matrices
              z <- array (1:20, dim=c(4,5))
              A <- matrix (1:20, 4, 5)
              B <- matrix (2,4,5)
              z[3,4]
              A[3,4]
x <- c(1,2,3)
							y <- c(4,5,6)
							cbind(x,y)
							rbind(x,y)
							cbind(B,1:4)
							C <- cbind(A,B)
							
#Arrays and Matrices, Arithmetic: Componentwise
							
              A <- matrix (1:20,4,5)
              B <- matrix (1:20,4,5)
              A+B
              A-B
              A*B
              A/B
              
              #Arrays and Matrices, Arithmetic: Matrix multiplication, inverse
              A <- matrix(runif(20),4,5)
              B <- A%*%t(A) # t(): transpose
              solve(B) # inverse matrix

#Lists
							Jeong <- list(first.name="Seok-Oh", age=40, married=T, no.children=2, child.ages=c(9, 6))
							Jeong$age
							Jeong[[1]]
							Jeong$child.ages
							Jeong[[5]][1]
							
#Factor
x <- c(80,90,85,85,50,60,45,50)
							z <- c("LD","BD","BD","LD","Non","Non","Non","Non")
z <- factor(z)
levels(z)
x.means <- tapply(x,z,mean)							
x.means
							

Data Import and Export:

							
#install and load package, detach
							
