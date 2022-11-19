square <- function(x) {
  y <- x ** 2
  return(y)
}

a <- c(1,2,3)
ta <- table(a)
class(a)
class(ta)
square(a)

with(a, x**2)

args(square)

df <- data.frame(player = c('AJ', 'Bob', 'Chad', 'Dan', 'Eric', 'Frank'),
                 position = c('A', 'B', 'B', 'B', 'B', 'A'),
                 points = c(1, 2, 2, 1, 0, 0))
df
table(df)

rm(list = ls())

Num <- c(100,100,100,100,100)
Cost <- c(1200,1300,1400,1500,1600)

data_A <- data.frame(Num,Cost,stringsAsFactors = FALSE)

with(data_A, Num*Cost)
with(data_A, Cost/Num)