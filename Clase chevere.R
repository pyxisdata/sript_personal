ggplot(iris, aes(iris$Species, fill = iris$Species)) + 
  geom_bar() +
  guides(fill = FALSE) +
  ggtitle("QUE")
  

iris%>%
  ggplot(aes(iris$Species, fill = iris$Species)) + 
  geom_bar()

1 -> a
c(1:10) -> lista
lista
lista * 2
c(11:15) -> lista2
lista / 2
lista / lista2 -> otro
cbind(otro)
install.packages("knitr")
a <- 5; b <- 6
a & b
a > b
!a > b
lista
lista[3]
lista2[4]
matrix(10:30, ncol = 3, nrow = 7) -> estajoda
t(estajoda)
colnames(estajoda) <- c("a", "b", "c")
estajoda
a <- c(10, 20, 30)
b <- c("diez", "veinte", "treinta")
c <- data.frame(c(a, b))
c
class(c)
dim(c)
dim(c)[2]
dim(c)[1]
library(readr)
desorden <- c(1,3,4,5,6,7,8,87,67,56,86,86,7,867,99,689)
desorden
sort(desorden)
sort(desorden, decreasing = FALSE)
sort(desorden, decreasing =  TRUE)
summary(iris)
sd(iris$Sepal.Length)

if (10 > 50) {
  print("Estamos jodidos")
} else {
  print("Buenisimo")
}
i <- 1
while (i < 1000000) {
  print(i)
  i <- i + 1
}
y <- rep(NA, 100)
y[1] <- 1
for (i in 2:100) {
  y[i] <- y[i - 1] + 4
}
y
library(MASS)
painters
escuela <- painters$School
escuela
table(escuela)
plot(table(escuela))
library("knitr")
painters%>%
  ggplot(aes(.$School, col = .$School, fill = .$School)) +
  geom_bar() +
  guides(fill = FALSE)

cbind(table(escuela))%>%
  kable(format = "markdown")

cbind(table(escuela)) / dim(painters)[1] * 100
round(cbind(table(escuela)) / dim(painters)[1] * 100, 2)
barplot(table(escuela), col = 1:2)

ggplot(painters, aes(painters$School)) +
  geom_bar() +
  labs(title = "Escuelas")

escuela == "A"
subset(escuela, escuela == "A")
subset(painters, painters$School == "A")
