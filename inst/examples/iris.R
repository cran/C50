library(C50)
x <- iris[,1:4]
y <- iris$Species
cat(C50:::makeNamesFile(x, y))
obj <- C5.0(x, y)
j <- sample(nrow(x))
i <- predict(obj, x[j,])
print(levels(y)[i])
correct <- length(y) - sum(levels(y)[i] != as.character(y[j]))
cat(sprintf('Got %d out of %d correct\n', correct, length(y)))
