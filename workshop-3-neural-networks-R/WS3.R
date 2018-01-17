install.packages('nnet')
install.packages('NeuralNetTools')


library(nnet)
library(NeuralNetTools)


data(iris)


# shuffle
x <- iris[sample(1:nrow(iris)),]


# Create training and testing data
train <- x[1:100,]
test <- x[101:150,]


model_nnet <- nnet(Species ~ ., data=train, size=10, maxit=1000)


pred<- predict(model_nnet, test, type="class")


table(true=test$Species, predicted=pred)

result<- cbind(test, data.frame(pred))

write.csv(result, file="D:\\resultBP.csv")


par(mar = numeric(4), family = 'serif')
plotnet(model_nnet, pos_col = "green", neg_col = "red")


#-------------------

install.packages('grnn')


library(grnn)

data = read.csv("D:\\iris_GRNN.csv", header=TRUE)

size=nrow(data)

length=ncol(data)

index <- 1:size

positions <- sample(index, trunc(size * 0.75))

training <- data[positions,]
testing <- data[-positions,1:length-1]
result = data[-positions,]
result$actual = result[,length]
result$predict = -1

nn <- learn(training, variable.column=length)
nn <- smooth(nn, sigma = 0.5)


for(i in 1:nrow(testing))
{	
	vec <- as.matrix(testing[i,])
	res <- guess(nn, vec)
	
	if(is.nan(res))
	{
		cat("Entry ",i," Generated NaN result!\n")
	}
	else
	{
		result$predict[i] <- res
	}
}



result.size = nrow(result)
result.correct = nrow(result[round(result$predict) == result$actual,])
cat("No of test cases = ",result.size,"\n")
cat("Correct predictions = ", result.correct ,"\n")
cat("Accuracy = ", result.correct / result.size * 100 ,"\n")


result2<- cbind(testing, data.frame(result))

write.csv(result, file="D:\\resultGRNN.csv")

