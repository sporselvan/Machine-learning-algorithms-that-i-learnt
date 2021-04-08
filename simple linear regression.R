
#simple linear regression
#prdicting students score by studying hour      


students_data <-read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv",header = TRUE)

summary(students_data)

#1.check NA's

colSums(is.na(students_data))  #NO Na values

#2.scatter plot for finding the relation between independent and dependent variable

scatter.smooth(students_data$Scores~students_data$Hours,xlab = "Hours",ylab = "score",col="red")

#boxpot for finding outliers 

boxplot(students_data$Hours,col = "red")
boxplot(students_data$Scores,col="blue")  #no  outliers

library(corrplot)
cor(x=students_data$Hours,y=students_data$Scores,method = "pearson")

#first assumption is relation between independent and dependent variable.
#correlation score= 0.97

#building linear model

linear_model<-lm(Scores~Hours,data = students_data)
summary(linear_model)
#p<0.05 ,rsquare value=0.95

par(mfrow=c(2,2))
plot(linear_model)
#Linear model satisfies the assumptions of linear Regression. 
#We can check this with diagnostic plots.


#predicting score for working hour 9.25

#residual standard error . close to 0 is better
sigma(linear_model)*100/mean(students_data$Scores)
#RSE is 10.88

#prediction
#predicting score for working hour 9.25

predict(linear_model,data.frame(Hours=9.25))  

#conclusion

#predicted score is 92.90.
#It means student who studied for 9.25 hours may get 92.90 marks.  

