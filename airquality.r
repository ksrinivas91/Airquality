require("datasets")
data("airquality")
str(airquality)
head(airquality)
col1<- mapply(anyNA,airquality) # apply function anyNA() on all columns of airquality dataset
col1
# Impute monthly mean in Ozone
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  # Impute monthly mean in Solar.R
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
  
}
#Normalize the dataset so that no particular attribute has more impact on clustering algorithm than others.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
airquality<- normalize(airquality) # replace contents of dataset with normalized values
str(airquality)


Y<- airquality[,"Ozone"] # select Target attribute
X<- airquality[,"Solar.R"] # select Predictor attribute

model1<- lm(Y~X)
model1 # provides regression line coefficients i.e. slope and y-intercept

plot(Y~X) # scatter plot between X and Y
abline(model1, col="blue", lwd=3) # add regression line to scatter plot to see relationship between X and Y

Y<- airquality[,"Ozone"] # select Target attribute
X<- airquality[,"Wind"] # select Predictor attribute

model2<- lm(Y~X)
model2 # provides regression line coefficients i.e. slope and y-intercept

plot(Y~X) # scatter plot between X and Y
abline(model2, col="blue", lwd=3) # add regression line to scatter plot to see relationship between X and Y

# Prediction of 'Ozone' when 'Solar.R'= 10
p1<- predict(model1,data.frame("X"=10))
p1

# Prediction of 'Ozone' when 'Wind'= 5
p2<- predict(model2,data.frame("X"=5))
p2

