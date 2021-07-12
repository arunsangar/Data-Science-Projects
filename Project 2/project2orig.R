df <- read.df('Bodyfatproject.csv', "csv", header="true", inferSchema = "true", na.strings = "NA") # convert CSV file to SparkDataFrame
dfselected <- select(df, "bodyfat", "Age", "Weight", "Height", "Abdomen") # selet a few columns
count(dfselected) # how many columns
dfcorrected <- filter(dfselected, "Height > 30") # filter out row with incorrect values
dfcorrected <- filter(dfcorrected, "bodyfat > 1") # filter out row with incorrect values
count(dfcorrected)

traintest <- randomSplit(dfcorrected, c(1,1)) # split into equal train/test portions
str(traintest) # traintest is a list with two SparkDataFrames
traindata <- traintest[[1]]
testdata <- traintest[[2]]

# train using glm. family="gaussian" specifies linear regression
mod <- glm(bodyfat~Age+Weight+Height+Abdomen, data=traindata, family="gaussian")
summary(mod) # show fitted model characteristics
prederr <- predict(mod, testdata) # predict using test data

mse <- head(select(prederr, mean((prederr$bodyfat-prederr$prediction)^2))) # calculate Mean Squared Error
a <- collect(select(prederr, "bodyfat", "prediction")) # convert SparkDataFrame to native R data.frame
plot(a$bodyfat,a$prediction)

stderr <- sqrt(mse)
