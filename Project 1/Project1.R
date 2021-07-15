###LM wth k folds (No Transforms)###

bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
	return(coeffs[1] + (coeffs[2] * Age) + (coeffs[3] * Weight) + (coeffs[4] * Height) + (coeffs[5] * Neck)
	+ (coeffs[6] * Chest) + (coeffs[7] * Abdomen) + (coeffs[8] * Hip) + (coeffs[9] * Thigh) + (coeffs[10] * Knee)
	+ (coeffs[11] * Ankle) + (coeffs[12] * Biceps) + (coeffs[13] * Forearm) + (coeffs[14] * Wrist))
}

bf <- read.csv("C:\\Users\\aruns\\Desktop\\Fall 2018\\CPSC 375\\Bodyfatproject.csv")

bf <- bf[,2:15]

rdata <- bf[sample(nrow(bf)),]

folds <- cut(seq(1,nrow(rdata)),breaks=10,labels=FALSE)

mse <- c(10)

for(i in 1:10) {

	testIndexes <- which(folds==i,arr.ind=TRUE)
	testData <- rdata[testIndexes,]
	trainData <- rdata[-testIndexes,]

	m <- lm(trainData$bodyfat~trainData$Age+trainData$Weight+trainData$Height
			+trainData$Neck+trainData$Chest+trainData$Abdomen
			+trainData$Hip+trainData$Thigh+trainData$Knee
			+trainData$Ankle+trainData$Biceps+trainData$Forearm
			+trainData$Wrist,data=trainData)

	coeffs <- coef(m)
	sse <- 0

	for(j in 1:nrow(testData)) {
	bfp <- bodyfatpercentage(testData[j,2],testData[j,3],testData[j,4],testData[j,5],testData[j,6],testData[j,7]
		,testData[j,8],testData[j,9],testData[j,10],testData[j,11],testData[j,12],testData[j,13],testData[j,14])

	sse <- sse + (bfp-testData[j,1])^2
	}

	mse[i] <- sse / (nrow(testData) - ncol(testData))
}

avgmse <- sum(mse)/10



###LM with k folds (BMI)###

bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
	return(coeffs[1] + (coeffs[2] * Age) + (coeffs[3] * Weight) + (coeffs[4] * Height) + (coeffs[5] * Neck)
	+ (coeffs[6] * Chest) + (coeffs[7] * Abdomen) + (coeffs[8] * Hip) + (coeffs[9] * Thigh) + (coeffs[10] * Knee)
	+ (coeffs[11] * Ankle) + (coeffs[12] * Biceps) + (coeffs[13] * Forearm) + (coeffs[14] * Wrist)
	+ (coeffs[15] * (Weight/(Height^2))))
}

bf <- read.csv("C:\\Users\\aruns\\Desktop\\Fall 2018\\CPSC 375\\Bodyfatproject.csv")

bf <- bf[,2:15]

bf <- bf[-42,]

bf$bmi <- (bf$Weight/(bf$Height^2))

rdata <- bf[sample(nrow(bf)),]

folds <- cut(seq(1,nrow(rdata)),breaks=10,labels=FALSE)

mse <- c(10)

for(i in 1:10) {

	testIndexes <- which(folds==i,arr.ind=TRUE)
	testData <- rdata[testIndexes,]
	trainData <- rdata[-testIndexes,]

	m <- lm(trainData$bodyfat~trainData$Age+trainData$Weight+trainData$Height
			+trainData$Neck+trainData$Chest+trainData$Abdomen
			+trainData$Hip+trainData$Thigh+trainData$Knee
			+trainData$Ankle+trainData$Biceps+trainData$Forearm
			+trainData$Wrist+trainData$bmi,data=trainData)

	coeffs <- coef(m)
	sse <- 0

	for(j in 1:nrow(testData)) {
	bfp <- bodyfatpercentage(testData[j,2],testData[j,3],testData[j,4],testData[j,5],testData[j,6],testData[j,7]
		,testData[j,8],testData[j,9],testData[j,10],testData[j,11],testData[j,12],testData[j,13],testData[j,14])

	sse <- sse + (bfp-testData[j,1])^2
	}

	mse[i] <- sse / (nrow(testData) - ncol(testData))
}

avgmse <- sum(mse)/10



###LM wth k folds (Remove Weak Predicters)###

bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
	return(coeffs[1] + (coeffs[2] * Weight) + (coeffs[3] * Height)
	+ (coeffs[4] * Chest) + (coeffs[5] * Abdomen) + (coeffs[6] * Hip) + (coeffs[7] * Thigh)
	+ (coeffs[8] * Wrist))
}

bf <- read.csv("C:\\Users\\aruns\\Desktop\\Fall 2018\\CPSC 375\\Bodyfatproject.csv")

bf <- bf[,2:15]

rdata <- bf[sample(nrow(bf)),]

folds <- cut(seq(1,nrow(rdata)),breaks=10,labels=FALSE)

mse <- c(10)

for(i in 1:10) {

	testIndexes <- which(folds==i,arr.ind=TRUE)
	testData <- rdata[testIndexes,]
	trainData <- rdata[-testIndexes,]

	m <- lm(trainData$bodyfat~trainData$Weight+trainData$Height
			+trainData$Chest+trainData$Abdomen+trainData$Hip+trainData$Thigh
			+trainData$Wrist,data=trainData)

	coeffs <- coef(m)
	sse <- 0

	for(j in 1:nrow(testData)) {
	bfp <- bodyfatpercentage(testData[j,2],testData[j,3],testData[j,4],testData[j,5],testData[j,6],testData[j,7]
		,testData[j,8],testData[j,9],testData[j,10],testData[j,11],testData[j,12],testData[j,13],testData[j,14])

	sse <- sse + (bfp-testData[j,1])^2
	}

	mse[i] <- sse / (nrow(testData) - (ncol(testData)-6))
}

avgmse <- sum(mse)/10###LM wth k folds (No Transforms)###

bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
	return(coeffs[1] + (coeffs[2] * Age) + (coeffs[3] * Weight) + (coeffs[4] * Height) + (coeffs[5] * Neck)
	+ (coeffs[6] * Chest) + (coeffs[7] * Abdomen) + (coeffs[8] * Hip) + (coeffs[9] * Thigh) + (coeffs[10] * Knee)
	+ (coeffs[11] * Ankle) + (coeffs[12] * Biceps) + (coeffs[13] * Forearm) + (coeffs[14] * Wrist))
}

bf <- read.csv("C:\\Users\\aruns\\Desktop\\Fall 2018\\CPSC 375\\Bodyfatproject.csv")

bf <- bf[,2:15]

rdata <- bf[sample(nrow(bf)),]

folds <- cut(seq(1,nrow(rdata)),breaks=10,labels=FALSE)

mse <- c(10)

for(i in 1:10) {

	testIndexes <- which(folds==i,arr.ind=TRUE)
	testData <- rdata[testIndexes,]
	trainData <- rdata[-testIndexes,]

	m <- lm(trainData$bodyfat~trainData$Age+trainData$Weight+trainData$Height
			+trainData$Neck+trainData$Chest+trainData$Abdomen
			+trainData$Hip+trainData$Thigh+trainData$Knee
			+trainData$Ankle+trainData$Biceps+trainData$Forearm
			+trainData$Wrist,data=trainData)

	coeffs <- coef(m)
	sse <- 0

	for(j in 1:nrow(testData)) {
	bfp <- bodyfatpercentage(testData[j,2],testData[j,3],testData[j,4],testData[j,5],testData[j,6],testData[j,7]
		,testData[j,8],testData[j,9],testData[j,10],testData[j,11],testData[j,12],testData[j,13],testData[j,14])

	sse <- sse + (bfp-testData[j,1])^2
	}

	mse[i] <- sse / (nrow(testData) - ncol(testData))
}

avgmse <- sum(mse)/10



###LM with k folds (BMI)###

bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
	return(coeffs[1] + (coeffs[2] * Age) + (coeffs[3] * Weight) + (coeffs[4] * Height) + (coeffs[5] * Neck)
	+ (coeffs[6] * Chest) + (coeffs[7] * Abdomen) + (coeffs[8] * Hip) + (coeffs[9] * Thigh) + (coeffs[10] * Knee)
	+ (coeffs[11] * Ankle) + (coeffs[12] * Biceps) + (coeffs[13] * Forearm) + (coeffs[14] * Wrist)
	+ (coeffs[15] * (Weight/(Height^2))))
}

bf <- read.csv("C:\\Users\\aruns\\Desktop\\Fall 2018\\CPSC 375\\Bodyfatproject.csv")

bf <- bf[,2:15]

bf <- bf[-42,]

bf$bmi <- (bf$Weight/(bf$Height^2))

rdata <- bf[sample(nrow(bf)),]

folds <- cut(seq(1,nrow(rdata)),breaks=10,labels=FALSE)

mse <- c(10)

for(i in 1:10) {

	testIndexes <- which(folds==i,arr.ind=TRUE)
	testData <- rdata[testIndexes,]
	trainData <- rdata[-testIndexes,]

	m <- lm(trainData$bodyfat~trainData$Age+trainData$Weight+trainData$Height
			+trainData$Neck+trainData$Chest+trainData$Abdomen
			+trainData$Hip+trainData$Thigh+trainData$Knee
			+trainData$Ankle+trainData$Biceps+trainData$Forearm
			+trainData$Wrist+trainData$bmi,data=trainData)

	coeffs <- coef(m)
	sse <- 0

	for(j in 1:nrow(testData)) {
	bfp <- bodyfatpercentage(testData[j,2],testData[j,3],testData[j,4],testData[j,5],testData[j,6],testData[j,7]
		,testData[j,8],testData[j,9],testData[j,10],testData[j,11],testData[j,12],testData[j,13],testData[j,14])

	sse <- sse + (bfp-testData[j,1])^2
	}

	mse[i] <- sse / (nrow(testData) - ncol(testData))
}

avgmse <- sum(mse)/10



###LM wth k folds (Remove Weak Predicters)###

bodyfatpercentage <- function(Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh, Knee, Ankle, Biceps, Forearm, Wrist) {
	return(coeffs[1] + (coeffs[2] * Weight) + (coeffs[3] * Height)
	+ (coeffs[4] * Chest) + (coeffs[5] * Abdomen) + (coeffs[6] * Hip) + (coeffs[7] * Thigh)
	+ (coeffs[8] * Wrist))
}

bf <- read.csv("C:\\Users\\aruns\\Desktop\\Fall 2018\\CPSC 375\\Bodyfatproject.csv")

bf <- bf[,2:15]

rdata <- bf[sample(nrow(bf)),]

folds <- cut(seq(1,nrow(rdata)),breaks=10,labels=FALSE)

mse <- c(10)

for(i in 1:10) {

	testIndexes <- which(folds==i,arr.ind=TRUE)
	testData <- rdata[testIndexes,]
	trainData <- rdata[-testIndexes,]

	m <- lm(trainData$bodyfat~trainData$Weight+trainData$Height
			+trainData$Chest+trainData$Abdomen+trainData$Hip+trainData$Thigh
			+trainData$Wrist,data=trainData)

	coeffs <- coef(m)
	sse <- 0

	for(j in 1:nrow(testData)) {
	bfp <- bodyfatpercentage(testData[j,2],testData[j,3],testData[j,4],testData[j,5],testData[j,6],testData[j,7]
		,testData[j,8],testData[j,9],testData[j,10],testData[j,11],testData[j,12],testData[j,13],testData[j,14])

	sse <- sse + (bfp-testData[j,1])^2
	}

	mse[i] <- sse / (nrow(testData) - (ncol(testData)-6))
}

avgmse <- sum(mse)/10
