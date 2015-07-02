## setup a vector of limit for each input variable, initialize it as zero vectors with len 11
limout <- rep(0, 11)
n <- dim(wineRaw2)[1]
for (i in 1:11) {
  t1 <- quantile(wineRaw2[,i], 0.75)
  t2 <- IQR(wineRaw2[,i])
  limout[i] <- t1 + 1.5*t2
}

##print(limout)

matrix_lim <- matrix(0, n, 11)

for (i in 1:n) {
  for (j in 1:11) {
    if (wineRaw2[i,j] > limout[j]) matrix_lim[i,j] <- 1
  }
}

vecOut <- apply(matrix_lim, 1, max)

##print(vecOut)

wineRaw4 <- cbind(wineRaw2, vecOut)
wineRaw4 <- wineRaw4[wineRaw4$vecOut == 0,]

vecTrain <- rbinom(dim(wineRaw4)[1], 1, 0.7)

wineRaw4 <- cbind(wineRaw4, vecTrain)

wineRaw_Train <- wineRaw4[wineRaw4$vecTrain == 1,-(13:14)]
wineRaw_Test <- wineRaw4[wineRaw4$vecTrain == 0,-(13:14)]
print(dim(wineRaw_Train))
print(dim(wineRaw_Test))