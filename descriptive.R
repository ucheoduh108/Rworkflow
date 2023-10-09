set.seed(1)
#Generate random number and store it in a variable called data
data =  runif(20,1,10)
data

#calculate the mean
mean = mean(data)
mean

#calculate median
med = median(data)
print(med)
cat("median = ",med)

#create a fnx for calculating mode
result <- mode(data)
print(result)
cat("mode =",result)

#calculate variance and standard deviation
varia = var(data)
stddev = sqrt(varia)
print(stddev)
cat('standard deviation =', stddev)

#plot histagram
hist(data, bins=10, range = c(0,10), edgecolor = 'black')
 