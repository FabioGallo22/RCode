# Create a matrix 'm' with 31 columns and 12 rows with random values. These are going to simulate a city temperatures in a year. Let's ignore the fact that not all the months have 31 days :)
summer = matrix(round(runif(3*31,10,40)),3,31)
fall = matrix(round(runif(3*31,8,30)),3,31)
winter = matrix(round(runif(3*31,-3,20)),3,31)
spring = matrix(round(runif(3*31,7,30)),3,31)
m = rbind(summer,fall,winter,spring)

# shows m's values
m

# Define an expression for obtaining:
# a - the mean temperature of each month.
meanT=apply(m, 1, mean)
meanT

# b - the month with the lowest mean temperature.
which(meanT == min(meanT))
