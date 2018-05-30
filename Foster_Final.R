#6.1
setwd("C:/Users/Dan/Desktop/336 Documents")
data = read.csv("JonesGlobalT.csv", header = TRUE) # Reads data file.
temp = data$MAY #or data$APR
year = data$YEAR #or data$YEAR
plot(year, temp, col = "black", type = "l", xlab = "Year", ylab = "Temperature [oC]", main = "Daniel Foster's plot of temperature anomalies", lwd = 3)
lm(temp ~ year)
# lm=0.004891
trend = 0.004891 * 100
trend
# trend=0.4891
abline(lm(temp ~ year), col="red")
text(1900, 0.5, "Trend: 0.4891 [degC per century]", cex=1.5)

#6.2
temp2=data$DEC
dec1=mean(temp2[51:60])
dec1
dec2=mean(temp2[61:70])
dec2
dec3=mean(temp2[71:80])
dec3
dec4=mean(temp2[81:90])
dec4
dec5=mean(temp2[91:100])
dec5
dec6=mean(temp2[101:110])
dec6
dec7=mean(temp2[111:120])
dec7
dec8=mean(temp2[121:130])
dec8
dec9=mean(temp2[131:140])
dec9
dec10=mean(temp2[141:150])
dec10
finaldata=c(dec1,dec2,dec3,dec4,dec5,dec6,dec7,dec8,dec9,dec10)
years=seq(1900,1990, by=10)
final=matrix(c(years,finaldata), nrow=10,ncol=2)
final
#    [,1]    [,2]
#[1,] 1900 -0.4274
#[2,] 1910 -0.4120
#[3,] 1920 -0.2692
#[4,] 1930 -0.1473
#[5,] 1940 -0.0678
#[6,] 1950 -0.0713
#[7,] 1960 -0.0621
#[8,] 1970 -0.0716
#[9,] 1980  0.1088
#[10,] 1990  0.2536

#Hottest decade is 1990-1999 and coldest decade is 1900-1909.

#6.3
d=.8
l=.6
k=0 
n=10000 
for (i in 1:n) {if((runif(1,0,d)+l*cos(runif(1,-pi/2,pi/2))) >= d) k=k+1} 
k/n
#Simulation reult is 0.471, which is about 47.1%.
