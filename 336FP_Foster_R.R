# Final project R source code for all data and figures.
# NOAA Global Mean Surface Temperature Anomalies (1880-2017)
rm(list = ls(all = TRUE))
# Read in .asc file (unopenable).
noaa_initial_data = scan("C:/Users/Dan/Desktop/336 Documents/NOAAGlobalTemp.gridded.v4.0.1.201802.asc")
length(noaa_initial_data) # Initial read length check.

# Hard to maniulate (x vs. y?).
noaa_initial_data[1:3]
#month, year, temp (72 rows, 36 columns)
data_samp1 = seq(1, 4267129, by = 2594)
data_samp2 = seq(2, 4267130, by = 2594)
length(data_samp1)
length(data_samp2)

month1 = noaa_initial_data[data_samp1] # Sample (months).
year1 = noaa_initial_data[data_samp2] # Sample (years).
head(month1)
head(year1)
length(month1)
length(year1) # Dimension check  based on year alone.

var_names <- paste(year1, sep = "-", month1) # year-month
head(data_samp1)
head(data_samp2)
data_samp3 = cbind(data_samp1, data_samp2) # Column bind.
data_samp4 = as.vector(t(data_samp3))
head(data_samp4)
data_next <- noaa_initial_data[-data_samp4] # Remote months/years.
length(data_next) / (36 * 72) # 137 years (2017-1880), (137 * 12) = 1,645 months

data_last <- matrix(data_next, ncol = 1645)
colnames(data_last) <- var_names # Bind column names.
latitudinal_data = seq(-87.5, 87.5, length = 36)
longitudinal_data = seq(2.5, 357.5,  length = 72)
LATITUDE = rep(latitudinal_data, 72)
LONGITUDE = rep(longitudinal_data[1],36)

for (i in 2:72) {LONGITUDE = c(LONGITUDE, rep(longitudinal_data[i], 36))}
noaa_data = cbind(LATITUDE, LONGITUDE, data_last)
head(noaa_data) # Gives temps based on lat/lon.
dim(noaa_data) # NOAA final dim sanity check.

for(x in 1:nrow(noaa_data)){
  for(y in 3:ncol(noaa_data)){
    if(noaa_data[x, y] < -300)
      noaa_data[x, y] <- 0
  }
}

# Now manipulate data for final intgeration with correct columns.
# Time - Year
time_year = seq(1880, 2017, by = 1) # 1880-2017
time_month = rep(time_year, each = 12)
time_by_month = rep(1:12, 138)
data_samp5 = paste(time_year, "-", time_by_month)
data_samp6 = c("Latitude", "Longitude", data_samp5)
length(data_samp6)
colnames(noaa_data) <- data_samp6[1:1647] # Transpose column names and remove 2017 last (extra) months.

# Plot field data via maps lib.
library(maps)
lat2_vec = seq(-87.5, 87.5, length = 36)
lon2_vec = seq(2.5, 357.5, length = 72)

vectors_to_map = noaa_data[,1635]
vector1 = pmin(vectors_to_map, 6)
vector2 = pmax(vector1, -6)
mapmat_vectors = matrix(vector2, nrow = 72)
plot(seq(-90, 90, len = 36), mapmat_vectors[36, c(1:36)], type = "l")

# Select latitude and longitude (here 30 to 50 and 230 to 295, respectively). CUS.
lat_and_lon <- which(noaa_data[,1] > 30 & noaa_data[,1] < 50 & noaa_data[,2] > 230 & noaa_data[,2] < 295)
dim(lat_and_lon)
noaa_manip = matrix(0, nrow = 52, ncol = ncol(noaa_data)) # nrow = size of lat_and_lon

# Final cleaned data for all NOAA stuff.
for(a in 1:52){
  noaa_manip[a,] = noaa_data[lat_and_lon[a],]
}

dim(noaa_manip)
#write.csv(noaa_manip, "C:/Users/Dan/Desktop/data.csv") To view data.
#mtf_dates = seq(1880, 2017.1666666666, by=0.0833333333) Don't need to include extra data in 2017.
#plot(mtf_dates)

svd_data = svd(noaa_manip)
dim(noaa_manip)

U = svd_data$u
D = svd_data$d
V = svd_data$v

### Annual SAT of EOF's and PC's (variance).
data_temp=seq(855,1466,by=12)
annual_SAT=matrix(0,nrow=52,ncol=50) # 52 rows.
for (i in 1:50) {annual_SAT[,i]=rowMeans(noaa_manip[,seq(data_temp[i],data_temp[i+1]-1, length= 12)])}
annual_SAT2=svd(annual_SAT)

annual_data <- annual_SAT2$v[,1]
dim(annual_data)
time_seq = seq(1951, 2000)
plot(time_seq,-annual_data,type='o')

### Mean temperature analysis.
par(mfrow=c(1,1))
USland = colMeans(noaa_manip)
write.csv(USland, "C:/Users/Dan/Desktop/data.csv")
write.csv(mtf_dates, "C:/Users/Dan/Desktop/mtf_dates.csv")
mtf_dates=seq(1880,2017,by=0.0833333333)
plot(mtf_dates,USland[3:1647],type='l',xlab="Year",ylab="Mean Temperature Anomaly (degrees C)",xlim=range(1880:2017),ylim=range(-4:4),main="Continental United States Mean Temperature Anomalies (1880-2017)")
lm1=lm(USland[3:1647]~mtf_dates,na.action=na.omit) # Full regression.
lm1
abline(lm1,col='red',lwd=2)
text(1900,2.9,col='black','Key:',cex=1) # Key.
text(1900,2.6,col='red','1880-2017 Trend',cex=1)


lm2=lm(USland[3:242]~mtf_dates[1:240],na.action=na.omit) # 1880-1900
lm2
abline(lm2,col='green',lwd=2)
text(1900,2.3,col='green','1880-1900 Trend',cex=1)

lm3=lm(USland[1407:1647]~mtf_dates[1405:1645],na.action=na.omit) # 1997-2017
lm3
abline(lm3,col='blue',lwd=2)
text(1900,2,col='blue','1997-2017 Trend',cex=1)

lm4=lm(USland[3:818]~mtf_dates[1:816],na.action=na.omit) # 1880-1945
lm4

lm5=lm(USland[833:1647]~mtf_dates[831:1645],na.action=na.omit) # 1946-2017
lm5

# Print the first ten eigenvalues of the space-time decomposition.
D[1:10]
# [1] 1924.77727  114.59196  101.85219   94.19324   67.52522   55.64969   52.48308   41.18995   40.87237
#[10]   36.59311

plot.new()
par(mar = c(4, 4, 0.2, 0.5))

# Spatial data (in terms of x).
dim(U)
plot(1:52, U[,50], type = "o", col = "green", xlab = "Spatial Position: x", ylab = "Mode Values", ylim = c(min(U[,50]), max(U[,50])), lwd = 1.5, main = "Spatial (U) vectors - Column One")
plot(1:52, U[,51], type = "o", col = "blue", xlab = "Spatial Position: x", ylab = "Mode Values", ylim = c(min(U[,51]), max(U[,51])), lwd = 1.5, main = "Spatial (U) vectors - Column Two")
plot(1:52, U[,52], type = "o", col = "orange", xlab = "Spatial Position: x", ylab = "Mode Values", ylim = c(min(U[,52]), max(U[,52])), lwd = 1.5, main = "Spatial (U) vectors - Column Three")

# Temporal data (in terms of t).
dim(V)
plot(1:1647, V[,52], type = "o", col = "green", xlab = "Temporal Position: t", ylab = "Mode Values", lwd = 1.5, main = "Temporal (V) vectors - Column One")
plot(1:1647, V[,51], type = "o", col = "blue", xlab = "Temporal Position: t", ylab = "Mode Values", lwd = 1.5, main = "Temporal (V) vectors - Column Two")
plot(1:1647, V[,50], type = "o", col = "orange", xlab = "Temporal Position: t", ylab = "Mode Values", lwd = 1.5, main = "Temporal (V) vectors - Column Three")

