require(plyr)
source("./distance.r")

setwd("C:/Users/zmyao/Dropbox/Prof Vasi/spectral analysis")

#Read in Raw data
lati <- read.csv("./Population_LatitudeLongitude_12.18.12.csv", stringsAsFactors=F)
lati <- lati[order(lati$FIPS_city),]
ows <- read.csv("OWSprotests_12.18.12.csv", stringsAsFactors=F)

dat1 <- subset(lati, select=c("FIPS_city", "Latitude", "Longitude"))
dat2 <- subset(ows, ows_protest == 1 ,select=c("City_FIPS", "ows_protest"))

#select cities has protest event and geo-spatial coordinates
event_city <- merge(dat1,dat2, by.x="FIPS_city", by.y="City_FIPS")

protest <- ows[ows$City_FIPS %in% event_city$FIPS_city, ]
sel <- ddply(protest, .(City_FIPS), function(X) tail(X,1))

# Create protest matrix
pro_mat <- matrix(0, 205, 77)
rownames(pro_mat) <- sel$City_FIPS

for(i in 1:205)
{
    for(j in sel$TIME[i]:77)
    {
        pro_mat[i,j]<-1
    }  
}

# Create Distance Matrix
dist_mat <- matrix(NA,205,1234)
rownames(dist_mat) <- event_city$FIPS_city
colnames(dist_mat) <- lati$FIPS_city

for (i in 1: nrow(event_city))
{
    for (j in 1: nrow(lati))
    {
        dist_mat[i,j] <- gcd.slc(event_city$Longitude[i]/180*pi, event_city$Latitude[i]/180*pi,
                                 lati$Longitude[j]/180*pi, lati$Latitude[j]/180*pi)
    }
}
