### main program
source("prepare.R")

library(stringr)

### read in prec data as background plot
precDF <- read.table("data/prec_annual.txt", header=F)
colnames(precDF) <- c("Lon", "Lat", "prec")

precDF$prec_cat <- ifelse(precDF$prec <= 100, 1, 
                          ifelse(precDF$prec > 100 & precDF$prec <= 500, 2,
                                 ifelse(precDF$prec > 500 & precDF$prec <= 2000, 3, 
                                        ifelse(precDF$prec > 2000 & precDF$prec <= 4000, 4, 5))))

### read in wet factors
wetDF <- read.csv("data/wet_factor_fixed_plot.csv")
colnames(wetDF) <- c("SCCSID", "Lat", "Lon", "wet_factor", "wet_pct", "wet_abs")
wetDF$wet_factor_cat <- ifelse(wetDF$wet_factor <= -2, 1, 
                          ifelse(wetDF$wet_factor > -2 & wetDF$wet_factor <= -1, 2,
                                 ifelse(wetDF$wet_factor > -1 & wetDF$wet_factor <= -0.1, 3, 
                                        ifelse(wetDF$wet_factor > -0.1 & wetDF$wet_factor <= 0.1, 4, 
                                               ifelse(wetDF$wet_factor > 0.1 & wetDF$wet_factor <= 1, 5, 
                                                      ifelse(wetDF$wet_factor > 1 & wetDF$wet_factor <= 2, 6, 7))))))

wetDF$wet_pct_cat <- ifelse(wetDF$wet_pct <= -2, 1, 
                               ifelse(wetDF$wet_pct > -2 & wetDF$wet_pct <= -1, 2,
                                      ifelse(wetDF$wet_pct > -1 & wetDF$wet_pct <= -0.1, 3, 
                                             ifelse(wetDF$wet_pct > -0.1 & wetDF$wet_pct <= 0.1, 4, 
                                                    ifelse(wetDF$wet_pct > 0.1 & wetDF$wet_pct <= 1, 5, 
                                                           ifelse(wetDF$wet_pct > 1 & wetDF$wet_pct <= 2, 6, 7))))))

wetDF$wet_abs_cat <- ifelse(wetDF$wet_abs <= -2, 1, 
                            ifelse(wetDF$wet_abs > -2 & wetDF$wet_abs <= -1, 2,
                                   ifelse(wetDF$wet_abs > -1 & wetDF$wet_abs <= -0.1, 3, 
                                          ifelse(wetDF$wet_abs > -0.1 & wetDF$wet_abs <= 0.1, 4, 
                                                 ifelse(wetDF$wet_abs > 0.1 & wetDF$wet_abs <= 1, 5, 
                                                        ifelse(wetDF$wet_abs > 1 & wetDF$wet_abs <= 2, 6, 7))))))


### read in elevation data
elevDF <- read.table("data/crutbaselv.txt", header=F, sep=",")
colnames(elevDF) <- c("Lon", "Lat", "label", "elev", "elev2", "region")

elevDF$elev_cat <- ifelse(elevDF$elev2 <= 200, 1, 
                              ifelse(elevDF$elev2 > 200 & elevDF$elev2 <= 500, 2,
                                     ifelse(elevDF$elev2 > 500 & elevDF$elev2 <= 1000, 3, 
                                            ifelse(elevDF$elev2 > 1000 & elevDF$elev2 <= 3000, 6, 7))))


### read enso data
### 2.5 degree resolution, ascii file
enso <- read.table("data/enso.txt", header=F)

# creat lon and lat list
lon.list <- seq(-178.75, 178.75, by=2.5)
lat.list <- seq(-88.75, 88.75, by=2.5)

ensoDF <- data.frame(rep(lon.list, each=length(lat.list)),
                     rep(lat.list, times=length(lon.list)),
                     NA)
colnames(ensoDF) <- c("Lon", "Lat", "enso")

for (i in 1:length(lon.list)) {
    for (j in 1:length(lat.list)) {
        ensoDF[ensoDF$Lon == lon.list[i] & ensoDF$Lat == lat.list[j], "enso"] <- enso[i,j]
    }
}

ensoDF$enso_cat <- ifelse(ensoDF$enso >-1 &ensoDF$enso <= -0.5, 1, 
                          ifelse(ensoDF$enso > -0.5 & ensoDF$enso <= -0.1, 2,
                                 ifelse(ensoDF$enso > -0.1 & ensoDF$enso <= 0.1, 3, 
                                        ifelse(ensoDF$enso > 0.1 & ensoDF$enso <= 0.5, 4, 5))))

### plotting
p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=as.character(precDF$prec_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=as.character(wetDF$wet_factor_cat)), size=3)+
    scale_fill_manual(name="Rainfall (mm/yr)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
    scale_color_manual(name="Wet factor", 
                       values=c("indianred4", "indianred3", "indianred1","thistle1", 
                                "slateblue1","purple1",  "purple4"),
                       label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 1", "1 to 2", "> 2"))+
    theme(legend.position="right")+
    guides(fill=guide_legend(nrow=3), color=guide_legend(nrow=3))


p2 <- ggplot() + 
    geom_tile(data=ensoDF, aes(y=Lat, x=Lon, fill=as.character(ensoDF$enso_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    borders(colour = alpha("black", 0.8), lwd=0.2)+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=as.character(wetDF$wet_pct_cat)), size=3)+
    scale_fill_manual(name="ENSO index", 
                      values=alpha(c("indianred4", "indianred1","thistle1","skyblue","blue"),0.2),
                      label=c("-1 to -0.5", "-0.5 to -0.1", "-0.1 to 0.1", "0.1 to 0.5", "0.5 to 1"))+
    scale_color_manual(name="Wet extreme - percentile", 
                       values=c("indianred4", "indianred3", "indianred1","thistle1", 
                                "slateblue1","purple1",  "purple4"),
                       label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 1", "1 to 2", "> 2"))+
    theme(legend.position="right")+
    guides(fill=guide_legend(nrow=3), color=guide_legend(nrow=3))

p3 <- ggplot() + 
    geom_tile(data=elevDF, aes(y=Lat, x=Lon, fill=as.character(elevDF$elev_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=as.character(wetDF$wet_abs_cat)), size=3)+
    scale_fill_manual(name="Elevation (m)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-200", "200-500", "500-1000", "1000-3000", ">3000"))+
    scale_color_manual(name="Wet extreme - absolute", 
                       values=c("indianred4", "indianred3", "indianred1","thistle1", 
                                "slateblue1","purple1",  "purple4"),
                       label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 1", "1 to 2", "> 2"))+
    theme(legend.position="right")+
    guides(fill=guide_legend(nrow=3), color=guide_legend(nrow=3))


pdf("output/wet_factors.pdf", width=12,height=10)
plot_grid(p1, p2, p3, labels="AUTO", 
          ncol=1, align="v", axis="l")
dev.off()


### read in drought indices to plot
dtDF <- read.table("data/indices_to_plot_drought.txt", header=F)
colnames(dtDF) <- c("SCCSID", "Lat", "Lon", "PE", "PDSI")

dtDF$PE_cat <- ifelse(dtDF$PE <= -0.3, 1, 
                      ifelse(dtDF$PE > -0.3 & dtDF$PE <= -0.1, 2, 
                             ifelse(dtDF$PE > -0.1 & dtDF$PE <= 0.1, 3, 
                                    ifelse(dtDF$PE > 0.1 & dtDF$PE <= 0.3, 4, 5))))

dtDF$PDSI_cat <- ifelse(dtDF$PDSI <= -2, 1, 
                               ifelse(dtDF$PDSI > -2 & dtDF$PDSI <= -1, 2,
                                      ifelse(dtDF$PDSI > -1 & dtDF$PDSI <= -0.1, 3, 
                                             ifelse(dtDF$PDSI > -0.1 & dtDF$PDSI <= 0.1, 4, 
                                                    ifelse(dtDF$PDSI > 0.1 & dtDF$PDSI <= 1, 5, 
                                                           ifelse(dtDF$PDSI > 1 & dtDF$PDSI <= 2, 6, 7))))))

### plot precDF wet factor
p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=as.character(precDF$prec_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=dtDF, aes(y=Lat, x=Lon, color=as.character(dtDF$PE_cat)), size=3)+
    scale_fill_manual(name="Rainfall (mm/yr)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
    scale_color_manual(name="PE", 
                       values=c("indianred4", "indianred1", "thistle1", "slateblue1", "purple4"),
                       label=c("< -0.3", "-0.3 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 0.3", "> 0.3"))+
    guides(fill=guide_legend(nrow=3), color=guide_legend(nrow=3))


p2 <- ggplot() + 
    geom_tile(data=ensoDF, aes(y=Lat, x=Lon, fill=as.character(ensoDF$enso_cat))) +
    borders(colour = alpha("black", 0.8), lwd=0.2)+
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=dtDF, aes(y=Lat, x=Lon, color=as.character(dtDF$PDSI_cat)), size=3)+
    scale_fill_manual(name="ENSO index", 
                      values=alpha(c("indianred4", "indianred1","thistle1","skyblue","blue"),0.2),
                      label=c("-1 to -0.5", "-0.5 to -0.1", "-0.1 to 0.1", "0.1 to 0.5", "0.5 to 1"))+
    scale_color_manual(name="PDSI", 
                       values=c("indianred4", "indianred3", "indianred1","thistle1", 
                                "slateblue1","purple1",  "purple4"),
                       label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 1", "1 to 2", "> 2"))+
    guides(fill=guide_legend(nrow=3), color=guide_legend(nrow=3))


pdf("output/drought_indices.pdf", width=12,height=8)
plot_grid(p1, p2, labels="AUTO", ncol=1, align="v", axis="l")
dev.off()



### read in drought indices to plot
dtDF2 <- read.table("data/indices_to_plot_drought_factors.txt", header=T)

dtDF2$F1_cat <- ifelse(dtDF2$Factor1 <= -2, 1, 
                       ifelse(dtDF2$Factor1 > -2 & dtDF2$Factor1 <= -1, 2,
                              ifelse(dtDF2$Factor1 > -1 & dtDF2$Factor1 <= -0.1, 3, 
                                     ifelse(dtDF2$Factor1 > -0.1 & dtDF2$Factor1 <= 0.1, 4, 
                                            ifelse(dtDF2$Factor1 > 0.1 & dtDF2$Factor1 <= 1, 5, 
                                                   ifelse(dtDF2$Factor1 > 1 & dtDF2$Factor1 <= 2, 6, 7))))))

dtDF2$F2_cat <- ifelse(dtDF2$Factor2 <= -2, 1, 
                        ifelse(dtDF2$Factor2 > -2 & dtDF2$Factor2 <= -1, 2,
                               ifelse(dtDF2$Factor2 > -1 & dtDF2$Factor2 <= -0.1, 3, 
                                      ifelse(dtDF2$Factor2 > -0.1 & dtDF2$Factor2 <= 0.1, 4, 
                                             ifelse(dtDF2$Factor2 > 0.1 & dtDF2$Factor2 <= 1, 5, 
                                                    ifelse(dtDF2$Factor2 > 1 & dtDF2$Factor2 <= 2, 6, 7))))))

### plot precDF wet factor
p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=as.character(precDF$prec_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=dtDF2, aes(y=Lat, x=Lon, color=as.character(dtDF2$F1_cat)), size=3)+
    scale_fill_manual(name="Rainfall (mm/yr)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
    scale_color_manual(name="Factor 1", 
                       values=c("indianred4", "indianred3", "indianred1","thistle1", 
                                "slateblue1","purple1",  "purple4"),
                       label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 1", "1 to 2", "> 2"))+
    guides(fill=guide_legend(nrow=3), color=guide_legend(nrow=3))


p2 <- ggplot() + 
    geom_tile(data=ensoDF, aes(y=Lat, x=Lon, fill=as.character(ensoDF$enso_cat))) +
    borders(colour = alpha("black", 0.8), lwd=0.2)+
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=dtDF2, aes(y=Lat, x=Lon, color=as.character(dtDF2$F2_cat)), size=3)+
    scale_fill_manual(name="ENSO index", 
                      values=alpha(c("indianred4", "indianred1","thistle1","skyblue","blue"),0.2),
                      label=c("-1 to -0.5", "-0.5 to -0.1", "-0.1 to 0.1", "0.1 to 0.5", "0.5 to 1"))+
    scale_color_manual(name="Factor 2", 
                       values=c("indianred4", "indianred3", "indianred1","thistle1", 
                                "slateblue1","purple1",  "purple4"),
                       label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
                               "0.1 to 1", "1 to 2", "> 2"))+
    guides(fill=guide_legend(nrow=3), color=guide_legend(nrow=3))


pdf("output/drought_factor_indices.pdf", width=12,height=8)
plot_grid(p1, p2, labels="AUTO", ncol=1, align="v", axis="l")
dev.off()


### read in tair df
tairDF <- read.table("data/tair_annual.txt", header=F)
colnames(tairDF) <- c("Lon", "Lat", "tair")

coldDF <- read.table("data/factors_to_plot_cold.txt", header=F)
colnames(coldDF) <- c("SCCSID", "Lat", "Lon", "cold")

warmDF <- read.table("data/warm_factor_plot.txt", header=F)
colnames(warmDF) <- c("SCCSID", "Lat", "Lon", "warm")

### plot tair cold
p1 <- ggplot() + 
    geom_tile(data=tairDF, aes(y=Lat, x=Lon, fill=tair), alpha=0.5) +
    coord_quickmap(xlim=range(tairDF$Lon), ylim=range(tairDF$Lat))+
    geom_point(data=coldDF, aes(y=Lat, x=Lon, color=cold),size=3)+
    scale_fill_continuous(name="Temperature (degree)", 
                          type="viridis")+
    scale_color_gradient2(name="Cold factor", 
                          low="red", high="blue")

p2 <- ggplot() + 
    geom_tile(data=tairDF, aes(y=Lat, x=Lon, fill=tair), alpha=0.5) +
    coord_quickmap(xlim=range(tairDF$Lon), ylim=range(tairDF$Lat))+
    geom_point(data=warmDF, aes(y=Lat, x=Lon, color=warm),size=3)+
    scale_fill_continuous(name="Temperature (degree)", 
                          type="viridis")+
    scale_color_gradient2(name="Warm factor", 
                          low="blue", high="red")


pdf("output/cold_warm_factor.pdf", width=12,height=8)
plot_grid(p1, p2, labels="AUTO", ncol=1, align="v", axis="l")
dev.off()


### read in predictability df
predDF <- read.table("data/factors_to_plot_pred.txt", header=F)
colnames(predDF) <- c("SCCSID", "Lat", "Lon", "dry", "wet")

### plot precDF wet factor
p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=as.character(precDF$prec_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=predDF, aes(y=Lat, x=Lon, color=dry),size=3)+
    scale_fill_manual(name="Rainfall (mm/yr)", 
                      values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                      label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
    scale_color_gradient2(name="dry predictability",
                          low="blue", high="red")

p2 <- ggplot() + 
    geom_tile(data=ensoDF, aes(y=Lat, x=Lon, fill=as.character(ensoDF$enso_cat))) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    borders(colour = alpha("black", 0.8), lwd=0.2)+
    geom_point(data=predDF, aes(y=Lat, x=Lon, color=wet),size=3)+
    scale_fill_manual(name="ENSO index", 
                      values=alpha(c("indianred4", "indianred1","thistle1","skyblue","blue"),0.2),
                      label=c("-1 to -0.5", "-0.5 to -0.1", "-0.1 to 0.1", "0.1 to 0.5", "0.5 to 1"))+
    scale_color_gradient2(name="wet predictability",
                          low="red", high="blue")

pdf("output/predictability.pdf", width=12,height=8)
plot_grid(p1, p2, labels="AUTO", ncol=1, align="v", axis="l")
dev.off()


### diff figure - absolute minus percentile
wetDF$diff <- with(wetDF, wet_abs-wet_pct)

p1 <- ggplot() + 
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    borders(colour = alpha("black", 0.8), lwd=0.2)+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=diff))+
    theme(legend.position="right")+
    scale_color_gradient2(name="wet extreme difference",
                          low="blue", high="orange")

pdf("output/difference.pdf", width=12,height=8)

plot(p1)
dev.off()

### read in global predictability file
gpredDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")

### add cru grid onto wetDF, dtDF and coldDF
### step 1: round the lat lon and deduct from lat lon
wetDF$LatC <- as.character(wetDF$Lat)
test <- strsplit(wetDF$LatC, split = "\\.")
test2 <-do.call(rbind, test)
test2 <- data.frame(test2)
colnames(test2) <- c("lat1", "lat2")
test2$lat2 <- paste0(".", test2$lat2)
wetDF <- data.frame(wetDF, test2)

wetDF$LonC <- as.character(wetDF$Lon)
test <- strsplit(wetDF$LonC, split = "\\.")
test2 <-do.call(rbind, test)
test2 <- data.frame(test2)
colnames(test2) <- c("lon1", "lon2")
test2$lon2 <- paste0(".", test2$lon2)
wetDF <- data.frame(wetDF, test2)

wetDF$lat1 <- as.numeric(as.character(wetDF$lat1))
wetDF$lon1 <- as.numeric(as.character(wetDF$lon1))
wetDF$lat2 <- as.numeric(as.character(wetDF$lat2))
wetDF$lon2 <- as.numeric(as.character(wetDF$lon2))

### subsetting negative and positive grids
wetDF1 <- subset(wetDF, Lat < 0)
wetDF2 <- subset(wetDF, Lat >= 0)

wetDF1$Latcru <- ifelse(wetDF1$lat2 < 0.25, wetDF1$lat1 + 0.25, 
                        ifelse(wetDF1$lat2 > 0.75, wetDF1$lat1 - 1.25, wetDF1$lat1 - 0.75))
wetDF2$Latcru <- ifelse(wetDF2$lat2 < 0.25, wetDF2$lat1 - 0.25, 
                        ifelse(wetDF2$lat2 >= 0.75, wetDF2$lat1 + 0.75, wetDF2$lat1 + 0.25))

wetDF <- rbind(wetDF1, wetDF2)

wetDF1 <- subset(wetDF, Lon < 0)
wetDF2 <- subset(wetDF, Lon >= 0)
wetDF1$Loncru <- ifelse(wetDF1$lon2 < 0.25, wetDF1$lon1 + 0.25, 
                        ifelse(wetDF1$lon2 > 0.75, wetDF1$lon1 - 1.25, wetDF1$lon1 - 0.75))
wetDF2$Loncru <- ifelse(wetDF2$lon2 < 0.25, wetDF2$lat1 - 0.25, 
                        ifelse(wetDF2$lon2 >= 0.75, wetDF2$lon1 + 0.75, wetDF2$lon1 + 0.25))
wetDF <- rbind(wetDF1, wetDF2)

wetDF <- wetDF[complete.cases(wetDF$Loncru),]
wetDF <- wetDF[complete.cases(wetDF$Latcru),]

### extract datapoints from gpredDF 
lon.list <- unique(wetDF$Loncru, na.rm=T)
lat.list <- unique(wetDF$Latcru, na.rm=T)

plotDF1 <- merge(wetDF,gpredDF,by.x=c("Loncru","Latcru"), by.y=c("lon", "lat"))

### plot precDF wet factor
p1 <- ggplot() + 
    geom_tile(data=gpredDF, aes(y=lat, x=lon, fill=precM)) +
    coord_quickmap(xlim=range(gpredDF$lon), ylim=range(gpredDF$lat))+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=wet_factor))+
    scale_fill_continuous(name="Contingency", 
                          type="viridis")+
    scale_color_gradient2(name="wet factor")

p2 <- ggplot() +
    geom_point(data=plotDF1, aes(y=wet_factor, x=prec_annual_mean))


pdf("output/wet_factor_contingency_spatial.pdf")
plot(p1)
dev.off()

pdf("output/wet_factor_contingency_correlation.pdf")
plot(p2)
dev.off()
