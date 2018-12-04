### main program
source("prepare.R")


### read in prec data as background plot
precDF <- read.table("data/prec_annual.txt", header=F)
colnames(precDF) <- c("Lon", "Lat", "prec")

### read in wet factors
wetDF <- read.csv("data/wet_factor_fixed_plot.csv")
colnames(wetDF) <- c("SCCSID", "Lat", "Lon", "wet_factor", "wet_pct", "wet_abs")

### plot precDF wet factor
p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=prec)) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=wet_factor))+
    scale_fill_continuous(name="Rainfall (mm/yr)", 
                          type="viridis")+
    scale_color_gradient2(name="Wet factor")

p2 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=prec)) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=wet_pct))+
    scale_fill_continuous(name="Rainfall (mm/yr)", 
                          type="viridis")+
    scale_color_gradient2(name="Wet Extremes - percentile")

p3 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=prec)) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=wetDF, aes(y=Lat, x=Lon, color=wet_abs))+
    scale_fill_continuous(name="Rainfall (mm/yr)", 
                          type="viridis")+
    scale_color_gradient2(name="Wet Extremes - absolute")

#plot(p2)

pdf("output/wet_factors.pdf", width=12,height=10)
plot_grid(p1, p2, p3, labels="AUTO", ncol=1, align="v", axis="l")
dev.off()


### read in drought indices to plot
dtDF <- read.table("data/indices_to_plot_drought.txt", header=F)
colnames(dtDF) <- c("SCCSID", "Lat", "Lon", "PE", "PDSI")

### plot precDF wet factor
p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=prec)) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=dtDF, aes(y=Lat, x=Lon, color=PE))+
    scale_fill_continuous(name="Rainfall (mm/yr)", 
                          type="viridis")+
    scale_color_gradient2(name="PE")

p2 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=prec)) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=dtDF, aes(y=Lat, x=Lon, color=PDSI))+
    scale_fill_continuous(name="Rainfall (mm/yr)", 
                          type="viridis")+
    scale_color_gradient2(name="PDSI")

#plot(p2)

pdf("output/drought_indices.pdf", width=12,height=8)
plot_grid(p1, p2, labels="AUTO", ncol=1, align="v", axis="l")
dev.off()

### read in tair df
tairDF <- read.table("data/tair_annual.txt", header=F)
colnames(tairDF) <- c("Lon", "Lat", "tair")

coldDF <- read.table("data/factors_to_plot_cold.txt", header=F)
colnames(coldDF) <- c("SCCSID", "Lat", "Lon", "cold")

### plot tair cold
p1 <- ggplot() + 
    geom_tile(data=tairDF, aes(y=Lat, x=Lon, fill=tair)) +
    coord_quickmap(xlim=range(tairDF$Lon), ylim=range(tairDF$Lat))+
    geom_point(data=coldDF, aes(y=Lat, x=Lon, color=cold))+
    scale_fill_continuous(name="Temperature (degree)", 
                          type="viridis")+
    scale_color_gradient2(name="Cold factor")


pdf("output/cold_factor.pdf", width=12,height=6)
plot(p1)
dev.off()


### read in predictability df
predDF <- read.table("data/factors_to_plot_pred.txt", header=F)
colnames(predDF) <- c("SCCSID", "Lat", "Lon", "dry", "wet")

### plot precDF wet factor
p1 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=prec)) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=predDF, aes(y=Lat, x=Lon, color=dry))+
    scale_fill_continuous(name="Rainfall (mm/yr)", 
                          type="viridis")+
    scale_color_gradient2(name="dry predictability")

p2 <- ggplot() + 
    geom_tile(data=precDF, aes(y=Lat, x=Lon, fill=prec)) +
    coord_quickmap(xlim=range(precDF$Lon), ylim=range(precDF$Lat))+
    geom_point(data=predDF, aes(y=Lat, x=Lon, color=wet))+
    scale_fill_continuous(name="Rainfall (mm/yr)", 
                          type="viridis")+
    scale_color_gradient2(name="wet predictability")


pdf("output/predictability.pdf", width=12,height=8)
plot_grid(p1, p2, labels="AUTO", ncol=1, align="v", axis="l")
dev.off()

