## Load required libraries.

library(dplyr)

## Download US medical expenditures csv dataset and read file into R.

fileURL <- "https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1480464000&Signature=WbAoLmDBb3wDldOewrxtII0BbsHF4MPi4PFsPhhOM1dhEqwQus-Wj7qN7cbovisNW6xROxveQtITivj13HhyNRBjF16PhSAH5BEYQK2iMBn9N29dMBq-7kHgQ7scsXSCyLj9UrcegZEjg0WPHsd0mfnEFntCy09CJL~ys~htJL0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file(fileURL, destfile = "./medexp.csv")
dateDownloaded <- date()
medexp <- read.csv("medexp.csv")



#############################
###  Plot 1: Data for NY  ###
#############################

## Subset data only for NY state.

NYmed <- subset(medexp, Provider.State == "NY")


## Build graph displaying the relationship bw Mean Covered Costs and Mean
## Total Payments for NY state. Scale using log for better visualization.
## Save plot as plot1.png.

pdf(file = "plot1.pdf")
with(NYmed, plot(x = log(Average.Covered.Charges), y = log(Average.Total.Payments), 
                 pch = 20, col = rgb(0, 0, 1, 0.3), 
                 main = "Medical Expenditures in NY", 
                 xlab = "log of Mean Covered Charges ($)", 
                 ylab = "log of Mean Total Payments ($)"))
model <- lm(log(Average.Total.Payments) ~ log(Average.Covered.Charges), data = NYmed)
abline(model, lwd = 2)
dev.off()



###########################################
###  Plot 2: Data by State & Condition  ###
###########################################

## Create subset including Condition, State, Avg Covered Charges, Avg Total Payments.

medexp2 <- select(medexp, DRG.Definition, Provider.State, 
                  Average.Covered.Charges, Average.Total.Payments)

## Start pdf plot

pdf(file = "plot2.pdf")


## Nested for loops to plot 6 scatterplots, one for each state in a 2x3 configuration.
## The loess regression will be plotted for each medical condition on each plot.

par(mfrow = c(2,3), mar = c(3, 3, 3, 0), oma = c(4, 4, 4, 4))

for (s in c(1:6)) {
        with(medexp2, plot(Average.Covered.Charges[Provider.State == levels(Provider.State)[s]], 
                           Average.Total.Payments[Provider.State == levels(Provider.State)[s]],
                           col = "grey83", 
                           xlab = "", 
                           ylab = "", 
                           main = levels(Provider.State)[s],
                           ylim = c(2500, 30000),
                           xlim = c(0, 200000)))
        
        for (i in c(1:6)) {
                with(subset(medexp2, Provider.State == levels(Provider.State)[s]), 
                     lines(lowess(Average.Covered.Charges[DRG.Definition == levels(DRG.Definition)[i]], 
                                  Average.Total.Payments[DRG.Definition == levels(DRG.Definition)[i]]), 
                                  col = i, 
                                  lwd = 2))
        }
}


## Add universal labels for all plots.

mtext("Medical Expenditures by State", 
      side = 3, 
      outer = TRUE)
mtext("Mean Covered Charges", 
      side = 1, 
      outer = TRUE)
mtext("Mean Total Payments", 
      side = 2, 
      outer = TRUE)


## Make universal legend below all plots.

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("194", "292", "392", "641", "690", "871"), 
       title = "Medical Condition",
       xpd = TRUE, 
       horiz = TRUE, 
       inset = c(0,0), 
       col = 1:6, 
       lty = 1, 
       lwd = 2)

dev.off()