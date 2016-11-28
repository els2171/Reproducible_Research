## Download US medical expenditures csv dataset and read file into R.

fileURL <- "https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1480464000&Signature=WbAoLmDBb3wDldOewrxtII0BbsHF4MPi4PFsPhhOM1dhEqwQus-Wj7qN7cbovisNW6xROxveQtITivj13HhyNRBjF16PhSAH5BEYQK2iMBn9N29dMBq-7kHgQ7scsXSCyLj9UrcegZEjg0WPHsd0mfnEFntCy09CJL~ys~htJL0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file(fileURL, destfile = "./medexp.csv")
dateDownloaded <- date()
medexp <- read.csv("medexp.csv")

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
