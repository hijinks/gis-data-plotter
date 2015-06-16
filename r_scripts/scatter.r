#!/usr/bin/Rscript

library(methods)
library(grDevices)
library(reshape2)
library(stats)
library(ggplot2)
library(plyr)
library(scales)

mean_annual <- read.csv("/Users/sambrooke/Repos/gis-data-plotter/data/2015_5_29_12_35_43_mean_annual/mean_annual_data.csv", header = TRUE, sep=",")
variable_index <- c(2, 6, 8, 10, 11, 15, 17, 18, 19, 20, 21, 22, 23)
rename_cols <-c("precip", "water_discharge", "area", "relief", "temp", "Qs", "erosion", 
	"slip_min", "slip_max", "Qs_T_min", "Qs_T_max", "fault_id", "fault_name")

# Separated by faults
grapevine <- mean_annual[mean_annual$fault.name == 'Grapevine Fault',]
black_mountain <- mean_annual[mean_annual$fault.name == 'Black Mountain Fault Zone',]
keane_wonder <- mean_annual[mean_annual$fault.name == 'Keane Wonder Fault',]
towne_pass <- mean_annual[mean_annual$fault.name == 'Towne Pass Fault',]
unnamed_west <- mean_annual[mean_annual$fault.name == 'Unamed West DV Fault zone',]

for (i in 1:length(variable_index) ) {
	names(mean_annual)[variable_index[i]] <- rename_cols[i]
	names(grapevine)[variable_index[i]] <- rename_cols[i]
	names(black_mountain)[variable_index[i]] <- rename_cols[i]
	names(keane_wonder)[variable_index[i]] <- rename_cols[i]
	names(unnamed_west)[variable_index[i]] <- rename_cols[i]
}


pdf("area_v_qs.pdf")

y_lab = expression(Discharge (m^{3}/s))
x_lab = expression(Area (km^{2}))

plot(mean_annual$area, mean_annual$water_discharge)

df <- data.frame(x = mean_annual$area, y = mean_annual$water_discharge)

pl <- nls(y ~ I(x^power), data=df, start = list(power = 1), trace = T)

lines(s, predict(m, list(x = s)), col = "green")

p <- ggplot(mean_annual, aes_string(x = "area", y = "Qs")) + theme_bw() +
	scale_y_log10(y_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	scale_x_log10(x_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	ggtitle("Area vs. Sediment Flux") +
	geom_point() +
	stat_smooth(method = "lm", formula = 'y ~ x')

p


pdf("bqart_vs_tectonics.pdf")

p <- ggplot(mean_annual, aes_string(x = "Qs_T_min", y = "Qs", color="fault_name")) + theme_bw() +
	scale_y_log10(y_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	scale_x_log10(x_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	ggtitle("BQART vs. Tectonic Qs") +
	geom_point()
p

dev.off()