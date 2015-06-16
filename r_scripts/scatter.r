#!/usr/bin/Rscript

library(methods)
library(grDevices)
library(reshape2)
library(stats)
library(ggplot2)
library(plyr)
library(scales)
library(poweRlaw)


mean_annual <- read.csv("../data/2015_5_29_12_35_43_mean_annual/mean_annual_data.csv", header = TRUE, sep=",")
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


pdf("area_v_water.pdf")
y_lab = expression(Discharge (m^{3}/s))
x_lab = expression(Area (km^{2}))

dd = mean_annual[complete.cases(mean_annual), ]
dd$area
dd$water_discharge
df <- data.frame(x = dd$area, y = dd$water_discharge)
# qw_stat <- lm("log(area) ~ log(water_discharge)", data=mean_annual)
# coefs = coef(qw_stat)
# ic = coefs[1]
# print(sprintf("Intercept: %s",ic))
# sl = coefs[2]
# print(sprintf("Slope: %s",sl))

# get_n <- function(t1,t2,m1,m2){(log10(t2) - log10(t1))/(log10(m2) - log10(m1))}
# get_y <- function(x, sl, ic) {sl * x + ic}
# 
# x1 = 1
# x2 = 500
# y1 = get_y(x1, sl, ic)
# y2 = get_y(x2, sl, ic)
# 
# n_guess <- get_n(x1,y1,x2,y2)
# k_guess = 10^ic
# print(sprintf("n_guess: %s",n_guess))
# print(sprintf("k_guess: %s",k_guess))
# 
# 
nls_stat <- nls(y~I*x^e, data = df, start = list(I=0.075, e = 0.8))
resid <- summary(nls_stat)
resid
coefs = coef(nls_stat)
k_guess = coefs[1]
print(sprintf("k: %s",k_guess))
n_guess = coefs[2]
print(sprintf("n: %s",n_guess))

pf <- function(x) {k_guess * x ^ n_guess}
plf <- function(x) {n_guess * log(x) + log(k_guess)}
lf <- function(x) {sl * x + ic }
dd$z <- sapply(dd$area, pf)

qw_stat <- lm("area ~ z", data=dd)
coefs = coef(qw_stat)
ic = coefs[1]
print(sprintf("Intercept: %s",ic))
sl = coefs[2]
print(sprintf("Slope: %s",sl))

ql_stat <- lm(water_discharge ~ sl*area+ic , data=dd)
ql_stat

p <- ggplot(dd, aes_string(x = "area", y = "water_discharge")) + theme_bw() +
	ggtitle("Area vs. Water Discharge") +
	geom_point() +
	stat_smooth(method = "lm", formula = 'y ~ sl*x+ic') +
	scale_y_log10(y_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	scale_x_log10(x_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x)))
	
p




pdf("area_v_qs.pdf")
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