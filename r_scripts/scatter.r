#!/usr/bin/Rscript

library(methods)
library(grDevices)
library(reshape2)
library(stats)
library(ggplot2)
library(plyr)
library(scales)
require(grid)

mean_annual <- read.csv("../data/2015_5_29_12_35_43_mean_annual/mean_annual_data.csv", header = TRUE, sep=",")


borrego <- read.csv("../data/anza-borrego/anza_borrego_combined.csv", header = TRUE, sep=",")
corinth <- read.csv("../data/corinth.csv", header = TRUE, sep=",")

# Borrego LGM Data
#borrego <- read.csv("../data/anza-borrego/Borrego_data/Mean_Annual_Combine.csv")
borrego_ccsm4_max <- read.csv("../data/anza-borrego/Borrego_data/max_t_lgm_ccsm4_combined.csv")
borrego_miroc_max <- read.csv("../data/anza-borrego/Borrego_data/max_t_lgm_miroc_combined.csv")
borrego_mpi_max <- read.csv("../data/anza-borrego/Borrego_data/max_t_lgm_mpi_combined.csv")
borrego_ccsm4_min <- read.csv("../data/anza-borrego/Borrego_data/min_t_lgm_ccsm4_combined.csv")
borrego_miroc_min <- read.csv("../data/anza-borrego/Borrego_data/min_t_lgm_miroc_combined.csv")
borrego_mpi_min <- read.csv("../data/anza-borrego/Borrego_data/min_t_lgm_mpi_combined.csv")

borrego_ccsm4_max$scenario <- as.factor("Max T")
borrego_ccsm4_min$scenario <- as.factor("Min T")

borrego_ccsm4_max$model <- as.factor("CCSM4")
borrego_ccsm4_min$model <- as.factor("CCSM4")

borrego_ccsm4_max$location <- as.factor(2)
borrego_ccsm4_min$location <- as.factor(2)

borrego_miroc_min$scenario <- as.factor("Min T")
borrego_miroc_max$scenario <- as.factor("Max T")
borrego_miroc_min$model <- as.factor("MIROC")
borrego_miroc_max$model <- as.factor("MIROC")

borrego_miroc_min$location <- as.factor(2)
borrego_miroc_max$location <- as.factor(2)


borrego_mpi_min$scenario <- as.factor("Min T")
borrego_mpi_max$scenario <- as.factor("Max T")
borrego_mpi_min$model <- as.factor("MPI")
borrego_mpi_max$model <- as.factor("MPI")

borrego_mpi_min$location <- as.factor(2)
borrego_mpi_max$location <- as.factor(2)

# DV LGM Data
#<- read.csv("../data/2015_5_29_12_35_43_mean_annual/mean_annual_data.csv")
dv_ccsm4_max <- read.csv("../data/2015_5_29_12_36_59_max_t_lgm_ccsm4/max_t_lgm_ccsm4_data.csv")
dv_ccsm4_min <- read.csv("../data/2015_5_29_12_38_11_min_t_lgm_ccsm4/min_t_lgm_ccsm4_data.csv")
dv_mpi_min <- read.csv("../data/2015_5_29_12_38_58_min_t_lgm_mpi/min_t_lgm_mpi_data.csv")
dv_mpi_max <- read.csv("../data/2015_5_29_12_40_5_max_t_lgm_mpi/max_t_lgm_mpi_data.csv")
dv_miroc_min <- read.csv("../data/2015_5_29_12_41_17_min_t_lgm_miroc/min_t_lgm_miroc_data.csv")
dv_miroc_max <- read.csv("../data/2015_5_29_12_42_0_max_t_lgm_miroc/max_t_lgm_miroc_data.csv")

dv_ccsm4_max$scenario <- as.factor("Max T")
dv_ccsm4_max$model <- as.factor("CCSM4")
dv_ccsm4_max$location <- as.factor(1)

dv_ccsm4_min$scenario <- as.factor("Min T")
dv_ccsm4_min$model <- as.factor("CCSM4")
dv_ccsm4_min$location <- as.factor(1)

dv_miroc_min$scenario <- as.factor("Min T")
dv_miroc_min$model <- as.factor("MIROC")
dv_miroc_min$location <- as.factor(1)

dv_miroc_max$scenario <- as.factor("Max T")
dv_miroc_max$model <- as.factor("MIROC")
dv_miroc_max$location <- as.factor(1)

dv_mpi_min$scenario <- as.factor("Min T")
dv_mpi_min$model <- as.factor("MPI")
dv_mpi_min$location <- as.factor(1)

dv_mpi_max$scenario <- as.factor("Max T")
dv_mpi_max$model <- as.factor("MPI")
dv_mpi_max$location <- as.factor(1)

scenerio_list = list(dv_ccsm4_max, dv_ccsm4_min,dv_miroc_min,dv_miroc_max, 
	dv_mpi_max, dv_mpi_min, borrego_ccsm4_max, borrego_ccsm4_min,
	borrego_mpi_min, borrego_mpi_max, borrego_miroc_min, borrego_miroc_max)

climate_scenarios <- join_all(scenerio_list, type = "full")

variable_index <- c(2, 6, 8, 10, 11, 15, 17, 18, 19, 20, 21, 22, 23)
variable_index_borrego <- variable_index+1

rename_cols <-c("precip", "water_discharge", "area", "relief", "temp", "Qs", "erosion", 
	"slip_min", "slip_max", "Qs_T_min", "Qs_T_max", "fault_id", "fault_name")

variable_index_corinth <- c(2, 5, 7, 8, 9)
corinth_cols <-c("water_discharge", "area", "relief", "temp", "Qs")

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
	names(climate_scenarios)[variable_index[i]] <- rename_cols[i]
	names(borrego)[variable_index_borrego[i]] <- rename_cols[i]
}


for (i in 1:length(variable_index_corinth) ) {
	names(corinth)[variable_index_corinth[i]] <- corinth_cols[i]
}

# Reverse calculate pp/yr for corinth
qw_to_precip <- function(area_km, qw) {((qw*(60*60*24*365))/(area_km*(1000000)))*1000}
corinth$precip <- mapply(qw_to_precip, corinth$area, corinth$water_discharge)

death_valley_m = mean_annual[complete.cases(mean_annual), ]
death_valley_m$location <- as.factor(2)

anza_full = borrego[complete.cases(borrego), ]
anza_full$location <- as.factor(1)

anza_dv_m <- merge(death_valley_m, anza_full, all=TRUE)

dv <- data.frame(x = death_valley_m$area, y = death_valley_m$Qs, p = death_valley_m$precip, w = death_valley_m$water_discharge)
dv$group <- as.factor(1)

anza <- data.frame(x = borrego$area, y = borrego$Qs, p = borrego$precip, w = borrego$water_discharge)
anza = anza[complete.cases(anza), ]

anza$group <- as.factor(2)
anza_dv <- merge(dv, anza, all=TRUE)

cc <- data.frame(x = corinth$area, y = corinth$Qs, p = corinth$precip, w = corinth$water_discharge)
cc = cc[complete.cases(cc), ]
cc$group <- as.factor(3)

all <- merge(anza_dv, cc, all=TRUE)
all_w <- data.frame(x = all$x, y = all$w)

y_lab = expression(Discharge (m^{3}/s))
x_lab = expression(Area (km^{2}))


# Corinth power law

cc_p <- data.frame(x = cc$x, y = cc$w)
nls_stat <- nls(y~I*(x^e), data = cc_p, start = list(I=0.075, e = 0.8))
resid <- sum(residuals(nls_stat)^2)
coefs <- coef(nls_stat)
sums <- sum((cc_p$y - mean(cc_p$y))^2)
r2_c <- (1 - (resid/sums))

k_guess_co = coefs[1]
n_guess_co = coefs[2]
k_guess = k_guess_co
n_guess = n_guess_co

pf <- function(x) {k_guess * x ^ n_guess}
plf <- function(x) {n_guess * log(x) + log(k_guess)}
lf <- function(x) {sl * x + ic }

cc$z <- sapply(cc$x, pf)


# Anza power law

an_p <- data.frame(x = anza$x, y = anza$w)
nls_stat <- nls(y~I*(x^e), data = an_p, start = list(I=0.075, e = 0.8))
resid <- sum(residuals(nls_stat)^2)
coefs <- coef(nls_stat)
sums <- sum((an_p$y - mean(an_p$y))^2)
r2_anza <- (1 - (resid/sums))

k_guess_anza = coefs[1]
n_guess_anza = coefs[2]
k_guess = k_guess_anza
n_guess = n_guess_anza

anza$z <- sapply(anza$x, pf)


# DV power law

dv_p <- data.frame(x = dv$x, y = dv$w)
nls_stat <- nls(y~I*(x^e), data = dv_p, start = list(I=0.075, e = 0.8))
resid <- sum(residuals(nls_stat)^2)
coefs <- coef(nls_stat)
sums <- sum((dv_p$y - mean(dv_p$y))^2)
r2dv <- (1 - (resid/sums))

k_guess_dv = coefs[1]
n_guess_dv = coefs[2]
k_guess = k_guess_dv
n_guess = n_guess_dv

dv$z <- sapply(dv$x, pf)

qw_stat <- lm("x ~ z", data=cc)
coefs = coef(qw_stat)
ic = coefs[1]
print(sprintf("Intercept: %s", ic))
sl = coefs[2]
print(sprintf("Slope: %s", sl))


ylab = expression('Discharge '(m^{3}/s))
xlab = expression('Area '(km^{2}))

lb1 <- paste("R^2 == ", round(r2dv, 2))
lb2 <- paste("y == ", round(k_guess_dv,2), "*x^", round(n_guess_dv, 2))
dv1 = annotate("text",  x = 140, y = 0.4, label = lb1, parse=TRUE, colour="red3")
dv2 = annotate("text",  x = 130, y = 0.25, label = lb2, parse=TRUE, colour="red3")

lb3 <- paste("R^2 == ", round(r2_anza, 2))
lb4 <- paste("y == ", round(k_guess_anza,2), "*x^", round(n_guess_anza, 2))
an1 = annotate("text",  x = 4, y = 0.02, label = lb3, parse=TRUE, colour="green3")
an2 = annotate("text",  x = 5, y = 0.015, label = lb4, parse=TRUE, colour="green3")

lb5 <- paste("R^2 == ", round(r2_c, 3))
lb6 <- paste("y == ", round(k_guess_co,2), "*x^", round(n_guess_co, 2))
co1 = annotate("text",  x = 6, y = 0.6, label = lb5, parse=TRUE, colour="blue3")
co2 = annotate("text",  x = 6, y = 0.4, label = lb6, parse=TRUE, colour="blue3")

p <- ggplot(all, aes_string(x = "x", y = "w")) +
	ggtitle("Area vs. Water Discharge") +
	geom_point(aes(size = p, colour=factor(group, labels=c("Death Valley", "Anza-Borrego", "Corinth")))) +
	scale_size_continuous(name="Precipitation\n(mm/yr)") +
	scale_shape(solid = FALSE) +
	geom_line(aes_string(x = "x", y = "z"), data=cc, colour="blue") +
	geom_line(aes_string(x = "x", y = "z"), data=anza, colour="green4") +
	geom_line(aes_string(x = "x", y = "z"), data=dv, colour="red") +
	scale_colour_hue(l=80, c=40) +
	scale_y_log10(ylab, breaks=c(0.01,0.02,0.04, 0.1, 0.2, 0.4, 0.6, 1, 2, 4, 10, 20, 30)) +
	scale_x_log10(xlab, breaks=c(1,2,4,10,20,40,60,100, 200, 400, 1000)) +
	annotation_logticks(scaled = TRUE) +
	theme_bw() +
	labs(colour="Area") +
	dv1 +
	dv2 +
	an1 +
	an2 +
	co1 +
	co2 +
	theme(panel.grid.minor = element_blank(), legend.key = element_blank())

ggsave("area_vs_water.pdf", width = 10, height = 9, dpi = 220)


anza_dv_m_p <- data.frame(x = anza_dv_m$area, y = anza_dv_m$Qs)
nls_stat <- nls(y~I*(x^e), data = anza_dv_m_p, start = list(I=200, e = 1))
resid <- sum(residuals(nls_stat)^2)
coefs <- coef(nls_stat)
sums <- sum((anza_dv_m_p$y - mean(anza_dv_m_p$y))^2)
r2andv <- (1 - (resid/sums))

k_guess_anza_dv_m = coefs[1]
n_guess_anza_dv_m = coefs[2]
k_guess = k_guess_anza_dv_m
n_guess = n_guess_anza_dv_m

anza_dv_m$z <- sapply(anza_dv_m$area, pf)

ll5 <- paste("R^2 == ", round(r2andv, 3))
ll6 <- paste("y == ", round(k_guess_anza_dv_m,2), "*x^", round(n_guess_anza_dv_m, 2))
andv1 = annotate("text",  x = 5, y = 10000, label = ll5, parse=TRUE, colour="black")
andv2 = annotate("text",  x = 6, y = 7500, label = ll6, parse=TRUE, colour="black")

ylab = expression('Qs '(m^{3}/yr))
xlab = expression('Area '(km^{2}))

p <- ggplot(anza_dv_m, aes_string(x = "area", y = "Qs")) +
	ggtitle("Area vs. Qs") +
	geom_point(aes(size = precip, colour = temp, shape = factor(location, labels=c("Death Valley", "Anza-Borrego")))) +
	scale_y_log10(ylab) +
	scale_x_log10(xlab) +
	scale_colour_gradient2( mid="blue2", high="red2") +
	scale_shape_manual(values = c(17,16)) +
	andv1 +
	andv2 +
	annotation_logticks(scaled = TRUE) +
	geom_line(aes_string(x = "area", y = "z"), colour="black") +
	labs(colour="Average Temperature (C)", shape ="Location", size = "Precipitation (mm/yr)") +
	theme_bw() +
	theme(panel.grid.minor = element_blank(), legend.key = element_blank())

ggsave("fault_sep.pdf", width = 10, height = 9, dpi = 220)

palette <- c("red4", "green4", "lightblue1")

all_qs <- data.frame(x = all$x, y = all$y)
nls_stat <- nls(y~I*(x^e), data = all_qs, start = list(I=200, e = 1))
resid <- sum(residuals(nls_stat)^2)
coefs <- coef(nls_stat)
sums <- sum((all_qs$y - mean(all_qs$y))^2)
k_guess_a = coefs[1]
n_guess_a = coefs[2]
k_guess = k_guess_a
n_guess = n_guess_a

all$z <- sapply(all$x, pf)

r2all <- (1 - (resid/sums))
r2all

ylab = expression('Qs '(m^{3}/yr))
xlab = expression('Area '(km^{2}))

lb7 <- paste("R^2 == ", round(r2all, 3))
lb8 <- paste("y == ", round(k_guess_a,2), "*x^", round(n_guess_a, 2))
a1 = annotate("text",  x = 100, y = 10000, label = lb5, parse=TRUE, colour="blue3")
a2 = annotate("text",  x = 110, y = 7000, label = lb6, parse=TRUE, colour="blue3")

p <- ggplot(all, aes_string(x = "x", y = "y")) + theme_bw() +
	scale_y_log10(y_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	scale_x_log10(x_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	ggtitle("Area vs. Sediment Flux") +
	geom_point(aes(alpha=0.5,shape = factor(group, labels=c("Death Valley", "Anza-Borrego", "Corinth")), order = sample(seq_along(x)))) +
	labs(shape="Area") +
	annotation_logticks(scaled = TRUE) +
	a1 +
	a2 +
	geom_line(aes_string(x = "x", y = "z"), data=all, colour="blue") +
	scale_shape_manual(values=3:5) +
	scale_colour_manual(values=palette) +
	theme(panel.grid.minor = element_blank(), legend.key = element_blank())

ggsave("area_vs_qs.pdf", width = 10, height = 9, dpi = 220)


pdf("bqart_vs_tectonics.pdf")

p <- ggplot(mean_annual, aes_string(x = "Qs_T_min", y = "Qs", color="fault_name")) + theme_bw() +
	scale_y_log10(y_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	scale_x_log10(x_lab, breaks = trans_breaks("log10", function(x) 10^x), 
		labels = trans_format("log10", math_format(10^.x))) +
	ggtitle("BQART vs. Tectonic Qs") +
	geom_point()
p

p <- ggplot(climate_scenarios, aes_string(x = "area", y = "Qs")) +
	ggtitle("Climate Scenarios") +
	facet_grid(. ~ model) +
	scale_y_log10(ylab) +
	scale_x_log10(xlab) +
	geom_line(aes_string(x = "area", y = "z"), data = anza_dv_m, colour="black") +
	geom_point(aes(shape=factor(location, labels=c("Death Valley", "Anza-Borrego")), color=factor(scenario))) + 
	labs(shape ="Location") +
	annotation_logticks(scaled = TRUE) +
	theme_bw() +
	theme(panel.grid.minor = element_blank(), legend.key = element_blank())

ggsave("climate_scenarios.pdf", width = 16, height = 6, dpi = 220)

dev.off()