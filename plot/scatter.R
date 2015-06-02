
set.seed(955)
library(tcltk)

args <- commandArgs(trailingOnly = TRUE)

x_dat=as.numeric(args[1])

y_dat=as.numeric(args[2])

x11(width=7, height = 7)

# Make some noisily increasing data
dat <- data.frame(cond = rep(c("A", "B"), each=10),
                  xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3))
head(dat)

library(ggplot2)

ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_point(shape=1)

prompt  <- "hit spacebar to close plots"
extra   <- "some extra comment"
capture <- tk_messageBox(message = prompt, detail = extra)