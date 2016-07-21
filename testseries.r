#!/usr/bin/R
library("ggplot2")
library("scales")

## Time series of the patient data ########################
quant <- TRUE
setwd("~/lisp/site/ut/")
data.patients <- read.csv(file="./figures/top_tests.csv", header=FALSE, sep=",", colClasses = "character")
## colnames(data.patients) <- c('clean','number','date','mrn','loc','doc') 
colnames(data.patients) <- c('test','doc','loc','date')


data.patients$date <- as.Date(data.patients$date)

png(file = "figures/testseries.png",  width = 1400, height = 865)
h <- ggplot(data.patients, aes(date, test)) +
    geom_point(aes(colour = doc)) +
    scale_colour_discrete(name = "Physician",l=45) +
                                        #scale_y_discrete(limits = pats.names, breaks = pats.names, labels = pats.names) +
    scale_x_date(labels = date_format("%b-%Y"),
                 breaks = date_breaks("1 month"),
                 minor_breaks = date_breaks("1 week")) +
    xlab("Time (months)") +
    ylab("Test") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
h
dev.off()
