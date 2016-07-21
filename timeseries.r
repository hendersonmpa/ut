#!/usr/bin/R
library("ggplot2")
library("scales")

## Time series of the patient data ########################
args <- commandArgs(TRUE)
data_type <- args[1] # quantitative t or nil

setwd("~/lisp/site/ut/")
data.patients <- read.csv(file="./figures/top_mrns.csv", header=FALSE, sep=",", colClasses = "character")
## colnames(data.patients) <- c('clean','number','date','mrn','loc','doc') 
colnames(data.patients) <- c('mrn','doc','loc','date','result', 'clean_result')
##data_type <- 'T'
##data_type <- 'NIL'
## Determine if the data is qualitative or quantitative
if (data_type == 'T'){
    quant = TRUE
    data.patients$clean_result <- as.numeric(data.patients$clean_result)
} else if (data_type == 'NIL') {
    quant = FALSE
    data.patients$result <- as.factor(data.patients$result)
}

data.patients$date <- as.Date(data.patients$date)

png(file = "figures/timeseries.png", width = 1400, heigh = 865)
if (quant == TRUE) {
    h <- ggplot(data.patients, aes(date, mrn)) +
        geom_point(aes(colour = doc , size = clean_result)) +
        scale_colour_discrete(name = "Physician",l=45) +
        scale_size_continuous(name = "Concentration") +
    #scale_y_discrete(limits = pats.names, breaks = pats.names, labels = pats.names) +
        scale_x_date(labels = date_format("%b-%Y"),
                     breaks = date_breaks("1 month"),
                     minor_breaks = date_breaks("1 week")) +
        xlab("Time (months)") +
        ylab("Patient") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    h
}else{
    h <- ggplot(data.patients, aes(date, mrn)) +
        geom_point(aes(colour = doc, shape = result), size=3) +
        scale_colour_discrete(name = "Physician", l=45) +
        scale_shape_discrete(name = "Result") +
                                        #scale_y_discrete(limits = pats.names, breaks = pats.names, labels = pats.names) + 
        scale_x_date(labels = date_format("%b-%Y"),
                     breaks = date_breaks("1 month"),
                     minor_breaks = date_breaks("1 week")) +
        xlab("Time (months)") +
        ylab("Patient") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    h
}
dev.off()
