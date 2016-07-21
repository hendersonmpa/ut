#!/usr/bin/R
library(ggplot2)
library(scales)
library(xtable)
library(e1071)
library(psych)

args <- commandArgs(TRUE)
analyte <- args[1] # Analyte name
number <- as.numeric(args[2]) # Top n tests
data_type <- args[3] # qual or quant

## analyte <- "ULC"
##analyte <- "TACRO"
## analyte <- "25VITD"
##analyte <- "FOL"
##analyte <- "SPE_INTERP"
#number <- 10
#data_type <- 'quant'

# results <- read.table("results.txt", sep ='\t', stringsAsFactors=FALSE)
results <- read.table("results.txt", sep =',', stringsAsFactors=FALSE)
results$V2 <- ifelse(results$V2==' NA', ' ', results$V2)

## Determine if the data is qualitative or quantitative
if (data_type == 'quant'){
  quant = TRUE
  results$V1 <- as.numeric(results$V1)
} else if (data_type == 'qual') {
  quant = FALSE
  results$V1 <- as.factor(results$V1)
}

## Settings
colours <- "skyblue4"
## Overview table #############
overview <- read.csv("summary.txt", header=FALSE)
colnames(overview) <- c("Group", "value")

overview.xtable <- xtable(overview,  caption =
      "Utilization statistics", label = "tab:overview",
      table.placement = "!htb", caption.placement = "top")
print(overview.xtable, include.rownames= FALSE , file = "figures/overview.tex", caption.placement='top')

## Histogram ###################
sk <- skewness(results$V1)

pdf(file = "figures/allhist.pdf")
if ( quant == TRUE & (abs(sk)<3)==TRUE) {
  h <- ggplot(results, aes(V1, fill = ..count..)) +
    geom_histogram()+ xlab("Concentration")
  h
} else if ( quant == TRUE & (abs(sk)>=3)==TRUE) {
  h <- ggplot(results, aes(V1, fill = ..count..)) +
    geom_histogram()+scale_x_log10()+ xlab("Concentration (log10 scaled)")
  h
} else {
  h <- ggplot(results, aes(V1, fill  = ..count..)) +
    geom_bar(stat='identity') + xlab("Category")
  h
}
dev.off()

pdf(file = "figures/allhist_flag.pdf")
if ( quant == TRUE & (abs(sk)<3)==TRUE) {
  h2 <- qplot(data=results, V1, fill=V2, xlab=paste0(analyte, ' Concentration'))+scale_fill_discrete(name='Abnormal Flag')
  h2
} else if ( quant == TRUE & (abs(sk)>=3)==TRUE) {
  h2 <- qplot(data=results, V1, fill=V2, xlab=paste0(analyte, ' Concentration'))+scale_x_log10()+scale_fill_discrete(name='Abnormal Flag')
  h2
} else {
  h2 <- ggplot(results, aes(V1, fill  = ..count..)) +
    geom_bar(stat='identity') + xlab("Category")
  h2
}
dev.off()


#cusum plot
cuplot <- qplot(results$V1, stat = "ecdf", geom = "step", xlab=paste(analyte, ' Concentration'), ylab='Cumulative Density')
ggsave(filename = "figures/ecdf.pdf", plot = cuplot)

## Describe data
description <- describe(results$V1)
print(xtable(description[,-c(1,6,13)], caption=paste('Statistical Summary of ', analyte)), file='figures/description.tex', include.rownames= F, caption.placement='top')

## Load the group data
data <- read.csv(file="ordered_list.csv",head=FALSE,sep=",")
colnames(data) <- c("group","names","count")
data$group <- as.factor(data$group)

## DOCS ##################################
## Histogram
docs <- subset(data, group == 'docs')
pdf(file = "figures/docshist.pdf")
h <- ggplot(docs, aes(count, fill  = ..count..)) +
  geom_histogram() +
  xlab("Number of tests ordered")
h
dev.off()

## Table
docs.xtable <- xtable(docs[1:number,2:3], caption = "Top ordering physicians. Count indicates the number of \"resulted\" tests ordered by that physician.", label = "tab:docs", table.placement = "htb",caption.placement = "top")
print(docs.xtable, include.rownames= FALSE,file = "figures/docs.tex", caption.placement='top')
## Bar plot
doc.names <- docs$names[1:number]
pdf(file = "figures/docsbar.pdf")
bb <- ggplot(docs[1:number,], aes(x = names, y = count)) +
  geom_bar(fill = colours, stat='identity') +
  scale_fill_manual(values = colours, guide="none") +
  scale_x_discrete(limits = doc.names, breaks = doc.names, labels = doc.names) +
  xlab("Physicians") +
  ylab("Orders") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bb
dev.off()

## LOCS ############################
## Histogram
locs <- subset(data, group == 'locs')
pdf(file = "figures/locshist.pdf")
h <- ggplot(locs, aes(count, fill  = ..count..)) +
  geom_histogram() +
  xlab("Number of tests ordered")
h
dev.off()
if (number > length(locs$names)){
  end <- length(locs$names)
}else{
  end <- number
}
## Table
locs.xtable <- xtable(locs[1:end,2:3], caption = "Top locations. Count indicates the number of \"resulted\" tests ordered by that location.", label = "tab:locs", table.placement = "htb",caption.placement = "top")

print(locs.xtable, include.rownames= FALSE, file = "figures/locs.tex", caption.placement='top')

## Bar plot
locs.names <- locs$names[1:end]
pdf(file = "figures/locsbar.pdf")
bb <- ggplot(locs[1:number,], aes(x = names, y = count)) +
  geom_bar(fill = colours, stat='identity') +
  scale_fill_manual(values = colours, guide="none") +
  scale_x_discrete(limits = locs.names, breaks = locs.names, labels = locs.names) +
  xlab("Locations") +
  ylab("Orders") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bb
dev.off()

## PATS #################################
## Histogram
pats <- subset(data, group == 'pats')
pdf(file = "figures/patshist.pdf")
h <- ggplot(pats, aes(count, fill  = ..count..)) +
  geom_histogram() +
  xlab("Number of tests ordered")
h
dev.off()

## Table
pats.xtable <- xtable(pats[1:number,2:3], caption =
                      "Top patients. Count indicates the number of unique test results for that patient.",label = "tab:pats", table.placement = "htb",caption.placement = "top")

print(pats.xtable, include.rownames= FALSE, file = "figures/pats.tex", caption.placement='top')

## Bar plot
pats.names <- pats$names[1:number]
pdf(file = "figures/patsbar.pdf")
bb <- ggplot(pats[1:number,], aes(x = names, y = count)) +
  geom_bar(fill = colours, stat='identity') +
  scale_fill_manual(values = colours, guide="none") +
  scale_x_discrete(limits = pats.names, breaks = pats.names, labels = pats.names) +
  xlab("Patients") +
  ylab("Orders") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bb
dev.off()

## Time series of the patient data ########################
data.patients <- read.csv(file="top_mrns.csv", header=FALSE, sep=",", colClasses = "character")
colnames(data.patients) <-
  c('clean','number','date','mrn','loc','doc')

data.patients$number <- as.numeric(data.patients$number)
data.patients$date <- as.Date(data.patients$date)

pdf(file = "figures/timeseries.pdf")
if (quant == TRUE) {
  h <- ggplot(data.patients, aes(date, mrn)) +
    geom_point(aes(colour = doc , size = number)) +
    scale_colour_discrete(name = "Physician",l=45) +
    scale_size_continuous(name = "Concentration") +
    scale_y_discrete(limits = pats.names, breaks = pats.names, labels = pats.names) +
    scale_x_date(labels = date_format("%b-%Y"),breaks = "1 month", minor_breaks = "1 week") +
    xlab("Time (months)") +
    ylab("Patient") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  h
}else{
  h <- ggplot(data.patients, aes(date, mrn)) +
    geom_point(aes(colour = doc, shape = clean), size=3) +
    scale_colour_discrete(name = "Physician", l=45) +
    scale_shape_discrete(name = "Result") +
    scale_y_discrete(limits = pats.names, breaks = pats.names, labels = pats.names) +
    scale_x_date(labels = date_format("%b-%Y"),breaks = "1 month", minor_breaks = "1 week") +
    xlab("Time (months)") +
    ylab("Patient") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  h
}
dev.off()


## ## Patient means over time period
## quartz()
## h <- ggplot(data, aes(DRW_DAT, DET_RES, colour = factor(data$ooids))) +
##   geom_point() +
##   scale_x_date(format = "%b", major="months", minor="3 days") +
##   geom_smooth(method="lm", formula=y~x) +
##  # geom_smooth(aes(x = DRW_DAT, y = DET_RES), stat="identity") +
##   opts(title = "25-OH Vitamin D Time Series")
## h
