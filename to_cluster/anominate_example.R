setwd("~/Box Sync/Measurement/2014")
library(anominate)
library(foreign)

sen <- read.dta("sen112kh.dta")
senvotes <- sen[,-c(1:9)]
rownames(senvotes) <- paste(sen$name,sen$lstate,sep=" ")

senvotes.rc <- rollcall(senvotes, yea=1, nay=6, notInLegis=0, missing=c(7,9), legis.names=paste(sen[,9],sen[,5]))
sen.nominate <- anominate(senvotes.rc, dims=1)