#!/usr/bin Rscript
inputArgs <- commandArgs(trailingOnly = TRUE)

setwd('..')

source('hyperparameterTuning.r')

nRange <- seq(as.numeric(inputArgs[1]), as.numeric(inputArgs[2]), 10)
biasRange <- seq(as.numeric(inputArgs[3]), as.numeric(inputArgs[4]), .1)
threadId <- as.numeric(inputArgs[5])

exp2Power(nRange = nRange, biasRange = biasRange, nSims = 50, threadId = threadId)
