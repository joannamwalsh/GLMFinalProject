library(tidyverse)
library(jpeg)
library(ggplot2)
library(imager)
library(ggpubr)
library(fields)
library(table1)

## View a single image, its resized form, and its standardized form:

image = load.image(file = "/Users/joannawalsh/Desktop/Dataset/Very_Mild_Demented/verymild_9.jpg")

smallimage <- resize(image,round(width(image)/3),round(height(image)/3))

X <- as.matrix(as.matrix(smallimage)[, -ncol(as.matrix(smallimage))])
train_means <- apply(X, 2, mean)
train_sd <- sd(as.vector(X))
X <- sweep(X, 2L, train_means)/train_sd

image(as.matrix(image), col=grey(seq(0, 1, length=256)))
image(as.matrix(smallimage), col=grey(seq(0, 1, length=256)))
image(X, col=grey(seq(0, 1, length=256)))

## This is how I created the Full Dataset:
library(doParallel)
registerDoParallel(5)
verymilddir <- "/Users/joannawalsh/Desktop/Dataset/Very_Mild_Demented"
verymildfiles <- list.files(path=verymilddir, full.names = TRUE)
nondir <- "/Users/joannawalsh/Desktop/Dataset/Non_Demented"
nonfiles <- list.files(path=nondir, full.names = TRUE)
moddir <- "/Users/joannawalsh/Desktop/Dataset/Moderate_Demented"
modfiles <- list.files(path=moddir, full.names = TRUE)
milddir <- "/Users/joannawalsh/Desktop/Dataset/Mild_Demented"
mildfiles <- list.files(path=milddir, full.names = TRUE)
files = c(verymildfiles, nonfiles, modfiles, mildfiles)

imgdata <- foreach(i = 1: length(files),
                   .combine = "rbind") %dopar% {
                     file = files[i]
                     image <- load.image(file = file)
                     image <- resize(image, round(width(image)/3), round(height(image)/3))
                     image <- as.vector(image)
                     t(image)
                   }

imgdata <- as.data.frame(imgdata)
imgdata$DX <- c(rep("Very Mild Demented", 2240), rep("Non Demented", 3200), rep("Moderate Demented", 64), rep("Mild Demented", 896))
write.csv(imgdata, file = "imagedata.csv", row.names = FALSE)

## Create Plots of Average Pixel Intensity by Diagnostic Group

imagedata <- read.csv("imagedata.csv")

nondemented <- colMeans(imagedata %>% filter(DX == "Non Demented") %>% select(-DX))
nondemented <- matrix(nondemented, nrow = 43, ncol = 43)
verymilddemented <- colMeans(imagedata %>% filter(DX == "Very Mild Demented") %>% select(-DX))
verymilddemented <- matrix(verymilddemented, nrow = 43, ncol = 43)
milddemented <- colMeans(imagedata %>% filter(DX == "Mild Demented") %>% select(-DX))
milddemented <- matrix(milddemented, nrow = 43, ncol = 43)
moderatedemented <- colMeans(imagedata %>% filter(DX == "Moderate Demented") %>% select(-DX))
moderatedemented <- matrix(moderatedemented, nrow = 43, ncol = 43)
image(nondemented,  col=grey(seq(0, 1, length=256)))
image(verymilddemented,  col=grey(seq(0, 1, length=256)))
image(milddemented,  col=grey(seq(0, 1, length=256)))
image(moderatedemented,  col=grey(seq(0, 1, length=256)))