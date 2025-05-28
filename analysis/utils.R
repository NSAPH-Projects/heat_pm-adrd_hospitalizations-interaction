library(survival)
library(data.table)
library(lubridate)
library(dplyr)
library(fst)
library(splines)
library(viridis)
library(ggpubr)
library(dlnm)
library(mgcv)

# Create joint basis
joint_basis <- function(x1, x2, df=3) {
  b1 <- bs(x1, df=df)
  b2 <- bs(x2, df=df)
  basis = cbind(b1, b2, 
                b1[,1]*b2[,1], b1[,1]*b2[,2], b1[,1]*b2[,3],
                b1[,2]*b2[,1], b1[,2]*b2[,2], b1[,2]*b2[,3],
                b1[,3]*b2[,1], b1[,3]*b2[,2], b1[,3]*b2[,3])
  return(list(b1 = b1, b2 = b2, basis = basis))
}

# Project new data on joint basis
project_basis <- function(x1_new, x2_new, b1, b2) {
  nb1 <- predict(b1, x1_new)
  nb2 <- predict(b2, x2_new)
  basis = cbind(nb1, nb2, 
                nb1[,1]*nb2[,1], nb1[,1]*nb2[,2], nb1[,1]*nb2[,3],
                nb1[,2]*nb2[,1], nb1[,2]*nb2[,2], nb1[,2]*nb2[,3],
                nb1[,3]*nb2[,1], nb1[,3]*nb2[,2], nb1[,3]*nb2[,3])
  return(basis)
}