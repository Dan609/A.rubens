# T10, T50, T90 calculations of 
# Calcium-induced A.rubens coelomocytes aggregation
# Sep 2021 , kartesh White sea biostation
# Dan Bobkov, dan.bobkov@gmail.com

library(tidyverse)
library(drc)
library(writexl)
library(data.table)

data <- read.csv2('rois.csv', dec = '.')

data$ROI1
data$ROI2
data$Time

data <- data.frame(
  Dose = data$Time,
  Response = data$ROI2
)

# Calculation 
m1 <- drm(Response ~ Dose, 
          data = data, 
          fct =LL.5(names = c("Slope", "Lower Limit", "Upper Limit", "ED50","f")))

summary(m1)

# reduce the model to the appropriate parameter function
mselect(m1, list(LL.5(), LL.4(), LL.3(), LL.2(),
                 W1.2(), W1.3(), W1.4(), 
                 W2.2(), W2.3(), W2.4()), 
                 linreg=TRUE, icfct=AIC)



models <- mselect(m1, list(LL.5(), LL.4(), LL.3(), LL.2(),
                 W1.2(), W1.3(), W1.4(), 
                 W2.2(), W2.3(), W2.4()), 
                 linreg=TRUE, icfct=AIC)

models <- data.frame(models, check.rows = TRUE)
models <- setDT(models, keep.rownames = 'Model')[]
write_xlsx(models,"models_HT1-1.xlsx")

########################
maED(m1, 
     list(W1.4(),
          LL.5(),
          LL.4(), 
          W2.4()),
          c(10, 50, 90), 
          interval="kang")

#you don't need the 'names = ' argument but it's useful to label the b, c, d, and e parameters until you're familiar with
model.W2.4 <-  drm(Response ~ Dose, data = data, , fct=W2.4(names = c("Slope", "Lower Limit", "Upper Limit","ED50")))
model.LL.5 <-  drm(Response ~ Dose, data = data, , fct=LL.5(names = c("Slope", "Lower Limit", "Upper Limit",  "ED50", "f")))
model.LL.4 <-  drm(Response ~ Dose, data = data, , fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
model.W1.4 <-  drm(Response ~ Dose, data = data, , fct=W2.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))




plot(model.W1.4, col="black", 
     lty=1, lwd=2, 
     type="all", ylab="", xlab = "", 
     main = "Лунка 2, повтор 1")
plot(model.W1.4, add=TRUE, col="blue",lty=2, lwd=2)
plot(model.LL.4, add=TRUE, col="forestgreen",lty=2, lwd=2)
plot(model.LL.5, add=TRUE, col="red",lty=2, lwd=2)

title(xlab="Время, сек", line=2, cex.lab=1.1)
title(ylab="Оптическая плотность, у.е.", line=2.4, cex.lab=1.1)

#plot(model.LL3, broken = TRUE, xlab="Concentration", ylab="Percent Response", type='all',lty=1, lwd=2)
#plot(model.W23, add=TRUE,col="orange",lty=1, lwd=2)
# ED(model.W24, c(10,50,90), interval="delta")

#maED(model.W24, 
#     list(W2.4(),LL.4(),LL.3(fixed=c(NA, 100, NA)), W1.4()),c(10, 50, 90), 
#     interval="kang")

#start by converting all the responses into a percent of the control response
#data <- data %>% 
#  mutate(percent_response = Response/(mean(data$Response[data$Dose==0]))*100)
#head(data)

# original example from 
# http://www.darrenkoppel.com/2020/09/04/dose-response-modelling-and-model-selection-in-r/
#model <- drm(Response ~ Dose, 
#            data = data, 
#            fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

#summary(model)
#interval = "delta" gives you asymptotically-based confidence intervals at a default 95% level.  
#ED(model, c(10,50, 90), interval="delta")
