# rm(list=ls())

library(dlnm)
library(tsModel)
library(splines)
library(qcc)
library(MASS)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(openxlsx)
library(ggsci)

###### 数据 ######
load("Data/air_confirm.rda")

dat_fit_all <- data.frame()

city1 <- "深圳"
df <- subset(data,data["城市"] == city1)
city2 <- '温州'
df2 <- subset(data, data$'城市' == city2)

dvar <- "确诊非输入型病例"

### "平均温度"
var1 <- "平均温度"
qvar1 <- quantile(df[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar2 <- quantile(df2[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar <- quantile(c(df[, var1], df2[,var1]), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

lagknots <- logknots(14,df=3)
cb1.var1 <- crossbasis(df[,var1],lag=14, argvar=list(df=2), arglag=list(knots=lagknots))
cb2.var1 <- crossbasis(df2[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))

model1 <- glm(df[,dvar]~ cb1.var1 + ns(df[,"风速"],3) + 
                ns(df[,'相对湿度.百分比'],3) + ns(df[,'PM2.5',3]) + 
                ns(df[,'CO',3]) + ns(df[,'NO2',3]) + 
                ns(df[,'O3_8h',3]),
              family=quasipoisson(),df)
sum_mod1 <- summary(model1)
sum_mod1$dispersion

ind <- 15:60
cases <- df[ind,dvar]
pre_cases <- unlist(model1$fitted.values)
city <- "SZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

model2 <- glm(df2[,dvar]~ cb2.var1 + ns(df2[,"风速"],3) + 
                ns(df2[,'相对湿度.百分比'],3) + ns(df2[,'PM2.5',3]) + 
                ns(df2[,'CO',3]) + ns(df2[,'NO2',3]) + 
                ns(df2[,'O3_8h',3]),
              family=quasipoisson(),df2)
sum_mod2 <- summary(model2)
sum_mod2$dispersion

ind <- 15:60
cases <- df2[ind,dvar]
pre_cases <- unlist(model2$fitted.values)
city <- "WZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

### 改动部分
pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                  qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)
pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                  qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)

fit1 <- t(data.frame(rbind(fit = round(pred1.var1[["matRRfit"]],2),
                           low = round(pred1.var1[["matRRlow"]],2), 
                           high = round(pred1.var1[["matRRhigh"]],2))))
fit2 <- t(data.frame(rbind(fit = round(pred2.var1[["matRRfit"]],2),
                           low = round(pred2.var1[["matRRlow"]],2), 
                           high = round(pred2.var1[["matRRhigh"]],2))))
fit <- cbind(fit1[,1],
             paste0(fit1[,9],"-",fit1[,17]),
             fit2[,1],
             paste0(fit2[,9],"-",fit2[,17]),
             fit1[,2],
             paste0(fit1[,10],"-",fit1[,18]),
             fit2[,2],
             paste0(fit2[,10],"-",fit2[,18]),
             fit1[,3],
             paste0(fit1[,11],"-",fit1[,19]),
             fit2[,3],
             paste0(fit2[,11],"-",fit2[,19]),
             fit1[,4],
             paste0(fit1[,12],"-",fit1[,20]),
             fit2[,4],
             paste0(fit2[,12],"-",fit2[,20]),
             1,
             1,
             fit1[,5],
             paste0(fit1[,13],"-",fit1[,21]),
             fit2[,5],
             paste0(fit2[,13],"-",fit2[,21]),
             fit1[,6],
             paste0(fit1[,14],"-",fit1[,22]),
             fit2[,6],
             paste0(fit2[,14],"-",fit2[,22]),
             fit1[,7],
             paste0(fit1[,15],"-",fit1[,23]),
             fit2[,7],
             paste0(fit2[,15],"-",fit2[,23]),
             fit1[,8],
             paste0(fit1[,16],"-",fit1[,24]),
             fit2[,8],
             paste0(fit2[,16],"-",fit2[,24])
             )

colnames(fit) <- c(substring(colnames(fit1)[1],2),"95%CI",substring(colnames(fit2)[1],2),"95%CI",
                   substring(colnames(fit1)[2],2),"95%CI",substring(colnames(fit2)[2],2),"95%CI",
                   substring(colnames(fit1)[3],2),"95%CI",substring(colnames(fit2)[3],2),"95%CI",
                   substring(colnames(fit1)[4],2),"95%CI",substring(colnames(fit2)[4],2),"95%CI",
                   round(qvar[5],2),round(qvar[5],2),
                   substring(colnames(fit1)[5],2),"95%CI",substring(colnames(fit2)[5],2),"95%CI",
                   substring(colnames(fit1)[6],2),"95%CI",substring(colnames(fit2)[6],2),"95%CI",
                   substring(colnames(fit1)[7],2),"95%CI",substring(colnames(fit2)[7],2),"95%CI",
                   substring(colnames(fit1)[8],2),"95%CI",substring(colnames(fit2)[8],2),"95%CI")
rownames(fit) <- 0:14
result_temp <- fit

sz_temp <- pred1.var1
wz_temp <- pred2.var1
temp_vec <- c(8.5, 21.5)
pred1.var1$predvar
pred2.var1$predvar

#save(sz_temp, file = 'sz_temp.Rda')
#save(wz_temp, file = 'wz_temp.Rda')
#save(temp_vec, file = 'temp_vec.Rda')



#### 画图 ####
dat_plot <- data.frame()
for (my_model in c("pred1.var1", "pred2.var1")) {
  matRRfit <- get(my_model)$matRRfit
  matRRlow <- get(my_model)$matRRlow
  matRRhigh <- get(my_model)$matRRhigh
  
  if (my_model %in% c("pred1.var1", "pred2.var1")) vec <- temp_vec
  
  ind1 <- row.names(matRRfit) == vec[1]
  ind2 <- row.names(matRRfit) == vec[2]
  
  mid1 <- matRRfit[ind1, ]
  low1 <- matRRlow[ind1, ]
  high1 <- matRRhigh[ind1, ]
  mid2 <- matRRfit[ind2, ]
  low2 <- matRRlow[ind2, ]
  high2 <- matRRhigh[ind2, ]
  
  dat_plot1 <- data.frame(num = 0:14, mid = mid1, low = low1, high = high1)
  dat_plot2 <- data.frame(num = 0:14, mid = mid2, low = low2, high = high2)
  dat_plot <- rbind(dat_plot, dat_plot1, dat_plot2)
}

dat_plot$city <- rep(c("深圳", "温州"), each = 30)
dat_plot$ylabel <- rep(c("10%", "90%"), each = 15)
dat_plot$xlabel <- rep("平均温度", 30)

dat_plot %>%
  ggplot(aes(x = num)) + 
  geom_ribbon(aes(ymin = low, ymax = high, fill = city), alpha = 0.3) +
  geom_hline(yintercept = 1, alpha = 0.5, size = 2) +
  geom_line(aes(y = mid, color = city), size = 1) +
  geom_point(aes(y = mid, color = city), size = 2) +
  facet_grid(xlabel ~ ylabel, scales = "free") +
  scale_x_continuous(breaks = 0:14, labels = 0:14) +
  theme_bw(base_family = "Times") +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "滞后时间（天）", y = "相对危险度（RR）") +
  coord_cartesian(ylim = c(0, 4))

#ggsave(plot = last_plot(), filename = 'tempscale.png')



### "风速"
var1 <- "风速"
qvar1 <- quantile(df[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar2 <- quantile(df2[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar <- quantile(c(df[, var1], df2[,var1]), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

lagknots <- logknots(14,df=3)
cb1.var1 <- crossbasis(df[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))
cb2.var1 <- crossbasis(df2[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))

model1 <- glm(df[,dvar]~ cb1.var1+ns(df[,"平均温度"],3) + 
                ns(df[,'相对湿度.百分比'],3) + ns(df[,'PM2.5',3]) + 
                ns(df[,'CO',3])
              + ns(df[,'NO2',3]) + ns(df[,'O3_8h',3]),
              family=quasipoisson(),df)
sum_mod1 <- summary(model1)
sum_mod1$dispersion

ind <- 15:60
cases <- df[ind,dvar]
pre_cases <- unlist(model1$fitted.values)
city <- "SZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[2], cumul=TRUE)

model2 <- glm(df2[,dvar] ~ cb2.var1+ns(df2[,"平均温度"],3) + 
                ns(df2[,'相对湿度.百分比'],3) + ns(df2[,'PM2.5',3]) + 
                ns(df2[,'CO',3])
              + ns(df2[,'NO2',3]) + ns(df2[,'O3_8h',3]),
              family=quasipoisson(),df2)
sum_mod2 <- summary(model2)
sum_mod2$dispersion

ind <- 15:60
cases <- df2[ind,dvar]
pre_cases <- unlist(model2$fitted.values)
city <- "WZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[2], cumul=TRUE)

### 改动部分
pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)
pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)

fit1 <- t(data.frame(rbind(fit = round(pred1.var1[["matRRfit"]],2),
                           low = round(pred1.var1[["matRRlow"]],2), 
                           high = round(pred1.var1[["matRRhigh"]],2))))
fit2 <- t(data.frame(rbind(fit = round(pred2.var1[["matRRfit"]],2),
                           low = round(pred2.var1[["matRRlow"]],2), 
                           high = round(pred2.var1[["matRRhigh"]],2))))
fit <- cbind(fit1[,1],
             paste0(fit1[,9],"-",fit1[,17]),
             fit2[,1],
             paste0(fit2[,9],"-",fit2[,17]),
             fit1[,2],
             paste0(fit1[,10],"-",fit1[,18]),
             fit2[,2],
             paste0(fit2[,10],"-",fit2[,18]),
             fit1[,3],
             paste0(fit1[,11],"-",fit1[,19]),
             fit2[,3],
             paste0(fit2[,11],"-",fit2[,19]),
             fit1[,4],
             paste0(fit1[,12],"-",fit1[,20]),
             fit2[,4],
             paste0(fit2[,12],"-",fit2[,20]),
             1,
             1,
             fit1[,5],
             paste0(fit1[,13],"-",fit1[,21]),
             fit2[,5],
             paste0(fit2[,13],"-",fit2[,21]),
             fit1[,6],
             paste0(fit1[,14],"-",fit1[,22]),
             fit2[,6],
             paste0(fit2[,14],"-",fit2[,22]),
             fit1[,7],
             paste0(fit1[,15],"-",fit1[,23]),
             fit2[,7],
             paste0(fit2[,15],"-",fit2[,23]),
             fit1[,8],
             paste0(fit1[,16],"-",fit1[,24]),
             fit2[,8],
             paste0(fit2[,16],"-",fit2[,24])
)

colnames(fit) <- c(substring(colnames(fit1)[1],2),"95%CI",substring(colnames(fit2)[1],2),"95%CI",
                   substring(colnames(fit1)[2],2),"95%CI",substring(colnames(fit2)[2],2),"95%CI",
                   substring(colnames(fit1)[3],2),"95%CI",substring(colnames(fit2)[3],2),"95%CI",
                   substring(colnames(fit1)[4],2),"95%CI",substring(colnames(fit2)[4],2),"95%CI",
                   round(qvar[5],2),round(qvar[5],2),
                   substring(colnames(fit1)[5],2),"95%CI",substring(colnames(fit2)[5],2),"95%CI",
                   substring(colnames(fit1)[6],2),"95%CI",substring(colnames(fit2)[6],2),"95%CI",
                   substring(colnames(fit1)[7],2),"95%CI",substring(colnames(fit2)[7],2),"95%CI",
                   substring(colnames(fit1)[8],2),"95%CI",substring(colnames(fit2)[8],2),"95%CI")
rownames(fit) <- 0:14
result_wind <- fit

sz_wind <- pred1.var1
wz_wind <- pred2.var1
wind_vec <- c(4.8625, 10)
pred1.var1$predvar
pred2.var1$predvar

#save(sz_wind, file = 'sz_wind.Rda')
#save(wz_wind, file = 'wz_wind.Rda')
#save(wind_vec, file = 'wind_vec.Rda')


#### 画图 ####
dat_plot <- data.frame()
for (my_model in c("pred1.var1", "pred2.var1")) {
  matRRfit <- get(my_model)$matRRfit
  matRRlow <- get(my_model)$matRRlow
  matRRhigh <- get(my_model)$matRRhigh
  
  if (my_model %in% c("pred1.var1", "pred2.var1")) vec <- wind_vec
  
  ind1 <- row.names(matRRfit) == vec[1]
  ind2 <- row.names(matRRfit) == vec[2]
  
  mid1 <- matRRfit[ind1, ]
  low1 <- matRRlow[ind1, ]
  high1 <- matRRhigh[ind1, ]
  mid2 <- matRRfit[ind2, ]
  low2 <- matRRlow[ind2, ]
  high2 <- matRRhigh[ind2, ]
  
  dat_plot1 <- data.frame(num = 0:14, mid = mid1, low = low1, high = high1)
  dat_plot2 <- data.frame(num = 0:14, mid = mid2, low = low2, high = high2)
  dat_plot <- rbind(dat_plot, dat_plot1, dat_plot2)
}

dat_plot$city <- rep(c("深圳", "温州"), each = 30)
dat_plot$ylabel <- rep(c("10%", "90%"), each = 15)
dat_plot$xlabel <- rep("风速", 30)

dat_plot %>%
  ggplot(aes(x = num)) + 
  geom_ribbon(aes(ymin = low, ymax = high, fill = city), alpha = 0.3) +
  geom_hline(yintercept = 1, alpha = 0.5, size = 2) +
  geom_line(aes(y = mid, color = city), size = 1) +
  geom_point(aes(y = mid, color = city), size = 2) +
  facet_grid(xlabel ~ ylabel, scales = "free") +
  scale_x_continuous(breaks = 0:14, labels = 0:14) +
  theme_bw() +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "滞后时间（天）", y = "相对危险度（RR）") +
  coord_cartesian(ylim = c(0, 2))

# ggsave(plot = last_plot(), filename = 'windscale.png')
ggsave("windscale.pdf", device = cairo_pdf, family = "Song")

### "PM2.5"
var1 <- "PM2.5"
qvar1 <- quantile(df[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar2 <- quantile(df2[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar <- quantile(c(df[, var1], df2[,var1]), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

lagknots <- logknots(14,df=3)
cb1.var1 <- crossbasis(df[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))
cb2.var1 <- crossbasis(df2[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))

model1 <- glm(df[,dvar] ~ cb1.var1+ns(df[,"平均温度"],3) + 
                ns(df[,'相对湿度.百分比'],3) + ns(df[,"风速"],3) +
                ns(df[,'CO',3])
              + ns(df[,'NO2',3]) + ns(df[,'O3_8h',3]),
              family=quasipoisson(),df)
sum_mod1 <- summary(model1)
sum_mod1$dispersion

ind <- 15:60
cases <- df[ind,dvar]
pre_cases <- unlist(model1$fitted.values)
city <- "SZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[2], cumul=TRUE)

model2 <- glm(df2[,dvar] ~ cb2.var1+ns(df2[,"平均温度"],3) + 
                ns(df2[,'相对湿度.百分比'],3) + ns(df2[,"风速"],3) +
                ns(df2[,'CO',3])
              + ns(df2[,'NO2',3]) + ns(df2[,'O3_8h',3]),
              family=quasipoisson(),df2)
sum_mod2 <- summary(model2)
sum_mod2$dispersion

ind <- 15:60
cases <- df2[ind,dvar]
pre_cases <- unlist(model2$fitted.values)
city <- "WZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[2], cumul=TRUE)

### 改动部分
pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)
pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)

fit1 <- t(data.frame(rbind(fit = round(pred1.var1[["matRRfit"]],2),
                           low = round(pred1.var1[["matRRlow"]],2), 
                           high = round(pred1.var1[["matRRhigh"]],2))))
fit2 <- t(data.frame(rbind(fit = round(pred2.var1[["matRRfit"]],2),
                           low = round(pred2.var1[["matRRlow"]],2), 
                           high = round(pred2.var1[["matRRhigh"]],2))))
fit <- cbind(fit1[,1],
             paste0(fit1[,9],"-",fit1[,17]),
             fit2[,1],
             paste0(fit2[,9],"-",fit2[,17]),
             fit1[,2],
             paste0(fit1[,10],"-",fit1[,18]),
             fit2[,2],
             paste0(fit2[,10],"-",fit2[,18]),
             fit1[,3],
             paste0(fit1[,11],"-",fit1[,19]),
             fit2[,3],
             paste0(fit2[,11],"-",fit2[,19]),
             fit1[,4],
             paste0(fit1[,12],"-",fit1[,20]),
             fit2[,4],
             paste0(fit2[,12],"-",fit2[,20]),
             1,
             1,
             fit1[,5],
             paste0(fit1[,13],"-",fit1[,21]),
             fit2[,5],
             paste0(fit2[,13],"-",fit2[,21]),
             fit1[,6],
             paste0(fit1[,14],"-",fit1[,22]),
             fit2[,6],
             paste0(fit2[,14],"-",fit2[,22]),
             fit1[,7],
             paste0(fit1[,15],"-",fit1[,23]),
             fit2[,7],
             paste0(fit2[,15],"-",fit2[,23]),
             fit1[,8],
             paste0(fit1[,16],"-",fit1[,24]),
             fit2[,8],
             paste0(fit2[,16],"-",fit2[,24])
)

colnames(fit) <- c(substring(colnames(fit1)[1],2),"95%CI",substring(colnames(fit2)[1],2),"95%CI",
                   substring(colnames(fit1)[2],2),"95%CI",substring(colnames(fit2)[2],2),"95%CI",
                   substring(colnames(fit1)[3],2),"95%CI",substring(colnames(fit2)[3],2),"95%CI",
                   substring(colnames(fit1)[4],2),"95%CI",substring(colnames(fit2)[4],2),"95%CI",
                   round(qvar[5],2),round(qvar[5],2),
                   substring(colnames(fit1)[5],2),"95%CI",substring(colnames(fit2)[5],2),"95%CI",
                   substring(colnames(fit1)[6],2),"95%CI",substring(colnames(fit2)[6],2),"95%CI",
                   substring(colnames(fit1)[7],2),"95%CI",substring(colnames(fit2)[7],2),"95%CI",
                   substring(colnames(fit1)[8],2),"95%CI",substring(colnames(fit2)[8],2),"95%CI")
rownames(fit) <- 0:14
result_pm25 <- fit

### "CO"
var1 <- "CO"
qvar1 <- quantile(df[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar2 <- quantile(df2[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar <- quantile(c(df[, var1], df2[,var1]), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar[1] <- 0.5

lagknots <- logknots(14,df=3)
cb1.var1 <- crossbasis(df[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))
cb2.var1 <- crossbasis(df2[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))

model1 <- glm(df[,dvar] ~ cb1.var1+ns(df[,"平均温度"],3) + 
                ns(df[,'相对湿度.百分比'],3) + ns(df[,"风速"],3) +
                ns(df[,'PM2.5',3])
              + ns(df[,'NO2',3]) + ns(df[,'O3_8h',3]),
              family=quasipoisson(),df)
sum_mod1 <- summary(model1)
sum_mod1$dispersion

ind <- 15:60
cases <- df[ind,dvar]
pre_cases <- unlist(model1$fitted.values)
city <- "SZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[2], cumul=TRUE)

model2 <- glm(df2[,dvar] ~ cb2.var1+ns(df2[,"平均温度"],3) + 
                ns(df2[,'相对湿度.百分比'],3) + ns(df2[,"风速"],3) +
                ns(df2[,'PM2.5',3])
              + ns(df2[,'NO2',3]) + ns(df2[,'O3_8h',3]),
              family=quasipoisson(),df2)
sum_mod2 <- summary(model2)
sum_mod2$dispersion

ind <- 15:60
cases <- df2[ind,dvar]
pre_cases <- unlist(model2$fitted.values)
city <- "WZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[2], cumul=TRUE)

### 改动部分
pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)
pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)

fit1 <- t(data.frame(rbind(fit = round(pred1.var1[["matRRfit"]],2),
                           low = round(pred1.var1[["matRRlow"]],2), 
                           high = round(pred1.var1[["matRRhigh"]],2))))
fit2 <- t(data.frame(rbind(fit = round(pred2.var1[["matRRfit"]],2),
                           low = round(pred2.var1[["matRRlow"]],2), 
                           high = round(pred2.var1[["matRRhigh"]],2))))
fit <- cbind(fit1[,1],
             paste0(fit1[,5],"-",fit1[,9]),
             fit2[,1],
             paste0(fit2[,5],"-",fit2[,9]),
             1,
             1,
             fit1[,3],
             paste0(fit1[,7],"-",fit1[,11]),
             fit2[,3],
             paste0(fit2[,7],"-",fit2[,11]),
             fit1[,4],
             paste0(fit1[,8],"-",fit1[,12]),
             fit2[,4],
             paste0(fit2[,8],"-",fit2[,12])
)

colnames(fit) <- c(substring(colnames(fit1)[1],2),"95%CI",substring(colnames(fit2)[1],2),"95%CI",
                   substring(colnames(fit1)[2],2),substring(colnames(fit2)[2],2),
                   substring(colnames(fit1)[3],2),"95%CI",substring(colnames(fit2)[3],2),"95%CI",
                   substring(colnames(fit1)[4],2),"95%CI",substring(colnames(fit2)[4],2),"95%CI"
                  )
rownames(fit) <- 0:14
result_co <- fit


## 画图
sz_CO <- pred1.var1
wz_CO <- pred2.var1
CO_vec <- quantile(c(pred1.var1$predvar, pred2.var1$predvar), c(0.2, 0.9))
CO_vec <- c(0.5, 0.8)
pred1.var1$predvar
pred2.var1$predvar

dat_plot <- data.frame()
for (my_model in c("pred1.var1", "pred2.var1")) {
  matRRfit <- get(my_model)$matRRfit
  matRRlow <- get(my_model)$matRRlow
  matRRhigh <- get(my_model)$matRRhigh
  
  if (my_model %in% c("pred1.var1", "pred2.var1")) vec <- CO_vec
  
  ind1 <- row.names(matRRfit) == vec[1]
  ind2 <- row.names(matRRfit) == vec[2]
  
  mid1 <- matRRfit[ind1, ]
  low1 <- matRRlow[ind1, ]
  high1 <- matRRhigh[ind1, ]
  mid2 <- matRRfit[ind2, ]
  low2 <- matRRlow[ind2, ]
  high2 <- matRRhigh[ind2, ]
  
  dat_plot1 <- data.frame(num = 0:14, mid = mid1, low = low1, high = high1)
  dat_plot2 <- data.frame(num = 0:14, mid = mid2, low = low2, high = high2)
  dat_plot <- rbind(dat_plot, dat_plot1, dat_plot2)
}

dat_plot$city <- rep(c("深圳", "温州"), each = 30)
dat_plot$ylabel <- rep(c("0.5", "0.8"), each = 15)
dat_plot$xlabel <- rep("CO", 30)

dat_plot %>%
  ggplot(aes(x = num)) + 
  geom_ribbon(aes(ymin = low, ymax = high, fill = city), alpha = 0.3) +
  geom_hline(yintercept = 1, alpha = 0.5, size = 2) +
  geom_line(aes(y = mid, color = city), size = 1) +
  geom_point(aes(y = mid, color = city), size = 2) +
  facet_grid(xlabel ~ ylabel, scales = "free") +
  scale_x_continuous(breaks = 0:14, labels = 0:14) +
  theme_bw(base_family = "Times") +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "滞后时间（天）", y = "相对危险度（RR）") +
  coord_cartesian(ylim = c(0, 4))

# ggsave(plot = last_plot(), filename = 'COscale.png')

#### 画图 ####
png(filename = 'Result/szCO.png', width = 1024, height = 768, pointsize = 15)
plot(pred1.var1, xlab=var1, zlab="RR", theta=200, phi=40, lphi=30,
     main=city1)
dev.off()

png(filename = 'Result/wzCO.png', width = 1024, height = 768, pointsize = 15)
plot(pred2.var1, xlab=var1, zlab="RR", theta=200, phi=40, lphi=30,
     main=city2)
dev.off()

### "NO2"
var1 <- "NO2"
qvar1 <- quantile(df[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar2 <- quantile(df2[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar <- quantile(c(df[, var1], df2[,var1]), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

lagknots <- logknots(14,df=3)
cb1.var1 <- crossbasis(df[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))
cb2.var1 <- crossbasis(df2[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))

model1 <- glm(df[,dvar] ~ cb1.var1+ns(df[,"平均温度"],3) + 
                ns(df[,'相对湿度.百分比'],3) + ns(df[,"风速"],3) +
                ns(df[,'PM2.5',3]) + ns(df[,'CO',3])
              + ns(df[,'O3_8h',3]),
              family=quasipoisson(),df)
sum_mod1 <- summary(model1)
sum_mod1$dispersion

ind <- 15:60
cases <- df[ind,dvar]
pre_cases <- unlist(model1$fitted.values)
city <- "SZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[2], cumul=TRUE)

model2 <- glm(df2[,dvar] ~ cb2.var1+ns(df2[,"平均温度"],3) + 
                ns(df2[,'相对湿度.百分比'],3) + ns(df2[,"风速"],3) +
                ns(df2[,'PM2.5',3])+ ns(df2[,'CO',3])
              + ns(df2[,'O3_8h',3]),
              family=quasipoisson(),df2)
sum_mod2 <- summary(model2)
sum_mod2$dispersion

ind <- 15:60
cases <- df2[ind,dvar]
pre_cases <- unlist(model2$fitted.values)
city <- "WZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[2], cumul=TRUE)

### 改动部分
pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)
pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)

fit1 <- t(data.frame(rbind(fit = round(pred1.var1[["matRRfit"]],2),
                           low = round(pred1.var1[["matRRlow"]],2), 
                           high = round(pred1.var1[["matRRhigh"]],2))))
fit2 <- t(data.frame(rbind(fit = round(pred2.var1[["matRRfit"]],2),
                           low = round(pred2.var1[["matRRlow"]],2), 
                           high = round(pred2.var1[["matRRhigh"]],2))))
fit <- cbind(fit1[,1],
             paste0(fit1[,9],"-",fit1[,17]),
             fit2[,1],
             paste0(fit2[,9],"-",fit2[,17]),
             fit1[,2],
             paste0(fit1[,10],"-",fit1[,18]),
             fit2[,2],
             paste0(fit2[,10],"-",fit2[,18]),
             fit1[,3],
             paste0(fit1[,11],"-",fit1[,19]),
             fit2[,3],
             paste0(fit2[,11],"-",fit2[,19]),
             fit1[,4],
             paste0(fit1[,12],"-",fit1[,20]),
             fit2[,4],
             paste0(fit2[,12],"-",fit2[,20]),
             1,
             1,
             fit1[,5],
             paste0(fit1[,13],"-",fit1[,21]),
             fit2[,5],
             paste0(fit2[,13],"-",fit2[,21]),
             fit1[,6],
             paste0(fit1[,14],"-",fit1[,22]),
             fit2[,6],
             paste0(fit2[,14],"-",fit2[,22]),
             fit1[,7],
             paste0(fit1[,15],"-",fit1[,23]),
             fit2[,7],
             paste0(fit2[,15],"-",fit2[,23]),
             fit1[,8],
             paste0(fit1[,16],"-",fit1[,24]),
             fit2[,8],
             paste0(fit2[,16],"-",fit2[,24])
)

colnames(fit) <- c(substring(colnames(fit1)[1],2),"95%CI",substring(colnames(fit2)[1],2),"95%CI",
                   substring(colnames(fit1)[2],2),"95%CI",substring(colnames(fit2)[2],2),"95%CI",
                   substring(colnames(fit1)[3],2),"95%CI",substring(colnames(fit2)[3],2),"95%CI",
                   substring(colnames(fit1)[4],2),"95%CI",substring(colnames(fit2)[4],2),"95%CI",
                   round(qvar[5],2),round(qvar[5],2),
                   substring(colnames(fit1)[5],2),"95%CI",substring(colnames(fit2)[5],2),"95%CI",
                   substring(colnames(fit1)[6],2),"95%CI",substring(colnames(fit2)[6],2),"95%CI",
                   substring(colnames(fit1)[7],2),"95%CI",substring(colnames(fit2)[7],2),"95%CI",
                   substring(colnames(fit1)[8],2),"95%CI",substring(colnames(fit2)[8],2),"95%CI")
rownames(fit) <- 0:14
result_no2 <- fit


#### 画图 ####
sz_NO2 <- pred1.var1
wz_NO2 <- pred2.var1
NO2_vec <- quantile(c(pred1.var1$predvar, pred2.var1$predvar), c(0.1, 0.9))
NO2_vec <- c(6.9, 39)
pred1.var1$predvar
pred2.var1$predvar

dat_plot <- data.frame()
for (my_model in c("pred1.var1", "pred2.var1")) {
  matRRfit <- get(my_model)$matRRfit
  matRRlow <- get(my_model)$matRRlow
  matRRhigh <- get(my_model)$matRRhigh
  
  if (my_model %in% c("pred1.var1", "pred2.var1")) vec <- NO2_vec
  
  ind1 <- row.names(matRRfit) == vec[1]
  ind2 <- row.names(matRRfit) == vec[2]
  
  mid1 <- matRRfit[ind1, ]
  low1 <- matRRlow[ind1, ]
  high1 <- matRRhigh[ind1, ]
  mid2 <- matRRfit[ind2, ]
  low2 <- matRRlow[ind2, ]
  high2 <- matRRhigh[ind2, ]
  
  dat_plot1 <- data.frame(num = 0:14, mid = mid1, low = low1, high = high1)
  dat_plot2 <- data.frame(num = 0:14, mid = mid2, low = low2, high = high2)
  dat_plot <- rbind(dat_plot, dat_plot1, dat_plot2)
}

dat_plot$city <- rep(c("深圳", "温州"), each = 30)
dat_plot$ylabel <- rep(c("10%", "90%"), each = 15)
dat_plot$xlabel <- rep("NO2", 30)

dat_plot %>%
  ggplot(aes(x = num)) + 
  geom_ribbon(aes(ymin = low, ymax = high, fill = city), alpha = 0.3) +
  geom_hline(yintercept = 1, alpha = 0.5, size = 2) +
  geom_line(aes(y = mid, color = city), size = 1) +
  geom_point(aes(y = mid, color = city), size = 2) +
  facet_grid(xlabel ~ ylabel, scales = "free") +
  scale_x_continuous(breaks = 0:14, labels = 0:14) +
  theme_bw(base_family = "Times") +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "滞后时间（天）", y = "相对危险度（RR）") +
  coord_cartesian(ylim = c(0, 2))

#ggsave(plot = last_plot(), filename = 'NO2scale.png')







png(filename = 'Result/szNO2.png', width = 1024, height = 768, pointsize = 15)
plot(pred1.var1, xlab=var1, zlab="RR", theta=200, phi=40, lphi=30,
     main=city1)
dev.off()

png(filename = 'Result/wzNO2.png', width = 1024, height = 768, pointsize = 15)
plot(pred2.var1, xlab=var1, zlab="RR", theta=200, phi=40, lphi=30,
     main=city2)
dev.off()

### "O3_8h"
var1 <- "O3_8h"
qvar1 <- quantile(df[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar2 <- quantile(df2[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar <- quantile(c(df[, var1], df2[,var1]), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

lagknots <- logknots(14,df=3)
cb1.var1 <- crossbasis(df[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))
cb2.var1 <- crossbasis(df2[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))

model1 <- glm(df[,dvar]~ cb1.var1+ns(df[,"平均温度"],3) + 
                ns(df[,'相对湿度.百分比'],3) + ns(df[,"风速"],3) +
                ns(df[,'PM2.5',3])+ ns(df[,'CO',3])
              + ns(df[,'NO2',3]),
              family=quasipoisson(),df)
sum_mod1 <- summary(model1)
sum_mod1$dispersion

ind <- 15:60
cases <- df[ind,dvar]
pre_cases <- unlist(model1$fitted.values)
city <- "SZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[2], cumul=TRUE)

model2 <- glm(df2[,dvar] ~ cb2.var1+ns(df2[,"平均温度"],3) + 
                ns(df2[,'相对湿度.百分比'],3) + ns(df2[,"风速"],3) +
                ns(df2[,'PM2.5',3]) + ns(df2[,'CO',3])
              + ns(df2[,'NO2',3]),
              family=quasipoisson(),df2)
sum_mod2 <- summary(model2)
sum_mod2$dispersion

ind <- 15:60
cases <- df2[ind,dvar]
pre_cases <- unlist(model2$fitted.values)
city <- "WZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[2], cumul=TRUE)

### 展示实际情况部分
pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[5], at=c(qvar[2], qvar[3], qvar[4], qvar[6],qvar[7],qvar[8]), cumul=TRUE)
pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[5], at=c(qvar[2], qvar[3], qvar[4], qvar[6],qvar[7],qvar[8]), cumul=TRUE)

fit1 <- t(data.frame(rbind(fit = round(pred1.var1[["matRRfit"]],2),
                           low = round(pred1.var1[["matRRlow"]],2), 
                           high = round(pred1.var1[["matRRhigh"]],2))))
fit2 <- t(data.frame(rbind(fit = round(pred2.var1[["matRRfit"]],2),
                           low = round(pred2.var1[["matRRlow"]],2), 
                           high = round(pred2.var1[["matRRhigh"]],2))))
fit <- cbind(fit1[,1],
             paste0(fit1[,7],"-",fit1[,13]),
             fit2[,1],
             paste0(fit2[,7],"-",fit2[,13]),
             fit1[,2],
             paste0(fit1[,8],"-",fit1[,14]),
             fit2[,2],
             paste0(fit2[,8],"-",fit2[,14]),
             fit1[,3],
             paste0(fit1[,9],"-",fit1[,15]),
             fit2[,3],
             paste0(fit2[,9],"-",fit2[,15]),
             1,
             1,
             fit1[,4],
             paste0(fit1[,10],"-",fit1[,16]),
             fit2[,4],
             paste0(fit2[,10],"-",fit2[,16]),
             fit1[,5],
             paste0(fit1[,11],"-",fit1[,17]),
             fit2[,5],
             paste0(fit2[,11],"-",fit2[,17]),
             fit1[,6],
             paste0(fit1[,12],"-",fit1[,18]),
             fit2[,6],
             paste0(fit2[,12],"-",fit2[,18]))


colnames(fit) <- c(substring(colnames(fit1)[1],2),"95%CI",substring(colnames(fit2)[1],2),"95%CI",
                   substring(colnames(fit1)[2],2),"95%CI",substring(colnames(fit2)[2],2),"95%CI",
                   substring(colnames(fit1)[3],2),"95%CI",substring(colnames(fit2)[3],2),"95%CI",
                   round(qvar[5],2),round(qvar[5],2),
                   substring(colnames(fit1)[4],2),"95%CI",substring(colnames(fit2)[4],2),"95%CI",
                   substring(colnames(fit1)[5],2),"95%CI",substring(colnames(fit2)[5],2),"95%CI",
                   substring(colnames(fit1)[6],2),"95%CI",substring(colnames(fit2)[6],2),"95%CI")
rownames(fit) <- 0:14
result_o3 <- fit

#### 画图 ####
# png(filename = 'Result/szO3.png', width = 1024, height = 768, pointsize = 15)
pdf(file = 'Result/szO3.pdf', family= "GB1")
plot(pred1.var1, xlab="O3 (8h)", zlab="RR", theta=200, phi=40, lphi=30,
     main = city1)
dev.off()

# png(filename = 'Result/wzO3.png', width = 1024, height = 768, pointsize = 15)
pdf(file = 'Result/wzO3.pdf', family= "GB1")
plot(pred2.var1, xlab="O3 (8h)", zlab="RR", theta=200, phi=40, lphi=30,
     main = city2)
dev.off()

### "相对湿度.百分比"
var1 <- "相对湿度.百分比"
qvar1 <- quantile(df[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar2 <- quantile(df2[, var1], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
qvar <- quantile(c(df[, var1], df2[,var1]), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

lagknots <- logknots(14,df=3)
cb1.var1 <- crossbasis(df[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))
cb2.var1 <- crossbasis(df2[,var1],lag=14, argvar=list(df=3), arglag=list(knots=lagknots))
model1 <- glm(df[,dvar] ~ cb1.var1 + ns(df[,"风速"],3) + 
                ns(df[,'平均温度'],3) + ns(df[,'PM2.5',3]) + 
                ns(df[,'PM10',3])+ ns(df[,'SO2',3]) + ns(df[,'CO',3])
              + ns(df[,'NO2',3]) + ns(df[,'O3_8h',3]),
              family=quasipoisson(),df)
sum_mod1 <- summary(model1)
sum_mod1$dispersion

ind <- 15:60
cases <- df[ind,dvar]
pre_cases <- unlist(model1$fitted.values)
city <- "SZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)


model2 <- glm(df2[,dvar] ~ cb2.var1 + ns(df2[,"风速"],3) + 
                ns(df2[,'平均温度'],3) + ns(df2[,'PM2.5',3]) + 
                ns(df2[,'PM10',3])+ ns(df2[,'SO2',3]) + ns(df2[,'CO',3])
              + ns(df2[,'NO2',3]) + ns(df2[,'O3_8h',3]),
              family=quasipoisson(),df2)
sum_mod2 <- summary(model2)
sum_mod2$dispersion

ind <- 15:60
cases <- df2[ind,dvar]
pre_cases <- unlist(model2$fitted.values)
city <- "WZ"
variable <- var1
dat_fit_temp <- data.frame(ind, cases, pre_cases, city, variable)
dat_fit_all <- rbind(dat_fit_all, dat_fit_temp)
# plot(cases, pre_cases)

### 改动部分
pred1.var1 <- crosspred(cb1.var1, model1,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)
pred2.var1 <- crosspred(cb2.var1, model2,  cen = qvar[5], at=c(qvar[1],qvar[2], 
                                                               qvar[3], qvar[4], qvar[6],qvar[7],qvar[8],qvar[9]), cumul=TRUE)

fit1 <- t(data.frame(rbind(fit = round(pred1.var1[["matRRfit"]],2),
                           low = round(pred1.var1[["matRRlow"]],2), 
                           high = round(pred1.var1[["matRRhigh"]],2))))
fit2 <- t(data.frame(rbind(fit = round(pred2.var1[["matRRfit"]],2),
                           low = round(pred2.var1[["matRRlow"]],2), 
                           high = round(pred2.var1[["matRRhigh"]],2))))
fit <- cbind(fit1[,1],
             paste0(fit1[,9],"-",fit1[,17]),
             fit2[,1],
             paste0(fit2[,9],"-",fit2[,17]),
             fit1[,2],
             paste0(fit1[,10],"-",fit1[,18]),
             fit2[,2],
             paste0(fit2[,10],"-",fit2[,18]),
             fit1[,3],
             paste0(fit1[,11],"-",fit1[,19]),
             fit2[,3],
             paste0(fit2[,11],"-",fit2[,19]),
             fit1[,4],
             paste0(fit1[,12],"-",fit1[,20]),
             fit2[,4],
             paste0(fit2[,12],"-",fit2[,20]),
             1,
             1,
             fit1[,5],
             paste0(fit1[,13],"-",fit1[,21]),
             fit2[,5],
             paste0(fit2[,13],"-",fit2[,21]),
             fit1[,6],
             paste0(fit1[,14],"-",fit1[,22]),
             fit2[,6],
             paste0(fit2[,14],"-",fit2[,22]),
             fit1[,7],
             paste0(fit1[,15],"-",fit1[,23]),
             fit2[,7],
             paste0(fit2[,15],"-",fit2[,23]),
             fit1[,8],
             paste0(fit1[,16],"-",fit1[,24]),
             fit2[,8],
             paste0(fit2[,16],"-",fit2[,24])
)

colnames(fit) <- c(substring(colnames(fit1)[1],2),"95%CI",substring(colnames(fit2)[1],2),"95%CI",
                   substring(colnames(fit1)[2],2),"95%CI",substring(colnames(fit2)[2],2),"95%CI",
                   substring(colnames(fit1)[3],2),"95%CI",substring(colnames(fit2)[3],2),"95%CI",
                   substring(colnames(fit1)[4],2),"95%CI",substring(colnames(fit2)[4],2),"95%CI",
                   round(qvar[5],2),round(qvar[5],2),
                   substring(colnames(fit1)[5],2),"95%CI",substring(colnames(fit2)[5],2),"95%CI",
                   substring(colnames(fit1)[6],2),"95%CI",substring(colnames(fit2)[6],2),"95%CI",
                   substring(colnames(fit1)[7],2),"95%CI",substring(colnames(fit2)[7],2),"95%CI",
                   substring(colnames(fit1)[8],2),"95%CI",substring(colnames(fit2)[8],2),"95%CI")
rownames(fit) <- 0:14
result_wet <- fit

### 输出表格文件
sheets = list("temp"=result_temp,"wind"=result_wind,"pm25"=result_pm25,
              "co"=result_co,"no2"=result_no2,"o3"=result_o3,"wet"=result_wet)
write.xlsx(sheets,rownames=TRUE,colnames=TRUE,"result6_2.xlsx")


##### ------------------ 绘制各个指标拟合情况图 ---------------- #####
dat_fit_all$city[dat_fit_all$city == "SZ"] <- "深圳市"
dat_fit_all$city[dat_fit_all$city == "WZ"] <- "温州市"
dat_fit_all$variable[dat_fit_all$variable == "O3_8h"] <- "O3"
dat_fit_all$variable[dat_fit_all$variable == "相对湿度.百分比"] <- "相对湿度"

dat_fit_all$city <- factor(dat_fit_all$city, levels = c("深圳市", "温州市"))
dat_fit_all$variable <- factor(dat_fit_all$variable, levels = c("平均温度", "相对湿度", "风速", "PM2.5", "CO", "NO2", "O3"))

ind_date <- 3:12 * 5
plot_date <- seq.Date(as.Date("2020/1/1"), by = "day", length.out = 60)[ind_date]
plot_date <- format(plot_date, "%m/%d")
dat_fit_all %>%
  ggplot() +
  geom_point(aes(x = ind, y = cases, color = "真实新增")) +
  geom_point(aes(x = ind, y = pre_cases, color = "预测新增")) +
  scale_x_continuous(breaks = ind_date, labels = plot_date) +
  theme_light(base_family = "SimSun") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        # panel.border = element_blank(),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 20),
        strip.background = element_rect(color = "white"),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_jco() +
  # facet_wrap(~ variable + city, ncol = 2) +
  facet_grid(rows = vars(variable), cols = vars(city)) +
  labs(x = "日期", y = "每日非输入新增患者人数", 
       title = NULL,
       color = "")
ggsave("fit_plot.pdf", height = 10, width = 8, device = cairo_pdf, family = "Song")
# ggsave("fit_plot.png", height = 10, width = 8, dpi = 300)
