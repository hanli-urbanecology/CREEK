################### CREEK project #################
########### extra##################
fit <- glm(data=df_epfu2, total ~ julian*year, family = "poisson")
summary(fit)

fit <- glm(data=df_epfu2, total ~ type * year + type* julian, family = "poisson")
summary(fit)

fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2019"), total ~ type, family = "poisson")
summary(fit)
fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2018"), total ~ type, family = "poisson")
summary(fit)
fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2020"), total ~ type, family = "poisson")
summary(fit)

fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2020"), total ~ tempmin*type, family = "poisson")
summary(fit)

fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2020"), total ~ julian2*type + julian + tempmin, family = "poisson")
summary(fit)

fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2020"&df_epfu2$type=="Edge"), total ~ julian2 + julian, family = "poisson")
summary(fit)

fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2020"&df_epfu2$type=="Open"), total ~ julian2 + julian, family = "poisson")
summary(fit)

fit <- glm(data=dfe, total ~ julian + tempmin, family = "poisson")
summary(fit)
fit <- glm(data=dfe, total ~ julian2 + tempmin, family = "poisson")
summary(fit)
fit <- glm(data=dfe, total ~ julian2 + julian + tempmin, family = "poisson")
summary(fit)
fit <- glm(data=dfe, total ~ julian2 + julian + tempmin2 + tempmin, family = "poisson")
summary(fit)
#fit <- glm(data=subset(dfe, dfe$year=="2020"), total ~ julian2 + julian + tempmin2 + tempmin, family = "poisson")
#summary(fit)
#fit <- glm(data=subset(dfe, dfe$year!="2020"), total ~ julian2 + julian + tempmin2 + tempmin, family = "poisson")
#summary(fit)
fit <- glm(data=dfe, total ~ type * (julian2 + julian) + tempmin2 + tempmin, family = "poisson")
summary(fit)
#fit <- glm(data=subset(dfe, dfe$year=="2020"), total ~ type * (julian2 + julian) + tempmin2 + tempmin, family = "poisson")
#summary(fit)
#fit <- glm(data=subset(dfe, dfe$year!="2020"), total ~ type * (julian2 + julian) + tempmin2 + tempmin, family = "poisson")
#summary(fit)


fit <- glm(data=df_epfu2, ratio ~ type + year, family = "quasibinomial")
summary(fit)

fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2020"), ratio ~ julian, family = "quasibinomial")
summary(fit)
fit <- glm(data=subset(df_epfu2, df_epfu2$year=="2020"), ratio ~ julian2 + julian, family = "quasibinomial")
summary(fit)

ggplot(data=df1[df1$AUTO.ID.=="EPTFUS",], aes(x=HOUR.12, fill=type)) + 
  #geom_density(alpha=.3) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_x_continuous(breaks=seq(9, 17 ,1), 
                     labels = c("9:00", "10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "4:00", "5:00"),
                     name = "Time") +
  ylab("Number of bat passes") +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 2), 
                    name = "Habitat type")+
  facet_grid(year~., scales = "free")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  ) 

ggplot(data=df1[df1$AUTO.ID.=="LASBOR",], aes(x=HOUR.12, fill=type)) + 
  #geom_density(alpha=.3) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_x_continuous(breaks=seq(9, 17 ,1), 
                     labels = c("9:00", "10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "4:00", "5:00"),
                     name = "Time") +
  ylab("Number of bat passes") +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 2), 
                    name = "Habitat type")+
  facet_grid(year~., scales = "free")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  ) 

ggplot(data=df1[df1$AUTO.ID.=="LASCIN",], aes(x=HOUR.12, fill=type)) + 
  #geom_density(alpha=.3) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_x_continuous(breaks=seq(9, 17 ,1), 
                     labels = c("9:00", "10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "4:00", "5:00"),
                     name = "Time") +
  ylab("Number of bat passes") +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 2), 
                    name = "Habitat type")+
  facet_grid(year~., scales = "free")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  ) 

ggplot(data=df1[df1$AUTO.ID.=="LASNOC",], aes(x=HOUR.12, fill=type)) + 
  #geom_density(alpha=.3) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_x_continuous(breaks=seq(9, 17 ,1), 
                     labels = c("9:00", "10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "4:00", "5:00"),
                     name = "Time") +
  ylab("Number of bat passes") +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 2), 
                    name = "Habitat type")+
  facet_grid(year~., scales = "free")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  ) 

ggplot(data=df1[df1$AUTO.ID.=="PERSUB",], aes(x=HOUR.12, fill=type)) + 
  #geom_density(alpha=.3) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_x_continuous(breaks=seq(9, 17 ,1), 
                     labels = c("9:00", "10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "4:00", "5:00"),
                     name = "Time") +
  ylab("Number of bat passes") +
  scale_fill_manual(values = wes_palette("Moonrise3", n = 2), 
                    name = "Habitat type")+
  facet_grid(year~., scales = "free")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  ) 

#########data collected in Glacier Creek Preserve in 2018-2020##############
#########diel activity of bats at a fine scale ##################
install.packages('overlap')
install.packages('ggdist')

library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(suncalc)
library(lubridate)
library(overlap)
library(dplyr)
library(mgcv)
library(ggpubr)
library(ggdist)

##########################data manipulation####################################
df2018 <- read.csv("2018.csv")
df2018$year <- "2018"
df2019 <- read.csv("2019.csv")
df2019$year <- "2019"
df2020 <- read.csv("2020.csv")
df2020$year <- "2020"

df2018$DATE <- as.Date(df2018$DATE, format = "%m/%d/%Y")
df2019$DATE <- as.Date(df2019$DATE, format = "%m/%d/%Y")
df2020$DATE <- as.Date(df2020$DATE, format = "%Y-%m-%d")

df <- rbind(df2018, df2019)
df <- rbind(df, df2020)

df <- subset(df, df$MATCH.RATIO >= 0.6)
df$site <- str_sub(df$FOLDER, end = 6)
df$type <- str_sub(df$site, end = 4)
df$year <- factor(df$year)
df1 <- subset(df, df$year=="2018"|df$year=="2019"| (df$DATE<"2020-07-16" & df$DATE>"2020-06-30"))

class(df$HOUR.12)
class(df$DATE)

df2 <- df1[c(10, 11, 15, 16, 45:47)]
df3 <- aggregate(df2$AUTO.ID., by=list(df2$DATE, df2$AUTO.ID., df2$HOUR.12, df2$year, df2$site, df2$type), FUN = length)
df3 <- aggregate(df2$AUTO.ID., by=list( df2$AUTO.ID.,  df2$year,  df2$type), FUN = length)

names(df01)[1] <- "date"
names(df01)[2] <- "bin4"
names(df01)[3] <- "site"
names(df01)[4] <- "id"
names(df01)[5] <- "alan"
names(df01)[6] <- "region"
names(df01)[7] <- "moonphase"
names(df01)[8] <- "count"

sum_table <- df1 %>%
  group_by(site) %>%
  summarise(
    MYOAUS_sum = sum(MYOAUS),
    MYOGRI_sum = sum(MYOGRI),
    MYOLUC_sum = sum(MYOLUC),
    MYOSEP_sum = sum(MYOSEP)
  )

##############################buzz data sorting##########################
df <- read.csv("buzz.csv")
df <- read.csv("all.csv")
df$SppAccp[df$SppAccp == ""] <- "noid"
df$date <- as.Date(df$MonitoringNight, format = "%m/%d/%Y")

df$sunset <- getSunlightTimes(date = df$date, 
                              lat = 41.249888, lon = -96.008715, 
                              keep = "sunset", tz ="+5")
df$sunsettime <- str_sub(df$sunset$sunset, -8)
write.csv(df, "buzz10.csv")

df$sunrise <- getSunlightTimes(date = df$date, 
                              lat = 41.249888, lon = -96.008715, 
                              keep = "sunrise", tz ="+5")
df$sunrisetime <- str_sub(df$sunrise$sunrise, -8)


df$pass <- df$time - df$sunsettime
time = format(dates, format = "%H:%M:%S")
ggplot(data=df[df$Auto.Buzz.Count>0,], aes(x=type, y=Auto.Buzz.Count)) +
  geom_boxplot()

################################descriptive ######################################################
df<- read.csv("buzz10.csv")
df$nightcent_f <- as.factor(df$nightcent_f)
df$hour_f <- as.factor(df$hour_f)
df$date <- as.Date(df$MonitoringNight, format = "%m/%d/%Y")
df$buzz <- as.factor(df$buzz)
df[is.na(df)] <- 0
df$year <- as.factor(df$year)
df$month <- as.factor(df$month)
df$julian <- yday(df$date)

df1 <- aggregate(df$SppAccp, by=list(df$date, df$NextDirUp, df$type, df$SppAccp,  df$buzz, df$year, df$month), FUN = length)
names(df1)[1] <- "date"
names(df1)[2] <- "site"
names(df1)[3] <- "type"
names(df1)[4] <- "species"
#names(df1)[5] <- "hour"
names(df1)[5] <- "buzz"
names(df1)[6] <- "year"
names(df1)[7] <- "month"
names(df1)[8] <- "count"

df2 <- spread(df1, species, count)
df2[is.na(df2)] <- 0

##############weather data##############
daily <- read.csv("daily.csv")
class(daily$date)
daily$date <- as.Date(daily$datetime, format = "%Y-%m-%d")
daily$year <- as.factor(daily$year)

ggplot(data=daily, aes(x=date, y=tempmin) )+
  geom_point(shape=19, alpha=0.9) + 
  facet_grid( .~ year, scales = "free")

#######eliminate sites, only in 2020##########
#######"Open 1 (Northwest prairie) 66" "Open 3 (Big cottonwood) 46"###########
####### general descriptive##########
df11 <- aggregate(df$buzz, by=list(df$year,  df$type, df$SppAccp, df$buzz), FUN = length)
names(df11)[1] <- "year"
#names(df11)[2] <- "site"
names(df11)[2] <- "type"
names(df11)[3] <- "species"
names(df11)[4] <- "buzz"
names(df11)[5] <- "count"

df22 <- spread(df11, species, count)
df22[is.na(df22)] <- 0

write.csv(df22, "df22.csv")


df3<- rbind.data.frame(df2, df22)
df3$id <- 1:1854
df3$id <- as.factor(df3$id)

write.csv(df3, "df3.csv")
df31 <- read.csv("df31.csv")

df4 <- gather(df3, hour, count, 6:14, factor_key=TRUE)
df4$hour <- as.factor(df4$hour)
ggplot(data=subset(df4, df4$species=="Epfu"), aes(x=hour, y=count, group=id)) +
  geom_line()+
  facet_wrap(~buzz, scales = "free")

ggplot(data=subset(df4, df4$species=="Epfu"), aes(x=hour, y=buzz, size=count)) +
  geom_point(alpha=0.3, position = position_jitter(width = 0.3, height = 0))
ggplot(data=subset(df4, df4$species=="Epfu"), aes(x=hour, y=buzz, size=count)) +
  geom_point(alpha=0.05,)


ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=type, y=count, fill=buzz)) +
  geom_boxplot()
ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=hour, y=log(count), fill=buzz)) +
  geom_boxplot()
ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=hour, y=log(count), fill=type)) +
  geom_boxplot() +
  facet_wrap(~buzz, scales = "free")
ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=hour, y=buzz, fill=count)) +
  geom_tile()


#####http://plantecology.syr.edu/fridley/bio793/gam.html###

###epfu-extra####
df_e-extrapfu <- df2[c(1:6, 8)]
df_epfu1 <- spread(df_epfu, buzz, Epfu) 
df_epfu1[is.na(df_epfu1)] <- 0
names(df_epfu1)[6] <- "commuting"
names(df_epfu1)[7] <- "feeding"
df_epfu1$total <- df_epfu1$commuting + df_epfu1$feeding
df_epfu2 <- subset(df_epfu1, df_epfu1$total>0)
df_epfu2$ratio <- df_epfu2$feeding/df_epfu2$total
df_epfu2$julian <- yday(df_epfu2$date)
df_epfu2$julian2 <- df_epfu2$julian^2
df_epfu2 <- merge(df_epfu2, daily, by = "date")

dfe <- subset(df_epfu2, df_epfu2$site != "Open 1 (Northwest prairie) 66" & df_epfu2$site != "Open 3 (Big cottonwood) 46")
dfe$tempmin2 <- dfe$tempmin^2
###########################
df_epfu <- df2[c(1:6, 8)]
df_epfu1 <- spread(df_epfu, buzz, Epfu) 
df_epfu1[is.na(df_epfu1)] <- 0
names(df_epfu1)[6] <- "commuting"
names(df_epfu1)[7] <- "feeding"
df_epfu1$total <- df_epfu1$commuting + df_epfu1$feeding
df_epfu2 <- subset(df_epfu1, df_epfu1$total>0)
df_epfu2$ratio <- df_epfu2$feeding/df_epfu2$total
df_epfu2$julian <- yday(df_epfu2$date)
df_epfu2$julian2 <- df_epfu2$julian^2
df_epfu2 <- merge(df_epfu2, daily, by = "date")

dfepfu <- subset(df_epfu2, df_epfu2$site != "Open 1 (Northwest prairie) 66" & df_epfu2$site != "Open 3 (Big cottonwood) 46")



###########gam############
###total#######
fit1 <- gam(data=subset(dfe, dfe$year=="2020"), total~s(julian), family=poisson)
summary(fit1)  
#fit2 <- glm(data=dfe, total ~ julian , family = "poisson")
#summary(fit2)
#fit3 <- glm(data=dfe, total ~ julian + I(julian^2) , family = "poisson")
#summary(fit3)
fit4 <- gam(data=subset(dfe, dfe$year=="2020"), total~s(julian) + s(tempmin), family=poisson)
summary(fit4) 
fit5 <- gam(data=subset(dfe, dfe$year=="2020"), total~s(julian) + s(tempmin) + type, family=poisson)
summary(fit5)
fit6 <- gam(data=subset(dfe, dfe$year=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)
fit7 <- gam(data=subset(dfe, dfe$year=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin) +type, family=negbin(3))
summary(fit7)

fit7 <- gam(data=dfe, total~s(julian) + s(tempmin) + te(julian, tempmin) +type, family=negbin(3))
summary(fit7)

fit <- gam(data = subset(dfgraph, dfgraph$species=="epfu"), total~s(julian) + s(tempmin) + type, family=poisson)
summary(fit)
fit <- gam(data = subset(dfgraph, dfgraph$species=="epfu"), ratio~s(julian) + s(tempmin) + type)
summary(fit)
fit <- glm(data = subset(dfgraph, dfgraph$species=="epfu"), ratio~julian + tempmin + type, family=quasibinomial())
summary(fit)
###ratio###
###https://rcompanion.org/handbook/J_02.html###

fit1 <- gam(data=subset(dfe, dfe$year=="2020"), feeding~s(julian) + offset(log(total)), family=poisson)
summary(fit1)
#fit2 <- glm(data=subset(dfe, dfe$year=="2020"), ratio~julian, family=quasibinomial())
#summary(fit2)
fit4 <- gam(data=subset(dfe, dfe$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin), family=poisson())
summary(fit4) 
fit5 <- gam(data=subset(dfe, dfe$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + type, family=poisson())
summary(fit5)
fit5 <- gam(data=subset(dfe, dfe$year=="2020"), feeding~s(julian) + offset(log(total))  + type, family=negbin(3))
summary(fit5)

fit6 <- gam(data=subset(dfe, dfe$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)
fit7 <- gam(data=subset(dfe, dfe$year=="2020"), feeding~s(julian) + offset(log(total))  + te(julian, tempmin) + type, family=negbin(3))
summary(fit7)

fit7 <- gam(data=dfe, ratio~s(julian)  + type, family=negbin(3))
summary(fit7)

AIC(fit1, fit4)
anova(fit4,fit6, test = "Chisq")


ggplot(data=dfe, aes(x=type, y=ratio)) +
  geom_boxplot()

+
  facet_grid(. ~ year, scales = "free" )

ggplot(data=df_epfu2, aes(x=type, y=ratio)) +
  geom_boxplot()+
  facet_grid(year ~ ., scales = "free" )

ggplot(data=subset(df_epfu2, df_epfu2$year=="2020"), aes(x=date, y=ratio, fill=type, color=type)) +
  geom_point(shape=19, alpha=0.9,
             position=position_jitter(width=0.01, height=0.01)) + 
#geom_boxplot()+
  facet_grid(site ~ year, scales = "free" )

#subset(dfe, dfe$year=="2020")
ggplot(data=subset(dfe, dfe$year=="2020"), aes(x=date, y=total, fill=type, color=type)) +
  geom_point(shape=19, alpha=0.9,
             position=position_jitter(width=0.01, height=0.01)) + 
  geom_smooth() + 
  #geom_boxplot()+
  facet_grid(site ~ year, scales = "free" )

###laci####
df_laci <- df2[c(1:6, 10)]
df_laci1 <- spread(df_laci, buzz, Laci) 
df_laci1[is.na(df_laci1)] <- 0
names(df_laci1)[6] <- "commuting"
names(df_laci1)[7] <- "feeding"
df_laci1$total <- df_laci1$commuting + df_laci1$feeding
df_laci2 <- subset(df_laci1, df_laci1$total>0)
df_laci2$ratio <- df_laci2$feeding/df_laci2$total
df_laci2$julian <- yday(df_laci2$date)
df_laci2$julian2 <- df_laci2$julian^2
df_laci2 <- merge(df_laci2, daily, by = "date")

dflaci <- subset(df_laci2, df_laci2$site != "Open 1 (Northwest prairie) 66" & df_laci2$site != "Open 3 (Big cottonwood) 46")

###total#######
fit1 <- gam(data=subset(dflaci, dflaci$year.x=="2020"), total~s(julian), family=poisson)
summary(fit1)  
#fit2 <- glm(data=dflaci, total ~ julian , family = "poisson")
#summary(fit2)
#fit3 <- glm(data=dflaci, total ~ julian + I(julian^2) , family = "poisson")
#summary(fit3)
fit4 <- gam(data=subset(dflaci, dflaci$year.x=="2020"), total~s(julian) + s(tempmin), family=poisson)
summary(fit4) 
fit5 <- gam(data=subset(dflaci, dflaci$year.x=="2020"), total~s(julian) + s(tempmin) + type, family=poisson)
summary(fit5)
fit6 <- gam(data=subset(dflaci, dflaci$year.x=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)
fit7 <- gam(data=subset(dflaci, dflaci$year.x=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin) +type, family=negbin(3))
summary(fit7)

###ratio###
###https://rcompanion.org/handbook/J_02.html###

fit1 <- gam(data=subset(dflaci, dflaci$year=="2020"), feeding~s(julian) + offset(log(total)), family=poisson)
summary(fit1)
#fit2 <- glm(data=subset(dflaci, dflaci$year=="2020"), ratio~julian, family=quasibinomial())
#summary(fit2)
fit4 <- gam(data=subset(dflaci, dflaci$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin), family=poisson())
summary(fit4) 
fit5 <- gam(data=subset(dflaci, dflaci$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + type, family=poisson())
summary(fit5)
fit6 <- gam(data=subset(dflaci, dflaci$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)
fit7 <- gam(data=subset(dflaci, dflaci$year.x=="2020"), feeding~s(julian) + offset(log(total))  + te(julian, tempmin) + type, family=negbin(3))
summary(fit7)

AIC(fit1, fit4)
anova(fit4,fit6, test = "Chisq")

###lano####
df_lano <- df2[c(1:6, 11)]
df_lano1 <- spread(df_lano, buzz, Lano) 
df_lano1[is.na(df_lano1)] <- 0
names(df_lano1)[6] <- "commuting"
names(df_lano1)[7] <- "feeding"
df_lano1$total <- df_lano1$commuting + df_lano1$feeding
df_lano2 <- subset(df_lano1, df_lano1$total>0)
df_lano2$ratio <- df_lano2$feeding/df_lano2$total
df_lano2$julian <- yday(df_lano2$date)
df_lano2$julian2 <- df_lano2$julian^2
df_lano2 <- merge(df_lano2, daily, by = "date")

dflano <- subset(df_lano2, df_lano2$site != "Open 1 (Northwest prairie) 66" & df_lano2$site != "Open 3 (Big cottonwood) 46")

###total#######
fit1 <- gam(data=subset(dflano, dflano$year.x=="2020"), total~s(julian), family=poisson)
summary(fit1)  
#fit2 <- glm(data=dflano, total ~ julian , family = "poisson")
#summary(fit2)
#fit3 <- glm(data=dflano, total ~ julian + I(julian^2) , family = "poisson")
#summary(fit3)
fit4 <- gam(data=subset(dflano, dflano$year.x=="2020"), total~s(julian) + s(tempmin), family=poisson)
summary(fit4) 
fit5 <- gam(data=subset(dflano, dflano$year.x=="2020"), total~s(julian) + s(tempmin) + type, family=poisson)
summary(fit5)
fit6 <- gam(data=subset(dflano, dflano$year.x=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)
fit7 <- gam(data=subset(dflano, dflano$year.x=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin) +type, family=negbin(3))
summary(fit7)

###ratio###
###https://rcompanion.org/handbook/J_02.html###

fit1 <- gam(data=subset(dflano, dflano$year=="2020"), feeding~s(julian) + offset(log(total)), family=poisson)
summary(fit1)
#fit2 <- glm(data=subset(dflano, dflano$year=="2020"), ratio~julian, family=quasibinomial())
#summary(fit2)
fit4 <- gam(data=subset(dflano, dflano$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin), family=poisson())
summary(fit4) 
fit5 <- gam(data=subset(dflano, dflano$year.x=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + type, family=poisson())
summary(fit5)
fit6 <- gam(data=subset(dflano, dflano$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)
fit7 <- gam(data=subset(dflano, dflano$year.x=="2020"), feeding~s(julian) + offset(log(total))  + te(julian, tempmin) + type, family=negbin(3))
summary(fit7)



AIC(fit1, fit4)
anova(fit4,fit6, test = "Chisq")

###labo####
df_labo <- df2[c(1:6, 9)]
df_labo1 <- spread(df_labo, buzz, Labo) 
df_labo1[is.na(df_labo1)] <- 0
names(df_labo1)[6] <- "commuting"
names(df_labo1)[7] <- "feeding"
df_labo1$total <- df_labo1$commuting + df_labo1$feeding
df_labo2 <- subset(df_labo1, df_labo1$total>0)
df_labo2$ratio <- df_labo2$feeding/df_labo2$total
df_labo2$julian <- yday(df_labo2$date)
df_labo2$julian2 <- df_labo2$julian^2
df_labo2 <- merge(df_labo2, daily, by = "date")
dflabo <- subset(df_labo2, df_labo2$site != "Open 1 (Northwest prairie) 66" & df_labo2$site != "Open 3 (Big cottonwood) 46")

###total#######
fit1 <- gam(data=subset(dflabo, dflabo$year.x=="2020"), total~s(julian), family=poisson)
summary(fit1)  
#fit2 <- glm(data=dflabo, total ~ julian , family = "poisson")
#summary(fit2)
#fit3 <- glm(data=dflabo, total ~ julian + I(julian^2) , family = "poisson")
#summary(fit3)
fit4 <- gam(data=subset(dflabo, dflabo$year.x=="2020"), total~s(julian) + s(tempmin), family=poisson)
summary(fit4) 
fit5 <- gam(data=subset(dflabo, dflabo$year.x=="2020"), total~s(julian) + s(tempmin) + type, family=poisson)
summary(fit5)
fit6 <- gam(data=subset(dflabo, dflabo$year.x=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)

###ratio###fit7 <- gam(data=subset(dflabo, dflabo$year.x=="2020"), total~s(julian) + s(tempmin) + te(julian, tempmin) +type, family=negbin(3))
summary(fit7)

###https://rcompanion.org/handbook/J_02.html###

fit1 <- gam(data=subset(dflabo, dflabo$year=="2020"), feeding~s(julian) + offset(log(total)), family=poisson)
summary(fit1)
#fit2 <- glm(data=subset(dflabo, dflabo$year=="2020"), ratio~julian, family=quasibinomial())
#summary(fit2)
fit4 <- gam(data=subset(dflabo, dflabo$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin), family=poisson())
summary(fit4) 
fit5 <- gam(data=subset(dflabo, dflabo$year.x=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + type, family=poisson())
summary(fit5)
fit6 <- gam(data=subset(dflabo, dflabo$year=="2020"), feeding~s(julian) + offset(log(total)) + s(tempmin) + te(julian, tempmin), family=poisson)
summary(fit6)
fit7 <- gam(data=subset(dflabo, dflabo$year.x=="2020"), feeding~s(julian) + offset(log(total))  + te(julian, tempmin) + type, family=negbin(3))
summary(fit7)

AIC(fit1, fit4)
anova(fit4,fit6, test = "Chisq")

###########graph###########
#########https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
dfepfu$species <- "epfu"
dflabo$species <- "labo"
dflaci$species <- "laci"
dflano$species <- "lano"

dfgraph <- rbind(dfepfu, dflabo)
dfgraph <- rbind(dfgraph, dflaci)
dfgraph <- rbind(dfgraph, dflano)

write.csv(dfgraph, "dfgraph.csv")
dfgraph <- read.csv("dfgraph.csv")
dfgraph <- read.csv("dfgraph1.csv")

foragingratio <- ggplot(dfgraph, aes(x=type, y=ratio, color=type)) + 
  scale_color_manual(values = c("#474648", "#D71920"), name= "Habitat type") +
  geom_boxplot(width = .35, outlier.alpha = 0, lwd=0.9) + 
  geom_point(shape = 19,size = 1.5, alpha = .3,
             position = position_jitterdodge() )+
  xlab("Habitat type") +
  ylab("Foraging ratio") + 
 # stat_halfeye(    adjust = .9, width = 1, .width = 0,  
                  # justification = -.2, point_colour = NA) +
  facet_wrap(~species, scales = "free", ncol=2,  
             labeller = labeller(species= c(epfu="EPFU", labo="LABO",
                                            laci="LACI", lano="LANO"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 


ggplot(data=dfgraph, aes(x=type, y=total, fill=type)) + 
  scale_fill_manual(values = c("red", "blue")) +
  #stat_halfeye(    adjust = .9, 
                 #  width = 1, 
                 #  .width = 0, 
                  # justification = -.2, 
                 #  point_colour = NA) + 
  geom_boxplot(width = .35) + 
 # geom_point(shape = 95,size = 15, alpha = .2, color = "#1D785A")+
  stat_dots(
    side = "left", 
    dotsize = 2, 
    justification = 1.4, 
    binwidth = 1) +
  #geom_violin() +
  #geom_errorbar(data=totalmean, aes(x=type, ymin=lower, ymax=upper))+ 
  xlab("Habitat type") +
  ylab("Bat activity (# of passes per night") + 
  facet_wrap(~species,  scales = "free", ncol=2, 
             labeller = labeller(species= c(epfu="EPFU", labo="LABO",                                            
                                            laci="LACI", lano="LANO"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  )  


totalmean <- dfgraph %>% 
  group_by(species, type) %>%
  summarise(mean_total=mean(total),
            sd_total=sd(total), 
            n_total=n(), 
            se_total=sd_total/sqrt(n_total), 
            upper=mean_total+1.96*se_total,
            lower=mean_total-1.96*se_total
            )

ratiomean <- dfgraph %>% 
  group_by(species, type) %>%
  summarise(mean_ratio=mean(ratio),
            sd_ratio=sd(ratio), 
            n_ratio=n(), 
            se_ratio=sd_ratio/sqrt(n_ratio), 
            upper=mean_ratio+1.96*se_ratio,
            lower=mean_ratio-1.96*se_ratio
  )

totaljulian <- ggplot(data=subset(dfgraph, dfgraph$year.x=="2020"), aes(x=julian, y=total, color=type)) + 
  scale_color_manual(values = c("#474648", "#D71920"), name= "Habitat type") +
  scale_x_continuous(expand = c(0, 1)) +
  geom_smooth(method = "gam") + 
   geom_point(shape = 19,size = 2, alpha = .4,
              position = position_jitterdodge() )+
  xlab("Julian date") +
  ylab("Bat activity (# of passes per night)") + 
  facet_wrap(~species,  scales = "free", ncol=2, 
             labeller = labeller(species= c(epfu="EPFU", labo="LABO",                                            
                                            laci="LACI", lano="LANO"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = c(0.9, 0.85),
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  )  

ggplot(data=dfgraph, aes(x=julian, y=ratio, color=type)) + 
  scale_color_manual(values = c("#474648", "#D71920"), name= "Habitat type") +
  geom_smooth(method = "gam") + 
  geom_point(shape = 20,size = 2, alpha = .2,
             position = position_jitterdodge() )+
  xlab("Julian date") +
  ylab("Bat activity (# of passes per night)") + 
  facet_wrap(~species,  scales = "free", ncol=2, 
             labeller = labeller(species= c(epfu="EPFU", labo="LABO",                                            
                                            laci="LACI", lano="LANO"))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  )  

fig1 <- ggarrange(totaljulian, foragingratio, ncol = 1,
          labels = c("A", "B" ),
          label.x = 0.02,
          label.y = 1,
          font.label = list(size = 16, color = "black", face = "bold", family = "serif"),
          common.legend = FALSE)

ggsave("Figure 1-new.jpg",plot=fig1, 
       dpi=900, dev='jpg', height=25, width=25, units="cm")


#############predict foraging###########
df$feeding <- 0
df[df$Auto.Buzz.Count>0,]$feeding<-1
df$year1 <- as.factor(df$year)

fit <- glm(data=df, feeding~type, family="binomial")
summary(fit)
fit <- glm(data=subset(df, df$SppAccp=="Epfu"), feeding~type*year1, family="binomial")
summary(fit)
fit <- glm(data=subset(df, df$SppAccp=="Epfu"), feeding~nightcent, family="binomial")
summary(fit)
fit <- glm(data=subset(df, df$SppAccp=="Epfu"), feeding~nightcent*type, family="binomial")
summary(fit)

df_epfu <- subset(df, df$SppAccp=="Epfu")
ggplot(data=df_epfu, aes(x=nightcent, y=feeding)) +
  geom_point(shape=1, alpha=0.05,
             position=position_jitter(width=0.01, height=0.01))+
  facet_grid(year1~type)



################## overlap package##########
###########################################################
dfo2 <- read.csv("buzz10.csv")
dfo2 <- subset(dfo2, dfo2$nightcent<1)
dfo2$timerad <- dfo2$nightcent *2 *pi
dfo2$year <- as.factor(dfo2$year)
dfo2 <- subset(dfo2, dfo2$NextDirUp != "Open 1 (Northwest prairie) 66" 
               & dfo2$NextDirUp != "Open 3 (Big cottonwood) 46")
 
df10 <- df2 %>%
  group_by(SppAccp, date, type, year, NextDirUp) %>%
  summarize(count = length(buzz))

df11 <- spread(df10, SppAccp, count)
df11[is.na(df11)] <- 0

###### preserve level#####
############ extra ################
overlapPlot(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad)
overlapEst(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
           dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad, 
           type="Dhat1")
overlapPlot(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad)


overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
           dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad, 
           type="Dhat1")


overlapPlot(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad)

overlapPlot(dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad)

overlapPlot(dfo2[(dfo2$SppAccp=="Tabr"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Tabr"&dfo2$type=="Edge"),]$timerad)

tabr_est <- overlapEst(dfo2[(dfo2$SppAccp=="Tabr"&dfo2$type=="Open"),]$timerad,
                       dfo2[(dfo2$SppAccp=="Tabr"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
tabr_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Tabr"&dfo2$type=="Open"),]$timerad,
                        dfo2[(dfo2$SppAccp=="Tabr"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(tabr_boots)
hist(tabr_boots)
bootCI(tabr_est, tabr_boots)

tabr_oe <- ggplot(data=dfo2[(dfo2$SppAccp=="Tabr"), ], aes(x=nightcent, fill=type, 
                                                           after_stat(density)))+
  geom_density(alpha=0.5)+
  ylab("Activity density") +
  xlab("Night") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5,0.75, 1), 
                     labels = c("Sunset", "Quarter of night", 
                                "Half of night", "Three quarters of night",
                                "Sunrise")) + 
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#c4aa23")) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=12),
    axis.text = element_text(family="serif", size =14)
  ) 

dfo2$SppAccp=="Tabr"|
  overlapPlot(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
              dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad)


overlapEst(dfo2[(dfo2$SppAccp=="Epfu"),]$timerad,
           dfo2[(dfo2$SppAccp=="Labo"),]$timerad, 
           type="Dhat1")
overlapEst(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
           dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad, 
           type="Dhat1")


#dflabo <- dfo2[dfo2$SppAccp=="Labo",]
overlapPlot(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2018"),]$timerad,
            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2019"),]$timerad)
densityPlot(dfo2[dfo2$SppAccp=="Epfu",]$timerad)
overlapPlot(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad)
overlapPlot(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2018"),]$timerad,
            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2019"),]$timerad)
overlapPlot(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2018"&dfo2$type=="Edge"),]$timerad,
            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2019"&dfo2$type=="Edge"),]$timerad)
overlapPlot(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2018"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2019"&dfo2$type=="Open"),]$timerad)


overlapEst(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
           dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad, 
           type="Dhat1")
overlapEst(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2018"),]$timerad,
           dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2019"),]$timerad, 
           type="Dhat1")
overlapEst(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2018"&dfo2$type=="Open"),]$timerad,
           dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2019"&dfo2$type=="Open"),]$timerad, 
           type="Dhat1")
overlapEst(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2018"&dfo2$type=="Edge"),]$timerad,
           dfo2[(dfo2$SppAccp=="Epfu"&dfo2$year=="2019"&dfo2$type=="Edge"),]$timerad, 
           type="Dhat1")

densityPlot(dfo2[dfo2$SppAccp=="Labo",]$timerad)
overlapPlot(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
            dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad)
overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
           dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad, 
           type="Dhat1")

dfplot1 <- subset(dfo2, dfo2$nightcent<1 & dfo2$SppAccp=="Labo")
dfplot1$timerad <- dfplot1$nightcent * 2 * pi
laboden <- densityPlot(dfplot1$timerad)

dfplot2 <- subset(dfo2, dfo2$nightcent<1 & dfo2$SppAccp=="Lano")
dfplot2$timerad <- dfplot2$nightcent * 2 * pi
densityPlot(dfplot2$timerad)

dfplot3 <- subset(dfo2, dfo2$nightcent<1 & dfo2$SppAccp=="Laci")
dfplot3$timerad <- dfplot3$nightcent * 2 * pi
densityPlot(dfplot3$timerad)

overlapPlot(dfplot1$timerad, dfplot2$timerad)
overlapPlot(dfplot2$timerad, dfplot3$timerad)
overlapPlot(dfplot1$timerad, dfplot3$timerad)

est <- overlapEst(dfplot1$timerad, dfplot2$timerad, type="Dhat4")
boots <- bootstrap(dfplot1$timerad, dfplot2$timerad, 999, type="Dhat4")
mean(boots)
hist(boots)
bootCI(est, boots)

est <- overlapEst(dfplot3$timerad, dfplot2$timerad, type="Dhat4")
boots <- bootstrap(dfplot3$timerad, dfplot2$timerad, 999, type="Dhat4")
mean(boots)
hist(boots)
bootCI(est, boots)

est <- overlapEst(dfplot3$timerad, dfplot1$timerad, type="Dhat4")
boots <- bootstrap(dfplot3$timerad, dfplot1$timerad, 999, type="Dhat4")
mean(boots)
hist(boots)
bootCI(est, boots)

##### intra-species open vs edge #############
#####epfu##########
epfu_est <- overlapEst(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
                       dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
epfu_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,
                        dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_boots)
hist(epfu_boots)
bootCI(epfu_est, epfu_boots)

epfu_oe <- ggplot(data=dfo2[(dfo2$SppAccp=="Epfu"), ], aes(x=nightcent, fill=type, 
                                                after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
    #breaks = c(0, 0.25, 0.5,0.75, 1), 
                     #labels = c("Sunset", "Quarter of night", 
                               # "Half of night", "Three quarters of night",
                               # "Sunrise")) + 
  scale_fill_manual(name = "Habitat type", 
                      values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.7, y=0.95, label = expression("EPFU" ~ Delta ~ "= 0.965"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

#########labo############

labo_est <- overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                       dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad,type="Dhat1")

labo_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                        dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(labo_boots)
hist(labo_boots)
bootCI(labo_est, labo_boots)

labo_oe <- ggplot(data=dfo2[(dfo2$SppAccp=="Labo"), ], 
                  aes(x=nightcent, fill=type, after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)
    #breaks = c(0, 0.25, 0.5,0.75, 1), 
                     #labels = c("Sunset", "Quarter of night", 
                                #"Half of night", "Three quarters of night",
                                #"Sunrise")
    ) + 
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.7, y=0.95, label = expression("LABO" ~ Delta ~ "= 0.923"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

##############laci#############
laci_est <- overlapEst(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,
                       dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
laci_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,
                        dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(laci_boots)
hist(laci_boots)
bootCI(laci_est, laci_boots)

laci_oe <- ggplot(data=dfo2[(dfo2$SppAccp=="Laci"), ], aes(x=nightcent, fill=type, 
                                                           after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)
    #breaks = c(0, 0.25, 0.5,0.75, 1), 
     #                labels = c("Sunset", "Quarter of night", 
      #                          "Half of night", "Three quarters of night",
       #                         "Sunrise")
    ) + 
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.7, y=0.95, label = expression("LACI" ~ Delta ~ "= 0.874"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

#################lano########
lano_est <- overlapEst(dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad,
                       dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
lano_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad,
                        dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(lano_boots)
hist(lano_boots)
bootCI(lano_est, lano_boots)

lano_oe <- ggplot(data=dfo2[(dfo2$SppAccp=="Lano"), ], aes(x=nightcent, fill=type, 
                                                           after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)
    #breaks = c(0, 0.25, 0.5,0.75, 1), 
     #                labels = c("Sunset", "Quarter of night", 
      #                          "Half of night", "Three quarters of night",
       #                         "Sunrise")
    ) + 
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.7, y=0.95, label = expression("LANO" ~ Delta ~ "= 0.912"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

#########merging#########
habitatoverlap <- ggarrange(epfu_oe, labo_oe, laci_oe, lano_oe,
          nrow = 2, ncol = 2, 
#          labels = c(expression("EPFU"~ Delta ~ "0.965"), 
#                     expression("LABO"~ Delta ~ "0.923"),
#                     expression("LACI"~ Delta ~ "0.874"),
#                     expression("LANO"~ Delta ~ "0.912")),
 #         label.x = 0.7,
#          label.y = 0.95,
          font.label = list(size = 16, color = "black", face = "bold", family = "serif"),
          common.legend = TRUE, legend = "right")


#####cross species in a habitat######
#############open############
epfu_labo_est <- overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,type="Dhat1")
epfu_labo_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad, 999, type="Dhat1")
mean(epfu_labo_boots)
hist(epfu_labo_boots)
bootCI(epfu_labo_est, epfu_labo_boots)

epfu_laci_est <- overlapEst(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,type="Dhat1")
epfu_laci_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad, 999, type="Dhat1")
mean(epfu_laci_boots)
hist(epfu_laci_boots)
bootCI(epfu_laci_est, epfu_laci_boots)

epfu_lano_est <- overlapEst(dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad,type="Dhat1")
epfu_lano_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Open"),]$timerad, 999, type="Dhat1")
mean(epfu_lano_boots)
hist(epfu_lano_boots)
bootCI(epfu_lano_est, epfu_lano_boots)


labo_laci_est <- overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                       dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,type="Dhat1")
labo_laci_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                        dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad, 999, type="Dhat1")
mean(labo_laci_boots)
hist(labo_laci_boots)
bootCI(labo_laci_est, labo_laci_boots)

labo_lano_est <- overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad,type="Dhat1")
labo_lano_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Open"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad, 999, type="Dhat1")
mean(labo_lano_boots)
hist(labo_lano_boots)
bootCI(labo_lano_est, labo_lano_boots)

laci_lano_est <- overlapEst(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad,type="Dhat1")
laci_lano_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Open"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Open"),]$timerad, 999, type="Dhat1")
mean(laci_lano_boots)
hist(laci_lano_boots)
bootCI(laci_lano_est, laci_lano_boots)


#############Edge##############
epfu_labo_est <- overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
epfu_labo_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_labo_boots)
hist(epfu_labo_boots)
bootCI(epfu_labo_est, epfu_labo_boots)

epfu_laci_est <- overlapEst(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
epfu_laci_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_laci_boots)
hist(epfu_laci_boots)
bootCI(epfu_laci_est, epfu_laci_boots)

epfu_lano_est <- overlapEst(dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
epfu_lano_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Epfu"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_lano_boots)
hist(epfu_lano_boots)
bootCI(epfu_lano_est, epfu_lano_boots)


labo_laci_est <- overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
labo_laci_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(labo_laci_boots)
hist(labo_laci_boots)
bootCI(labo_laci_est, labo_laci_boots)

labo_lano_est <- overlapEst(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
labo_lano_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Labo"&dfo2$type=="Edge"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(labo_lano_boots)
hist(labo_lano_boots)
bootCI(labo_lano_est, labo_lano_boots)


laci_lano_est <- overlapEst(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad,
                            dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad,type="Dhat1")
laci_lano_boots <- bootstrap(dfo2[(dfo2$SppAccp=="Laci"&dfo2$type=="Edge"),]$timerad,
                             dfo2[(dfo2$SppAccp=="Lano"&dfo2$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(laci_lano_boots)
hist(laci_lano_boots)
bootCI(laci_lano_est, laci_lano_boots)

####################graph######################
speciesoverlap <- ggplot(data=dfo2[((dfo2$SppAccp=="Lano"|dfo2$SppAccp=="Laci"|
                    dfo2$SppAccp=="Labo"|dfo2$SppAccp=="Epfu")), ], 
       aes(x=nightcent, fill=SppAccp, after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)
   # breaks = c(0, 0.25, 0.5,0.75, 1), 
                    # labels = c("Sunset", "Quarter of night", 
                          #      "Half of night", "Three quarters of night",
                             #   "Sunrise")
   ) + 
  scale_fill_manual(name = "Species", 
                    #  values = wes_palette("Darjeeling2", n = 5)) +
                  values = c("#ade292", "#0d5f8a", "#fd7f20","#905fd0")) + 
  facet_wrap(~type, nrow = 2) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

##########heat graph ################
#######https://r-charts.com/correlation/heat-map-ggplot2/########
########https://i0.wp.com/www.smartprix.com/bytes/wp-content/uploads/2022/06/flat-design-color-chart.png?ssl=1
########https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html###
heat1 <- read.csv("heat1.csv")
hgraph1 <- ggplot(heat1, aes(x=species1, y=species2, fill=level)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = label), family="serif", size = 7) +
  #geom_abline()+
  scale_fill_manual(values = c("#e6b0aa",
                               "#d98880",
                               "#cd6155",
                               "#fbfcfc",
                               "#99a3a4",
                               "#7f8c8d")
    ) + 
  xlab("") +
  ylab("") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "white"),
    axis.ticks = element_line(colour = "white"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

fig2part <- ggarrange(speciesoverlap, hgraph1, widths= c(2, 1), ncol = 2,
                      labels = c("B-1", "B-2" ),
                      label.x = 0,
                      label.y = 1,
                      font.label = list(size = 16, color = "black", face = "bold", family = "serif"),
                      common.legend = FALSE)

fig2 <- ggarrange(habitatoverlap, fig2part, ncol = 1,
                  labels = c("A", "" ),
                  label.x = 0,
                  label.y = 1,
                  font.label = list(size = 16, color = "black", face = "bold", family = "serif"),
                  common.legend = FALSE)

ggsave("Figure 2-r11.jpg",plot=fig2, 
       dpi=900, dev='jpg', height=28, width=48, units="cm")

#############overlapping of foraging#############
dfo3 <- subset(dfo2, dfo2$buzz >0)

##### intra-species open vs edge #############
#####epfu##########
epfu_est <- overlapEst(dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad,
                       dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
epfu_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad,
                        dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_boots)
hist(epfu_boots)
bootCI(epfu_est, epfu_boots)

epfu_oe <- ggplot(data=dfo3[(dfo3$SppAccp=="Epfu"), ], aes(x=nightcent, fill=type, 
                                                           after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
  breaks = c(0.25, 0.5,0.75), 
  #labels = c("Sunset", "Quarter of night", 
  # "Half of night", "Three quarters of night",
  # "Sunrise")) + 
  ) +
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.8, y=0.95, label = expression("EPFU" ~ Delta ~ "= 0.936"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

#########labo############

labo_est <- overlapEst(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                       dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad,type="Dhat1")

labo_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                        dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(labo_boots)
hist(labo_boots)
bootCI(labo_est, labo_boots)

labo_oe <- ggplot(data=dfo3[(dfo3$SppAccp=="Labo"), ], 
                  aes(x=nightcent, fill=type, after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100), 
                     breaks = c(0.25, 0.5,0.75)
                     #labels = c("Sunset", "Quarter of night", 
                     #"Half of night", "Three quarters of night",
                     #"Sunrise")
  ) + 
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.8, y=0.95, label = expression("LABO" ~ Delta ~ "= 0.898"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

##############laci#############
laci_est <- overlapEst(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad,
                       dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
laci_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad,
                        dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(laci_boots)
hist(laci_boots)
bootCI(laci_est, laci_boots)

laci_oe <- ggplot(data=dfo3[(dfo3$SppAccp=="Laci"), ], aes(x=nightcent, fill=type, 
                                                           after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)
                     #breaks = c(0, 0.25, 0.5,0.75, 1), 
                     #                labels = c("Sunset", "Quarter of night", 
                     #                          "Half of night", "Three quarters of night",
                     #                         "Sunrise")
  ) + 
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.8, y=0.95, label = expression("LACI" ~ Delta ~ "= 0.793"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

#################lano########
lano_est <- overlapEst(dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad,
                       dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
lano_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad,
                        dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(lano_boots)
hist(lano_boots)
bootCI(lano_est, lano_boots)

lano_oe <- ggplot(data=dfo3[(dfo3$SppAccp=="Lano"), ], aes(x=nightcent, fill=type, 
                                                           after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)
                     #breaks = c(0, 0.25, 0.5,0.75, 1), 
                     #                labels = c("Sunset", "Quarter of night", 
                     #                          "Half of night", "Three quarters of night",
                     #                         "Sunrise")
  ) + 
  scale_fill_manual(name = "Habitat type", 
                    values = c("#343c24", "#D71920")) + 
  annotate("text", x=0.8, y=0.95, label = expression("LANO" ~ Delta ~ "= 0.787"), 
           family="serif", size = 6) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

#########merging#########
habitatoverlap <- ggarrange(epfu_oe, labo_oe, laci_oe, lano_oe,
                            nrow = 2, ncol = 2, 
                            #labels = c("EPFU\n0.936", "LABO\n0.898", 
                             #          "LACI\n0.793", "LANO\n0.787" ),
                            #label.x = 0.8,
                            #label.y = 0.95,
                            font.label = list(size = 16, color = "black", face = "bold", family = "serif"),
                            common.legend = TRUE, legend = "right")


#####cross species in a habitat######
#############open############
epfu_labo_est <- overlapEst(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad,type="Dhat1")
epfu_labo_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad, 999, type="Dhat1")
mean(epfu_labo_boots)
hist(epfu_labo_boots)
bootCI(epfu_labo_est, epfu_labo_boots)

epfu_laci_est <- overlapEst(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad,type="Dhat1")
epfu_laci_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad, 999, type="Dhat1")
mean(epfu_laci_boots)
hist(epfu_laci_boots)
bootCI(epfu_laci_est, epfu_laci_boots)

epfu_lano_est <- overlapEst(dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad,type="Dhat1")
epfu_lano_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Open"),]$timerad, 999, type="Dhat1")
mean(epfu_lano_boots)
hist(epfu_lano_boots)
bootCI(epfu_lano_est, epfu_lano_boots)


labo_laci_est <- overlapEst(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad,type="Dhat1")
labo_laci_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad, 999, type="Dhat1")
mean(labo_laci_boots)
hist(labo_laci_boots)
bootCI(labo_laci_est, labo_laci_boots)

labo_lano_est <- overlapEst(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad,type="Dhat1")
labo_lano_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Open"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad, 999, type="Dhat1")
mean(labo_lano_boots)
hist(labo_lano_boots)
bootCI(labo_lano_est, labo_lano_boots)

laci_lano_est <- overlapEst(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad,type="Dhat1")
laci_lano_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Open"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Open"),]$timerad, 999, type="Dhat1")
mean(laci_lano_boots)
hist(laci_lano_boots)
bootCI(laci_lano_est, laci_lano_boots)


#############Edge##############
epfu_labo_est <- overlapEst(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
epfu_labo_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_labo_boots)
hist(epfu_labo_boots)
bootCI(epfu_labo_est, epfu_labo_boots)

epfu_laci_est <- overlapEst(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
epfu_laci_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_laci_boots)
hist(epfu_laci_boots)
bootCI(epfu_laci_est, epfu_laci_boots)

epfu_lano_est <- overlapEst(dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
epfu_lano_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Epfu"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(epfu_lano_boots)
hist(epfu_lano_boots)
bootCI(epfu_lano_est, epfu_lano_boots)


labo_laci_est <- overlapEst(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
labo_laci_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(labo_laci_boots)
hist(labo_laci_boots)
bootCI(labo_laci_est, labo_laci_boots)

labo_lano_est <- overlapEst(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
labo_lano_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Labo"&dfo3$type=="Edge"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(labo_lano_boots)
hist(labo_lano_boots)
bootCI(labo_lano_est, labo_lano_boots)


laci_lano_est <- overlapEst(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad,
                            dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad,type="Dhat1")
laci_lano_boots <- bootstrap(dfo3[(dfo3$SppAccp=="Laci"&dfo3$type=="Edge"),]$timerad,
                             dfo3[(dfo3$SppAccp=="Lano"&dfo3$type=="Edge"),]$timerad, 999, type="Dhat1")
mean(laci_lano_boots)
hist(laci_lano_boots)
bootCI(laci_lano_est, laci_lano_boots)

####################graph######################
speciesoverlap <- ggplot(data=dfo3[((dfo3$SppAccp=="Lano"|dfo3$SppAccp=="Laci"|
                                       dfo3$SppAccp=="Labo"|dfo3$SppAccp=="Epfu")), ], 
                         aes(x=nightcent, fill=SppAccp, after_stat(scaled)))+
  geom_density(alpha=0.7)+
  ylab("Activity density") +
  xlab("Percentage of the night") +
  scale_x_continuous(labels = scales::percent_format(scale = 100)
                     # breaks = c(0, 0.25, 0.5,0.75, 1), 
                     # labels = c("Sunset", "Quarter of night", 
                     #      "Half of night", "Three quarters of night",
                     #   "Sunrise")
  ) + 
  scale_fill_manual(name = "Species", 
                    #values = wes_palette("Darjeeling2", n = 5)) +
                    values = c("#ade292", "#0d5f8a", "#fd7f20","#905fd0")) +  
  facet_wrap(~type, nrow = 2) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "black"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

heat2 <- read.csv("heat2.csv")
hgraph2 <- ggplot(heat2, aes(x=species1, y=species2, fill=level)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = label), family="serif", size = 7) +
  #geom_abline()+
  scale_fill_manual(values = c("#e6b0aa",
                               "#cd6155",
                               "#a93226",
                               "#fbfcfc",
                               "#b2babb",
                               "#99a3a4",
                               "#7f8c8d",
                               "#707b7c")
  ) + 
  xlab("") +
  ylab("") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=16),
    legend.text = element_text(family="serif",size=16),
    axis.line = element_line(colour = "white"),
    axis.ticks = element_line(colour = "white"),
    axis.title= element_text(family="serif", size=16),
    axis.text = element_text(family="serif", size =16)
  ) 

fig3part <- ggarrange(speciesoverlap, hgraph2, widths= c(2, 1), ncol = 2,
                      labels = c("B-1", "B-2" ),
                      label.x = 0,
                      label.y = 1,
                      font.label = list(size = 16, color = "black", face = "bold", family = "serif"),
                      common.legend = FALSE)
fig3 <- ggarrange(habitatoverlap, fig3part, ncol = 1,
                  labels = c("A", "" ),
                  label.x = 0,
                  label.y = 1,
                  font.label = list(size = 16, color = "black", face = "bold", family = "serif"),
                  common.legend = FALSE)

ggsave("Figure 3-1r1.jpg",plot=fig3, 
       dpi=900, dev='jpg', height=28, width=48, units="cm")
