################### CREEK project #################
#########data collected in Glacier Creek Preserve in 2018-2020##############
#########diel activity of bats at a fine scale ##################
install.packages('overlap')

library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(suncalc)
library(lubridate)
library(overlap)
library(dplyr)

##############################################################
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


##############################buzz##########################
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


df<- read.csv("buzz10.csv")
df$nightcent_f <- as.factor(df$nightcent_f)
df$hour_f <- as.factor(df$hour_f)

df1 <- aggregate(df$buzz, by=list(df$date, df$NextDirUp, df$type, df$SppAccp, df$hour_f, df$buzz), FUN = length)
names(df1)[1] <- "date"
names(df1)[2] <- "site"
names(df1)[3] <- "type"
names(df1)[4] <- "species"
names(df1)[5] <- "hour"
names(df1)[6] <- "buzz"
names(df1)[7] <- "count"

ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=hour, y=count, fill=buzz)) +
  geom_boxplot()
ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=hour, y=log(count), fill=buzz)) +
  geom_boxplot()
ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=hour, y=log(count), fill=type)) +
  geom_boxplot() +
  facet_wrap(~buzz, scales = "free")
ggplot(data=subset(df1, df1$species=="Epfu"), aes(x=hour, y=buzz, fill=count)) +
  geom_tile()

df2 <- spread(df1, hour, count)
df2[is.na(df2)] <- 0


df11 <- aggregate(df$buzz, by=list(df$date, df$NextDirUp, df$type, df$SppAccp, df$hour_f), FUN = length)
names(df11)[1] <- "date"
names(df11)[2] <- "site"
names(df11)[3] <- "type"
names(df11)[4] <- "species"
names(df11)[5] <- "hour"
names(df11)[6] <- "count"
df22 <- spread(df11, hour, count)
df22[is.na(df22)] <- 0
df22$buzz <- "total"

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


################## test overlap package##########
df2 <- read.csv("buzz2.csv")
df2 <- subset(df2, df2$nightcent<1)
df2$timerad <- df2$nightcent *2 *pi
df2$year <- as.factor(df2$year)

df10 <- df2 %>%
  group_by(SppAccp, date, type, year, NextDirUp) %>%
  summarize(count = length(buzz))

df11 <- spread(df10, SppAccp, count)
df11[is.na(df11)] <- 0

###### preserve level#####
###species A vs species B###
overlapPlot(df2[(df2$SppAccp=="Epfu"),]$timerad,
            df2[(df2$SppAccp=="Labo"),]$timerad)
overlapEst(df2[(df2$SppAccp=="Epfu"),]$timerad,
           df2[(df2$SppAccp=="Labo"),]$timerad, 
           type="Dhat1")

#dflabo <- df2[df2$SppAccp=="Labo",]
overlapPlot(df2[(df2$SppAccp=="Epfu"&df2$year=="2018"),]$timerad,
            df2[(df2$SppAccp=="Epfu"&df2$year=="2019"),]$timerad)
densityPlot(df2[df2$SppAccp=="Epfu",]$timerad)
overlapPlot(df2[(df2$SppAccp=="Epfu"&df2$type=="Open"),]$timerad,
            df2[(df2$SppAccp=="Epfu"&df2$type=="Edge"),]$timerad)
overlapPlot(df2[(df2$SppAccp=="Epfu"&df2$year=="2018"),]$timerad,
            df2[(df2$SppAccp=="Epfu"&df2$year=="2019"),]$timerad)
overlapPlot(df2[(df2$SppAccp=="Epfu"&df2$year=="2018"&df2$type=="Edge"),]$timerad,
            df2[(df2$SppAccp=="Epfu"&df2$year=="2019"&df2$type=="Edge"),]$timerad)
overlapPlot(df2[(df2$SppAccp=="Epfu"&df2$year=="2018"&df2$type=="Open"),]$timerad,
            df2[(df2$SppAccp=="Epfu"&df2$year=="2019"&df2$type=="Open"),]$timerad)


overlapEst(df2[(df2$SppAccp=="Epfu"&df2$type=="Open"),]$timerad,
                  df2[(df2$SppAccp=="Epfu"&df2$type=="Edge"),]$timerad, 
                  type="Dhat1")
overlapEst(df2[(df2$SppAccp=="Epfu"&df2$year=="2018"),]$timerad,
           df2[(df2$SppAccp=="Epfu"&df2$year=="2019"),]$timerad, 
           type="Dhat1")
overlapEst(df2[(df2$SppAccp=="Epfu"&df2$year=="2018"&df2$type=="Open"),]$timerad,
           df2[(df2$SppAccp=="Epfu"&df2$year=="2019"&df2$type=="Open"),]$timerad, 
           type="Dhat1")
overlapEst(df2[(df2$SppAccp=="Epfu"&df2$year=="2018"&df2$type=="Edge"),]$timerad,
           df2[(df2$SppAccp=="Epfu"&df2$year=="2019"&df2$type=="Edge"),]$timerad, 
           type="Dhat1")

densityPlot(df2[df2$SppAccp=="Labo",]$timerad)
overlapPlot(df2[(df2$SppAccp=="Labo"&df2$type=="Open"),]$timerad,
            df2[(df2$SppAccp=="Labo"&df2$type=="Edge"),]$timerad)
overlapEst(df2[(df2$SppAccp=="Labo"&df2$type=="Open"),]$timerad,
           df2[(df2$SppAccp=="Labo"&df2$type=="Edge"),]$timerad, 
           type="Dhat1")

dfplot1 <- subset(df2, df2$nightcent<1 & df2$SppAccp=="Labo")
dfplot1$timerad <- dfplot1$nightcent * 2 * pi
laboden <- densityPlot(dfplot1$timerad)

dfplot2 <- subset(df2, df2$nightcent<1 & df2$SppAccp=="Lano")
dfplot2$timerad <- dfplot2$nightcent * 2 * pi
densityPlot(dfplot2$timerad)

dfplot3 <- subset(df2, df2$nightcent<1 & df2$SppAccp=="Laci")
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

