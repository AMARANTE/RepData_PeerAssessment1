##Reproducible Resoruce
##Semana-02
##Geraldo Barbosa do Amarante
##Semana-01 - NOVEMBRO/2017
#-------------------------------------
##Definição da pasta de trabalho
setwd("D:/coursera/ReproducibleResource/Semana02/ArquivodeDados")

##Instalação dos pacotes
install.packages("knitr")
##Leitura das bibliotecas
library(readr)
library(knitr)
library(dplyr)
library(ggplot2)
##library(dplyr)
#library(tidyr)
#library(readxl)
#library(lubridate)
##---------------------------------------
## 1-LOADING AND PROCESSING THE DATA  ---
##---------------------------------------
# 1.1 - Load the data. ---
#-----------------------
if(file.exists("./activity.csv"))
  atividade <- read_csv("./activity.csv")
##Verificando informações
atividade
head(atividade)
##--------------------------------------------------------------------------------------------
# 1.2 - Process/transform the data (if necessary) into a format suitable for your analysis ---
#---------------------------------------------------------------------------------------------
# Remover as informações NA
atividadesemNA <- atividade[ with (atividade, { !(is.na(steps)) } ), ]
##Verificando o arquivo
atividadesemNA
head(atividadesemNA)
##---------------------------------------------------------
## 2 - WHAT IS MEAN TOTALNUMBER OF STEPS TAKEN PER DAY ---
##---------------------------------------------------------
# 2.1 - Calculate the total number of steps taken per day
#----------------------------------------------------------------------------------------------------------
# 2.2 - If you do not understand the difference between a histogram and a barplot,
#       research the difference between them. Make a histogram of the total number of steps taken each day
#---------------------------------------------------------------------------------------------------------
# 2.3 - Calculate and report the mean and median of the total number of steps taken per day
#---------------------------------------------------------------------------------------------------------
agrupamentopordata <- group_by(atividadesemNA, date)
passosdiarios <- summarise(agrupamentopordata, total = sum(steps))
##Verificando informações
agrupamentopordata
head(agrupamentopordata)
passosdiarios
head(passosdiarios)
##----------------------------------------------------------------
hist(passosdiarios$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day",col = "yellow")
summary(passosdiarios)
#-----------------------------------------------------
## 3 - WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN? ---
##--------------------------------------------------------------------------------------------------------
# 3.1 - Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number 
#       of steps taken, averaged across all days (y-axis)
#-----------------------------------------------------------------------------------------------------------
# 3.2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number 
#       of steps?
#-----------------------------------------------------------------------------------------------------------
passosporintervalo <- aggregate(steps ~ interval, atividadesemNA, mean)
#----
plot(passosporintervalo$interval, passosporintervalo$steps, type='l', 
     main="THE AVERAGE DAILY ACTIVITY PATTERN", xlab="5-minute interval", 
     ylab="Averaged across all days",col="green")
#----
valoresmaximos <- which.max(passosporintervalo$steps)
passosporintervalo[valoresmaximos, ]
##---------------------------------
## 4 - IMPUTING MISSING VALUES  ---
##--------------------------------------------------------------------------------------------------------
# 4.1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of 
#       rows with NAs)
#---------------------------------------------------------------------------------------------------------
# 4.2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not 
#       need to be sophisticated. For example, you could use the mean/median for that day, or the mean for
#       that 5-minute interval, etc.
#---------------------------------------------------------------------------------------------------------
# 4.3 - Create a new dataset that is equal to the original dataset but with the missing data filled in.
#-----------------------------------------------------------------------------------------------------------
# 4.4 - Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
#       median total number of steps taken per day. Do these values differ from the estimates from the first
#       part of the assignment? What is the impact of imputing missing data on the estimates of the total 
#       daily number of steps?
#-----------------------------------------------------------------------------------------------------------
somaatividade <- sum(is.na(atividade))
#--
for (i in 1:nrow(atividade)){
  if (is.na(atividade$steps[i])){
    valorintervalo <- atividade$interval[i]
    registro <- which(passosporintervalo$interval == valorintervalo)
    valorpasso <- passosporintervalo$steps[registro]
    atividade$steps[i] <- valorpasso
  }
}
##Verificando informações
somaatividade
valorintervalo
registro
valorpasso
#---------------------------------------------------------------------
atividadeagregada <- aggregate(steps ~ date, atividade, sum)
atividadeagregadasemNA <- aggregate(steps ~ date, atividadesemNA, sum)
#--
hist(atividadeagregada$steps,main="Total number of steps per day", 
     xlab="Total steps in a day", col = "blue")
#----
# mean e median
mean(atividadeagregada$steps)
#----
median(atividadeagregada$steps)
#----
summary(atividadeagregada)

#--------------------------------------------------------------------------------------------------
## 5 - ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS? --
#--------------------------------------------------------------------------------------------------
# 5.1 - Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#       indicating whether a given date is a weekday or weekend day.
#--------------------------------------------------------------------------------------------------
# 5.2 - Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval
#       (x-axis) and the average number of steps taken, averaged across all weekday days or weekend
#       days (y-axis). See the README file in the GitHub repository to see an example of what this 
#       plot should look like using simulated data.
#--------------------------------------------------------------------------------------------------
atividade['type_of_day'] <- weekdays(as.Date(atividade$date))
atividade$type_of_day[atividade$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
atividade$type_of_day[atividade$type_of_day != "weekend"] <- "weekday"
atividade$type_of_day <- as.factor(atividade$type_of_day)

# calculate average steps by interval across all days
passosporintervalo <- aggregate(steps ~ interval + type_of_day, atividade, mean)

# creat a plot
qplot(interval, 
      steps, 
      data = passosporintervalo, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)



































