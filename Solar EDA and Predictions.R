############################## LOADING OF DATA SET##############################

solar <- readRDS('solar_dataset.RData')
station_info <- read.csv('station_info.csv')
addition_var <-  readRDS('additional_variables.RData')


#################################### EDA #######################################
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

solar <- as.data.table(solar)

##reformatting date column
solar$Date <- as.Date(solar$Date,format = '%Y%m%d')

##removing NA for EDA
solar_mod <- na.omit(solar)
solar_mod <- as.data.frame(solar_mod)

##removing PC columns for EDA
solar_mod <- solar_mod[,-(100:456)]


## there seems to be not much difference in output produced by all stations with difference between min and max being 0.2

max(sort(sapply(solar_mod[-1], function(x){100*(sum(x)/sum(solar_mod[,-1]))}),decreasing = T)) - min(sort(sapply(solar_mod[-1], function(x){100*(sum(x)/sum(solar_mod[,-1]))}),decreasing = T))


##arranging in descending order of energy produced
sort(sapply(solar_mod[-1], function(x){100*(sum(x)/sum(solar_mod[,-1]))}),decreasing = T)

## top 10 produce 11% of total energy
sum(sort(sapply(solar_mod[-1], function(x){100*(sum(x)/sum(solar_mod[,-1]))}),decreasing = T)[1:10])


##splitting the time in 2 parts to check consistency in ranking of stations
number_of_days = as.integer(difftime(max(solar_mod$Date) , min(solar_mod$Date), units = 'days'))
(number_of_days/365)/2


## focusing on the first 7 years
solar_mod_first <- solar_mod %>% filter(Date >= '1994-01-01' & Date <= '2001-01-01')

##top 10 producers
sort(sapply(solar_mod_first[,-1], function(x){round(100*(sum(x)/sum(solar_mod_first[,-1])),3)}),decreasing = T)[1:10]

## focusing on the latest 7 years
solar_mod_latest <- solar_mod %>% filter(Date > '2001-01-01')

##top 10 producers
sort(sapply(solar_mod_latest[,-1], function(x){round(100*(sum(x)/sum(solar_mod_latest[,-1])),3)}),decreasing = T)[1:10]

##observe a sudden jump in energy produced by station BEAV, BOIS over takes KENT, CHEY, ERIC and WOOD new entries
names(sort(sapply(solar_mod_first[,-1], function(x){100*(sum(x)/sum(solar_mod_first[,-1]))}),decreasing = T)[1:10])
names(sort(sapply(solar_mod_latest[,-1], function(x){100*(sum(x)/sum(solar_mod_latest[,-1]))}),decreasing = T)[1:10])

##adding column by year
solar_mod$by_year <- format(solar_mod$Date,'%Y')


##building comparison table by year
by_year_table <- 
  solar_mod %>% group_by(by_year) %>% summarise(BEAV=sum(BEAV),KENT=sum(KENT),BOIS= sum(BOIS),CHEY=sum(CHEY),ERIC=sum(ERIC)
                                                ,WOOD=sum(WOOD),SLAP=sum(SLAP),MANG=sum(MANG),ARNE=sum(ARNE),CAMA=sum(CAMA),
                                                HOOK = sum(HOOK),HOLL=sum(HOLL),GOOD=sum(GOOD),TIPT=sum(TIPT))


by_year_table

#################################### PLOTTING ##################################


##KENT overtaking BOIS (out preforming BOIS in the Years 2002 till 2005 especially 2003 by a big margin)  

ggplotly(ggplot(by_year_table) + geom_point(aes(y=KENT,x=by_year),col='blue',size=4) + geom_point(aes(y=BOIS,x=by_year),col='green',size=3) + xlab('Year') + ylab('Energy Production') + theme(axis.text.y = element_blank())+labs(title = 'ENERGY PRODUCTION (KENT IN BLUE, BOIS IN GREEN'))
plot(y=by_year_table$KENT,x=by_year_table$by_year,type='l',col='blue',main = 'KENT & BOIS',ylab = 'Energy Produced',xlab = 'Year') + lines(y=by_year_table$BOIS,x=by_year_table$by_year,type='l',col='green') + legend('topright',legend = c('KENT','BOIS'),col = c('blue','green'),lty = 2,cex = c(0.5,0.5),text.width = c(0.5,0.5))

##BEAV reaches top 3 in second half with significant growth in 2007
ggplot(by_year_table) + geom_point(aes(y=BEAV,x=by_year,col=BEAV),size=4) + geom_point(aes(y=HOOK,x=by_year)) + geom_point(aes(y=HOOK,x=by_year))+ geom_point(aes(y=HOLL,x=by_year)) +geom_point(aes(y=TIPT,x=by_year)) + geom_point(aes(y=GOOD,x=by_year)) + theme(axis.text.y = element_blank()) + xlab('Year') + ylab('Energy Produced') + labs(title = 'ENERGY PRODUCTION',caption = 'Other stations marked in black,BEAV sized for convenience')

##CHEY & SLAP
plot(y=by_year_table$CHEY,x=by_year_table$by_year,type='l',col='blue',main = 'CHEY & SLAP',ylab = 'Energy Produced',xlab = 'Year',ylim = c(5.5e+09,6.8e+09)) + lines(y=by_year_table$SLAP,x=by_year_table$by_year,type='l',col='green') + legend('bottomright',legend = c('CHEY','SLAP'),col = c('blue','green'),lty = 2,cex = c(0.5,0.5),text.width = c(0.5,0.5))



##looking at elevation and relating it to ranking of stations
## it comes as no surprise, there seems to be correlation between elevation of the station and total energy produced
station_info <- as.data.table(station_info)
station_info[order(station_info$elev,decreasing = T)][,c(1,4)][1:20]

library(leaflet)
##leaflet location
BOIS_LOC <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-102.4971, lat=36.69256, popup="BOIS")

KENT_LOC <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-102.8782, lat=36.82937, popup="KENT")



BOIS_LOC
KENT_LOC

####################### MERGING ADDITIONAL VARIABLES ##########################

addition_var <- as.data.frame(addition_var)

class(addition_var)

addition_var$Date <- as.Date(addition_var$Date,format = '%Y%m%d')

class(addition_var$Date)

##median across columns observed to be 0
sapply(addition_var[,-1], function(x){median(x,na.rm = T)})

##replacing NULL values by median value
addition_var[is.na(addition_var)] <- 0

addition_var <- as.data.table(addition_var)


addition_var_mod <- addition_var[1:5113]

solar_mod_addition <- merge(solar_mod,addition_var_mod)

## checking for correlation
library(corrgram)
library(corrplot)

## converting columns to numeric
solar_mod_addition[,-1] <- sapply(solar_mod_addition[,-1],as.numeric)
solar_mod_addition <- as.data.frame(solar_mod_addition)

##correlation - we observe no or very little correlation of additional variables with the energy produced by the stations
## this is observed taking 3 random selection
correlation_1 <- solar_mod_addition[1:30,c(2:10,135:145)]
correlation_2 <- solar_mod_addition[1:30,c(20:30,170:180)]
correlation_3 <- solar_mod_addition[1:30,c(30:40,150:160)]

corrplot(cor(correlation_1),method = 'color')
corrplot(cor(correlation_2),method = 'color')
corrplot(cor(correlation_3),method = 'color')

##outlier
ggplotly(ggplot(Total_Average_Year, aes(Year,value)) + geom_boxplot(fill='blue') + theme(axis.text.y = element_blank()))


#########################  CLEANING AND MODIFYING DATA SET 2 ###################

energy <- as.data.table(readRDS("/Users/Sumant Duggal/Downloads/project(1) (1)/solar_dataset.RData"))
energy$Date <- as.POSIXct(x = energy$Date,format = "%Y%m%d")
energy <- energy[,!(100:456)]
energy$Year <- as.integer(format(energy$Date, format="%Y"))
energy$Month <- factor(format(energy$Date, format="%m"))
energy_train <- energy[1:5113,]
energy_predict  <- energy[5114:6909,]
## average energy production by month 

avg_energy_train_month <- aggregate(energy_train[, 2:99], list(energy_train$Month), mean)

avg_energy_train_month$Month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
avg_energy_train_month$Group.1 <- NULL
Total_Average_Year <- melt(data.table("Year" = avg_energy_train_year$Group.1,avg_energy_train_year[,-1],"Total_Average" = rowMeans(avg_energy_train_year)),  
                           id.vars = 'Year', 
                           variable.name = 'series')

############################ SVM ###################################
##################### LOADING AND MODIFYING THE DATA ##############################

solar <- readRDS('solar_dataset.RData')
solar <- as.data.table(solar)

##reformatting date column
solar$Date <- as.Date(solar$Date,format = '%Y%m%d')

##removing NA for EDA
solar_svm <- na.omit(solar)

##removing PC columns for EDA
solar_svm <- solar_svm[,-(100:456)]

##creating blocks based on years
solar_2001 <- solar_svm[(solar_svm$Date > '2000-12-31' & solar_svm$Date <= '2001-12-31'),]
solar_2004_2007 <- solar_svm[(solar_svm$Date > '2003-12-31' & solar_svm$Date <= '2007-12-31'),]

##preparing test data set
solar_test <- rbind(solar_2001,solar_2004_2007)
solar_test <- solar_test[1:(nrow(solar_test)-30)]

##prediction date columns
solar_test_date = solar[is.na(KENT)== T][,1]

##################### RUNNING THE MODEL #######################################

## tune function to find optimized cost and gamma value
tune(svm, train.x,train.y,kernel='radical',)

##checking for errors produced using the defined hyper parameters, epsilon value found to be optimised at 0.01 through trial and error
comp <- data.table()
for(iterator in colnames((solar_svm)[,2:10])){
  
  # train SVM model with a particular set of hyper parameters
  model <- svm(as.formula(paste(iterator,"~ .")), data = solar_svm[,-1], kernel="radial",
               cost = 10,epsilon = 0.01,gamma = 0.5);
  
  # Get model predictions
  predictions_train <- predict(model, newdata = solar_svm[,-1]);
  predictions_test <- predict(model, newdata = solar_test);
  
  # Get errors
  errors_train <- predictions_train - as.integer(data.matrix(solar_svm[,..iterator]));
  errors_test <- predictions_test - as.integer(data.matrix(solar_test[,..iterator]));
  
  # Compute Metrics
  mse_train <- round(mean(errors_train^2), 2);
  mae_train <- round(mean(abs(errors_train)), 2);
  
  mse_test <- round(mean(errors_test^2), 2);
  mae_test <- round(mean(abs(errors_test)), 2);
  
  # Check results
  comp <- rbind(comp,data.table(col_name=iterator, mse_train = mse_train, mae_train = mae_train,
                                mse_test = mse_test, mae_test = mae_test));
}

##predicting future solar energy values
result <- data.table(Date=solar_test_date$Date)
for(iterator in colnames((solar_svm)[,2:ncol(solar_svm)])){
  
  # train SVM model with a particular set of hyper parameters
  model <- svm(as.formula(paste(iterator,"~ .")), data = solar_svm[,-1], kernel="radial",
               cost = 10,epsilon = 0.01,gamma = 0.5);
  
  # Get model predictions
  predictions_test <- predict(model, newdata = solar_test);
  
  
  # Check results
  result <- cbind(result,(data.table(iterator=predictions_test)));
  
  ## to check where the loop as reached
  print(sprintf('%s is done',iterator))
}

result

############### Neural Network ################

library(data.table)
solar <- readRDS('solar_dataset.RData')
solar <- as.data.table(solar)

##reformatting date column
solar$Date <- as.Date(solar$Date,format = '%Y%m%d')

##removing NA for EDA
solar_nn <- na.omit(solar)

##removing PC columns for EDA
solar_nn <- solar_nn[,-(100:456)]

##creating blocks based on years
solar_2001 <- solar_nn[(solar_nn$Date > '2000-12-31' & solar_nn$Date <= '2001-12-31'),]
solar_2004_2007 <- solar_nn[(solar_nn$Date > '2003-12-31' & solar_nn$Date <= '2007-12-31'),]

##preparing test data set

solar_test <- rbind(solar_2001,solar_2004_2007)
solar_test <- solar_test[1:(nrow(solar_test)-30)]

##preparing train data set
solar_nn$by_year <- format(solar_nn$Date,format = '%Y')
solar_train <- solar_nn[by_year %in% c(1994:2003)]


##normalising data - train
maxs <- apply(solar_train[,-c(1,100)],2,max)
mins <- apply(solar_train[,-c(1,100)],2,min)
scaled.data_train <- scale(solar_train[,-c(1,100)],center=mins,scale=maxs-mins)
scaled_train <- as.data.frame(scaled.data_train)

##normalising data - train
maxs <- apply(solar_test[,-1],2,max)
mins <- apply(solar_test[,-1],2,min)
scaled.data_test <- scale(solar_test[,-1],center=mins,scale=maxs-mins)
scaled_test <- as.data.frame(scaled.data_test)

install.packages('neuralnet')
library(neuralnet)
n <- names(scaled_train)
f <-as.formula(paste('KENT ~',paste(n[!n %in% 'KENT'],collapse = '+')))
nn <- neuralnet(f,scaled_train,hidden=3,linear.output = T)
plot(nn)

predicted.nn.values <- compute(nn,scaled_test[,-(which(colnames(scaled_test)=='KENT'))])
true.predictions <- predicted.nn.values$net.result * (max(solar_nn$KENT)-min(solar_nn$KENT))+min(solar_nn$KENT)
data.frame(KENT = true.predictions)
