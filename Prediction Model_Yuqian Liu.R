
library(XLConnect)
library(xlsx)
library(dplyr)
library(stringr)
library(bestglm)

setwd('D:/job/')
#load data
HDL<-read.csv("D:/job/HDL Data.csv",stringsAsFactors = F,strip.white =T)

#convert all column names to lower case
colnames(HDL)<-tolower(colnames(HDL))%>%gsub('[x]','',.)

colnames(HDL)[1]<-'area.name'

#standardlize colname names
HDL1<-HDL%>%select(hd.count=home.depot.store.count,lw.count=lowes.store.count,po2000=population.size.2000,income.2000,
                    under18.2000=..of.population.under.18.in.2000,college2000=..with.college.degree.2000,
                    owner2000=..home.ownership.2000,density_2000,
                    po2010=population.size.2010,income.2010,
                    under18.2010=..of.population.under.18.in.2010,college2010=..with.college.degree.2010,
                    owner2010=..home.ownership.2010,density_2010,area.name,
                   county.name,county.code,state,home.depot.region,lowes.region)
  
# HDL$..home.ownership.2000<-HDL$..home.ownership.2000/100
# HDL$..home.ownership.2010<-HDL$..home.ownership.2010/100

#remove na in HDL
HDL1[is.na(HDL1)]<-0

#remove 0 for all independent variables
row_sub <- apply(HDL1[3:14], 1, function(row) all(row !=0 ))
HDL1<-HDL1[row_sub,]

###########################set up polynomial regression model##########################

#look at histogram of hd_count and lw_count
hist(HDL1$hd.count)
hist(HDL1$lw.count)


pc2000<-c('under18.2000','college2000','owner2000')
pc2010<-c('under18.2010','college2010','owner2010')

#convert % to actual number
HDL_r<-HDL1%>%
  mutate_at(pc2000,funs((./100)*po2000))%>%
  mutate_at(pc2010,funs((./100)*po2010))

##############################regression model for home depot's data##########

#look at matrix of scatter plots for all variables for home depot dataset
#save MA's data for prediction
hd<-HDL_r%>%filter(state!='MA')%>%select(1,3:14)
pairs(hd)

#split orginal dataset to training set(80%) and testing set(20%)
train<-sample_frac(hd,0.8)
test<-sample_frac(hd,0.2)

#fit polynomial regression for hd
fit_train<-lm(hd.count~po2000+income.2000+under18.2000+college2000+density_2000+owner2000+po2010+income.2010+under18.2010+college2010+density_2010+owner2010,
              data=train)

summary(fit_train)

#get rid off independent variables by best GLMSELECT method
#use default select criteria BIC in this case

#remove variables from previous step
train1<-train%>%select(1:2,4,6:8,10,12:13)
bestglm(train1)

#fit new model
fit_train1<-lm(hd.count~owner2000+po2010+owner2010,
            data=train1)

summary(fit_train1)

#compute to prediction value
final_train<-train%>%mutate(residual=fit_train1$residuals,
                            pred_hd_count=round(hd.count-residual,0),
                            accuracy=ifelse(hd.count==pred_hd_count,1,0))

accuracy_rate<-final_train%>%group_by(accuracy)%>%summarise(n=n())

train_rate<-2071/2507

#write.xlsx(final_train,'hd_train.xlsx',sheetName = 'hd_train_pred')

#fit model with test data
fit_test<-lm(hd.count~owner2000+po2010+owner2010,
               data=test)

summary(fit_test)

final_hd_test<-test%>%mutate(residual=fit_test$residuals,
                            pred_hd_count=round(hd.count-residual,0),
                            accuracy=ifelse(hd.count==pred_hd_count,1,0))

hd_test_acc_rate<-final_hd_test%>%group_by(accuracy)%>%summarise(n=n())

hd_test_rate<-514/627

############################regression model for lowe's data############

#look at matrix of scatter plots for all variables for lowe's dataset
lw<-HDL_r%>%select(2:14)
pairs(lw)

#split orginal dataset to training set(80%) and testing set(20%)
lw_train<-sample_frac(lw,0.8)
lw_test<-sample_frac(lw,0.2)

#fit polynomial regression for lowe's
fit_lw<-lm(lw.count~po2000+income.2000+under18.2000+college2000+density_2000+owner2000+po2010+income.2010+under18.2010+college2010+density_2010+owner2010,
           data=lw_train)

summary(fit_lw)

#get rid off independent variables by best GLMSELECT method
#use default select criteria BIC in this case

#remove variables from previous step
lw_train1<-lw_train%>%select(1:4,6:13)

bestglm(lw_train1)

#fit new model
fit_lw1<-lm(lw.count~po2000+under18.2000+density_2000+under18.2010+owner2010,
            data=lw_train1)

summary(fit_lw1)

#compute prediction value
final_lw_train<-lw_train%>%mutate(residual=fit_lw1$residuals,
                            pred_lw_count=round(lw.count-residual,0),
                            accuracy=ifelse(lw.count==pred_lw_count,1,0))

lw_train_rate<-final_lw_train%>%group_by(accuracy)%>%summarise(n=n())

lw_train_acc_rate<-1946/2507

#fit model with test data
fit_lw_test<-lm(lw.count~po2000+income.2000+under18.2000+college2000+owner2000+density_2000+under18.2010+college2010,
             data=lw_test)

summary(fit_lw_test)

final_lw_test<-lw_test%>%mutate(residual=fit_lw_test$residuals,
                             pred_lw_count=round(lw.count-residual,0),
                             accuracy=ifelse(lw.count==pred_lw_count,1,0))

lw_test_acc_rate<-final_lw_test%>%group_by(accuracy)%>%summarise(n=n())

lw_test_rate<-478/627


## Underserved counties in MA
## Potential locations in MA

#for home depot
#fit model with test data
ma<-HDL_r%>%filter(state =='MA')
fit_hd_ma<-lm(hd.count~owner2000+po2010+owner2010,
              data=ma)

summary(fit_hd_ma)

#underserved results and prediction 
final_hd_ma<-ma%>%mutate(residual=fit_hd_ma$residuals,
                         pred_hd_count=round(hd.count-residual,0),
                         hd_underserved=ifelse(residual<0,1,0))

#for lowe's
#fit model with test data
fit_lw_ma<-lm(lw.count~po2000+income.2000+under18.2000+college2000+owner2000+density_2000+under18.2010+college2010,
              data=ma)

summary(fit_lw_ma)

#underserved results and prediction 
final_lw_ma<-ma%>%mutate(residual=fit_lw_ma$residuals,
                         pred_lw_count=round(lw.count-residual,0),
                         lw_underserved=ifelse(residual<0,1,0))


# write.xlsx(final_lw_ma,'test2.xlsx',row.names = F)


################################################################################
####################################################################################
##Visulization in map

library(urbnmapr)
library(ggplot2)

dev.off()
#import state shape file
ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = 'grey', color = 'white') +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


#add leading 0 to county code in orginal data

HDL_p<-HDL
HDL_p$county.code<- str_pad(HDL_p$county.code, 5, pad = "0")

HDL_p<-as.data.frame(HDL_p)%>%mutate(state.code=substr(county.code,0,2))

#plot original store count of two chains by state
po<-HDL_p%>%select(state,state.code,home.depot.store.count,lowes.store.count,population.size.2000)

state_po<-po%>%group_by(state,state.code)%>%summarise(hd_t=sum(home.depot.store.count),
                                                      lw_t=sum(lowes.store.count))

HDL_map<-merge(states,state_po,by.x='state_fips',by.y='state.code',all.y = T)

#merge HDL and state data
# HDL_map1<-merge(counties,HDL,by.x='county_fips',by.y='county.code',all.x = T)


# map for lowe's total store count in 51 states
HDL_map %>%
  ggplot(aes(long, lat, group = group, fill = lw_t)) +
  #ggplot(aes(long, lat, group = group, fill = income.2000))+
  #geom_polygon(color = NA) +
  geom_polygon(color = NA,size=0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Lowe's total store count")

# map for home depot's total store count in 51 states
HDL_map %>%
  ggplot(aes(long, lat, group = group, fill = hd_t)) +
  #ggplot(aes(long, lat, group = group, fill = income.2000))+
  #geom_polygon(color = NA) +
  geom_polygon(color = NA,size=0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Home Depot's total store count")

# write.csv(counties,'Counties.csv',row.names = F)
# write.csv(states,'States.csv',row.names = F)
