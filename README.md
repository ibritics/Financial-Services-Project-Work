# Financial-Services-Project-Work
This was a university project work realized by me and Yahya Misbah during the Financial Instruments and Markets labs. 
2nd Assignment Financial services : The Procyclical Leverage Estimation
```

library(tidyverse)
#Real Estate-----
real_estate = read_csv("china_realestate_services.csv")

real_estate %>% is.na() %>% sum()

#This creates an index from row 1 to nrow (number of rows of the table) 
#every 5 rows. You can play with the starting point and the 5 to extract other sequences.
names = real_estate[seq(1,nrow(real_estate),5),1]
names = c(t(names))

total_asset = real_estate[seq(, nrow(real_estate),5),1 ]

#variables = c('total_asset',"Mtb","lvg")
#numbers = c("2","3","5")


#We are creating the function to select the values which we need and with assign()
#we are assigning them globaly.
list_function=function(x,y){
  assign(x, real_estate[seq(y, nrow(real_estate), 5),4:66 ], envir = .GlobalEnv)
  }

list_function("lvg","5")
list_function("total_asset","2")
list_function("Mtb","3")

#Creating the panel data names
panel_names = c()
for (i in 1:129){
  x = (rep(names[i],63))
  panel_names=append(panel_names,x)
  }
panel_names

#Creating total assets panel data 
total_asset_panel = c()
for (i in 1:129){
  x=total_asset[i,]
  x=t(x)
  total_asset_panel = append(total_asset_panel,x)
}
#Creating the leverage panel data
lvg_panel = c()
for (i in 1:129){
  x=lvg[i,]
  x=t(x)
  lvg_panel = append(lvg_panel,x)
}
#Creating the MTB panel
MTB_panel = c()
for (i in 1:129){
  x=Mtb[i,]
  x=t(x)
  MTB_panel = append(MTB_panel,x)
}

#We have 129 entities so we need the time period to be applied 129 times
time = colnames(real_estate[,4:66])


#I will need with time this code
time_panel = c()
count = 0
while (count != 129) {
  for (i in time){
    time_panel=append(time_panel,i)
  }
count = count + 1  
}
time_panel

real_estate_panel = cbind(panel_names,time_panel,total_asset_panel,MTB_panel,lvg_panel)

colnames(real_estate_panel)=c("Entity","Time","Total_Asset","MTB","Leverage")
write.csv(real_estate_panel,"china_real_estate.csv",row.names = FALSE)


#Remowing NA's 
#real_estate_panel= real_estate_panel[!is.na(real_estate_panel)]

#Banks----
banks = read_csv("Banks_China_original.csv")

#Getting names of entities
names = banks[seq(1,nrow(banks),5),1]
names = c(t(names))
names

#variables = c('total_asset',"Mtb","lvg")
#numbers = c("2","3","5")


#We are creating the function to select the values which we need and with assign()
#we are assigning them globaly.
list_function=function(x,y){
  assign(x, banks[seq(y, nrow(banks), 5),4:66 ], envir = .GlobalEnv)
}

list_function("lvg","5")
list_function("total_asset","2")
list_function("Mtb","3")

#Creating the panel data names
panel_names = c()
for (i in 1:13){
  x = (rep(names[i],63))
  panel_names=append(panel_names,x)
}
panel_names

#Creating total assets panel data 
total_asset_panel = c()
for (i in 1:13){
  x=total_asset[i,]
  x=t(x)
  total_asset_panel = append(total_asset_panel,x)
}
#Creating the leverage panel data
lvg_panel = c()
for (i in 1:13){
  x=lvg[i,]
  x=t(x)
  lvg_panel = append(lvg_panel,x)
}
#Creating the MTB panel
MTB_panel = c()
for (i in 1:13){
  x=Mtb[i,]
  x=t(x)
  MTB_panel = append(MTB_panel,x)
}

time = colnames(banks[,4:66])

#I will need with time this code
time_panel = c()
count = 0
while (count != 13) {
  for (i in time){
    time_panel=append(time_panel,i)
  }
  count = count + 1  
}
view(time_panel)


bank_panel = cbind(panel_names,time_panel,total_asset_panel,MTB_panel,lvg_panel)

colnames(bank_panel)=c("Entity","Time","Total_Asset","MTB","Leverage")
write.csv(real_estate_panel,"china_bank_panel.csv",row.names = FALSE)

#Financial Services----
financial = read_csv("Financial_services_Original.csv")

#Getting names of entities
names = financial[seq(1,nrow(financial),5),1]
names = c(t(names))
names

#variables = c('total_asset',"Mtb","lvg")
#numbers = c("2","3","5")


#We are creating the function to select the values which we need and with assign()
#we are assigning them globaly.
list_function=function(x,y){
  assign(x, financial[seq(y, nrow(financial), 5),4:66 ], envir = .GlobalEnv)
}

list_function("lvg","5")
list_function("total_asset","2")
list_function("Mtb","3")

#Creating the panel data names
panel_names = c()
for (i in 1:17){
  x = (rep(names[i],63))
  panel_names=append(panel_names,x)
}
panel_names

#Creating total assets panel data 
total_asset_panel = c()
for (i in 1:17){
  x=total_asset[i,]
  x=t(x)
  total_asset_panel = append(total_asset_panel,x)
}
#Creating the leverage panel data
lvg_panel = c()
for (i in 1:17){
  x=lvg[i,]
  x=t(x)
  lvg_panel = append(lvg_panel,x)
}
#Creating the MTB panel
MTB_panel = c()
for (i in 1:17){
  x=Mtb[i,]
  x=t(x)
  MTB_panel = append(MTB_panel,x)
}

time = colnames(financial[,4:66])

#I will need with time this code
time_panel = c()
count = 0
while (count != 17) {
  for (i in time){
    time_panel=append(time_panel,i)
  }
  count = count + 1  
}
view(time_panel)


financial_panel = cbind(panel_names,time_panel,total_asset_panel,MTB_panel,lvg_panel)

colnames(financial_panel)=c("Entity","Time","Total_Asset","MTB","Leverage")
write.csv(financial_panel,"china_financial_panel.csv",row.names = FALSE)

#Full_dataset
full_panel_data = rbind(bank_panel, financial_panel,real_estate_panel)

library(tidyr)
full_panel_data %>% is.na() %>% sum()
view(full_panel_data)

#adding ID
library(dplyr)
full_panel_data = as.tibble(full_panel_data)
full_panel_data = full_panel_data %>% group_by(Entity)  %>% mutate(ID=cur_group_id() %>% ungroup())

full_panel_data %>% is.na() %>% sum()
full_panel_data=na.omit(full_panel_data)

post_na_results = full_panel_data %>% group_by(Entity) %>% summarize(n=n()) %>% arrange(n)
view(post_na_results) 

#Selecting the observations which are below 80%. 63 (all times)*80%
missing = c("")
missing = post_na_results %>% filter(n<0.8*63) %>% select(Entity)

#DPlyr anti_join----
full_panel_data_no_na =full_panel_data %>% anti_join(missing)
full_panel_data_no_na %>% group_by(Entity) %>% summarize(n=n()) %>% arrange(n)

#Creating ID's ----
#full_panel_data_no_na <- transform(full_panel_data_no_na,
                      #ID = as.numeric(factor(Entity)))
full_panel_data_no_na %>%
  group_by(Entity) %>%
  arrange(Time) %>%
  mutate(diff = diff(Total_Asset)/lag(Total_Asset, default = first(Total_Asset)))

full_panel_data_no_na %>% 
  group_by(Entity) %>% 
  arrange(Time)

#Growth rate for loop----
full_panel_data_no_na
```
