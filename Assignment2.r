
library(tidyverse)
real_estate = read_csv("china_realestate_services.csv")

real_estate %>% is.na() %>% sum()

#This creates an index from row 1 to nrow (number of rows of the table) 
#every 5 rows. You can play with the starting point and the 5 to extract other sequences.
names = real_estate[seq(1,nrow(real_estate),5),1]
names = c(t(names))

total_asset = real_estate[seq(, nrow(real_estate),5),1 ]

variables = c('total_asset',"Mtb","lvg")
numbers = c("2","3","5")


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

time = colnames(real_estate[,4:66])

#I will need with time this code
time_panel = c()
count = 0
while (count != 63) {
  for (i in time){
    time_panel=append(time_panel,i)
  }
count = count + 1  
}
time_panel

real_estate_panel = cbind(panel_names,time_panel,total_asset_panel,MTB_panel,lvg_panel)

colnames(real_estate_panel)=c("Entity","Time","Total_Asset","MTB","Leverage")
write.csv(real_estate_panel,"china_real_estate.csv",row.names = FALSE)
