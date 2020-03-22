libs <- c("data.table", "dplyr")
lapply(libs, require, character.only = TRUE)

df <- fread('../data/County records.csv')
df_change <-  fread('../data/County records planting date change.csv')

order_ct <- unique(df$county)

#Build District models
order_dis <- unique(df$District)
dis_level <- list()
i <- 1
for (dis in order_dis){
  dis_level[i] <- list(df%>%filter(District==dis))
  i <- i+1
}

dis_stage <- lapply(dis_level,function(x) lm<- lm(de_Yield~GP1.KDD+GP1.GDD+GP1.PCPN+
                                                    GP2.KDD+GP2.GDD+GP2.PCPN+
                                                    GP3.KDD+GP3.GDD+GP3.PCPN+
                                                    GP4.KDD+GP4.GDD+GP4.PCPN ,data=x))

#Predict yield after olanting date changes
index <- match(df_change$District,order_dis)
changes <- c()
for (i in 1:dim(df_change)[1]){
  single <- predict(dis_stage[[index[i]]],df_change[i,])
  changes <- c(changes,single)
}
df_change$changes <- changes

#Analysis production changes

df_change_dcast <- dcast(df_change,Year+District+county+Tons_Ha+Harvested+de_Yield~manage,
                         value.var = 'changes')
df_change_dcast[,':='(early=(One_week_earlier-de_Yield)*Harvested,
                       late=(One_week_later-de_Yield)*Harvested,
                       Total_prod=Tons_Ha*Harvested)]

#Total production change rates for each district
total_rate <- df_change_dcast[,lapply(.SD, sum),
                              by=list(District),
                              .SDcol=c('Total_prod','early','late')]
total_rate[,':='(early_rate=early/Total_prod,
                 late_rate=late /Total_prod)]

#Annual variations on production changes over the Corn Belt
total_prod_year <- df_change_dcast[,lapply(.SD, sum),
                                   by=list(Year),
                                   .SDcol=c('Total_prod','early','late')]
total_prod_year[,':='(early_rate=early/Total_prod,
                      late_rate=late /Total_prod)]

# Production changes in three northern states
df_change_dcast[,c("State", "district") := tstrsplit(District, ",", fixed=TRUE)]
total_prod_year <- df_change_dcast[State %in% c('michigan', 'minnesota',  'wisconsin'),
                                   lapply(.SD, sum),by=list(Year),
                                   .SDcol=c('Total_prod','early','late')]
