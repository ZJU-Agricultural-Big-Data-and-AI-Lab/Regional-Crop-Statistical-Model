libs <- c("data.table", "dplyr")
lapply(libs, require, character.only = TRUE)

df <- fread('../data/County records.csv')

order_ct <- unique(df$county)

all_county <- list()
for(ct in order_ct){
  z <- df%>%filter(county==ct)
  eval(parse(text = paste("all_county$`",ct,"` <- z",sep='')))
}

trends <- lapply(all_county,function(x) lm<- lm(Tons_Ha~Year, data=x))

trends_coef <- data.frame()
for (i in 1:length(trends)) {
  element <- summary(trends[[i]])$coefficients%>%as.data.table(keep.rownames = TRUE)
  element$county <- names(trends[i])
  trends_coef <- rbind(trends_coef,element)
}

#Summarize the results
summary(trends_coef[rn=='Year' & `Pr(>|t|)`<0.05])
sd(trends_coef[rn=='Year' & `Pr(>|t|)`<0.05]$Estimate)
trends_coef[rn=='Year' & `Pr(>|t|)`<0.05 & Estimate>0]
