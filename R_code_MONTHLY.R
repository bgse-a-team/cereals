cereals <- read.csv("./cereals.csv")

# Sells per company
table(as.factor(cereals$firm_id))
plot(as.factor(cereals$firm_id))

# Sells per store
table(as.factor(cereals$store))
plot(as.factor(cereals$store))

# When do companies sell their products
plot(as.factor(cereals$firm_id),cereals$mon)

# Sells per store and and firm_id (given the company, the sells per store are exactly the same)
plot(as.factor(cereals$store),as.factor(cereals$firm_id),ylab="Company",xlab="store")

# Sells of each product
plot(as.factor(cereals$nitem))
# Sells of each product (on Halloween)
plot(as.factor(cereals$nitem[cereals$halloween == 1]))

# Demand model for all manufacturers
plot(cereals$quant_mon,cereals$price_mon,cex=0.3)
abline(lm(price_mon ~ quant_mon, data=cereals),col="red")
plot(log(cereals$quant_mon),log(cereals$price_mon),cex=0.3)
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[-which(cereals[,"quant_mon"] == 0),]),col="red")

# Manufacturers
levels(as.factor(cereals$manufacturername))

# Percentage content of corn, wheat, rice, oat and barley per manufacturer
exp <- rbind(tapply(cereals$cornweight,as.factor(cereals$manufacturername),mean),
             tapply(cereals$wheatweight,as.factor(cereals$manufacturername),mean),
             tapply(cereals$riceweight,as.factor(cereals$manufacturername),mean),
             tapply(cereals$oatweight,as.factor(cereals$manufacturername),mean),
             tapply(cereals$barleyweight,as.factor(cereals$manufacturername),mean))
rownames(exp) <- c("% Corn","% Wheat","% Rice","% Oat","% Barley")
round(exp,2)

# Mean nutritional information per manufacturer
exp <- rbind(tapply(cereals$caloriesserving,as.factor(cereals$manufacturername),mean),
             tapply(cereals$totalfatg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$saturatedfatg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$sodiummg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$totalcarbohydratesg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$dietaryfiberg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$sugarsg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$proteing,as.factor(cereals$manufacturername),mean),
             tapply(cereals$vitaminamg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$ironmg,as.factor(cereals$manufacturername),mean),
             tapply(cereals$vitaminciu,as.factor(cereals$manufacturername),mean),
             tapply(cereals$calciummg,as.factor(cereals$manufacturername),mean))
rownames(exp) <- c("Calories","Total fat","Saturated fat","Sodium","Carbohydrates","Fiber","Sugar","Protein",
                   "Vitamin A","Iron","Vitamin C","Calcium")
round(exp,2)

# Descriptive
tapply(cereals$sale_c_mon,as.factor(cereals$manufacturername),mean)
tapply(cereals$adultall,as.factor(cereals$manufacturername),mean)

# Evolution consumption fat over time
plot(tapply(cereals$totalfatg*cereals$nsize,as.factor(cereals$month),mean)/tapply(cereals$nsize,as.factor(cereals$month),mean))

# Evolution prices over time (inflation??)
plot(cereals$month,cereals$price_mon,cex=0.3,pch=20)
abline(lm(cereals$price_mon ~ cereals$month),col="red")

# Demand model for each manufactuer (red) compared to all manufacturers (blue)
par(mfrow=c(4,2))
cereals <- cereals[-which(cereals[,"quant_mon"] == 0),]
# Dominicks
idx <- cereals[,"manufacturername"] == "Dominicks"
plot(log(cereals$quant_mon[idx]),log(cereals$price_mon[idx]),cex=0.3,main="Dominicks")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[idx,]),col="red")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals),col="blue")
# General Mills
idx <- cereals[,"manufacturername"] == "General Mills"
plot(log(cereals$quant_mon[idx]),log(cereals$price_mon[idx]),cex=0.3,main="General Mills")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[idx,]),col="red")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals),col="blue")
# Kelloggs
idx <- cereals[,"manufacturername"] == "Kelloggs"
plot(log(cereals$quant_mon[idx]),log(cereals$price_mon[idx]),cex=0.3,main="Kelloggs")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[idx,]),col="red")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals),col="blue")
# Kraft
idx <- cereals[,"manufacturername"] == "Kraft"
plot(log(cereals$quant_mon[idx]),log(cereals$price_mon[idx]),cex=0.3,main="Kraft")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[idx,]),col="red")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals),col="blue")
# Nabisco / Kraft
idx <- cereals[,"manufacturername"] == "Nabisco / Kraft"
plot(log(cereals$quant_mon[idx]),log(cereals$price_mon[idx]),cex=0.3,main="Nabisco / Kraft")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[idx,]),col="red")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals),col="blue")
# Quaker Oats
idx <- cereals[,"manufacturername"] == "Quaker Oats"
plot(log(cereals$quant_mon[idx]),log(cereals$price_mon[idx]),cex=0.3,main="Quaker Oats")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[idx,]),col="red")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals),col="blue")
# Ralston
idx <- cereals[,"manufacturername"] == "Ralston"
plot(log(cereals$quant_mon[idx]),log(cereals$price_mon[idx]),cex=0.3,main="Ralston")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals[idx,]),col="red")
abline(lm(log(price_mon) ~ log(quant_mon), data=cereals),col="blue")

# Demand models using cross-price elasticities

  ## Variables that are correlated with price
  # distance_factory (different value for each manufacturer). It never changes, right? So not necessary
  # retailprofit_mon. Does profit affect prices? cor(cereals$retailprofit_mon,cereals$price_mon,use="complete")
  # cereals$electricityprice_midwest (electricity price) plot(cereals$month,cereals$electricityprice_midwest)
  # retailprofperquant_mon. cor(cereals$retailprofperquant_mon,cereals$price_mon,use="complete")
  # distance_gasoline
  # barley_price_us, wheat_price_us, rice_price_us, corn_price_us, oats_price_us
  cereals$cost_inputs <- apply(cereals[,c("barley_g_price","sugar_g_price","wheat_g_price","rice_g_price","corn_g_price","oat_g_price")],1,sum,na.rm=T)
  IV_price <- lm(price_mon ~ distance_gasoline + cost_inputs + earnings_tradetransport + retailprofperquant_mon + price_instru_zone, data = cereals)

  ## Use price per onze
  cereals$price_oz <- cereals$price_mon/cereals$nsize
  
  ## Mean price per month and company
  tmp <- tapply(cereals$price_oz,list(cereals$month,cereals$manufacturername),mean,na.rm=T)
  
  ## Mean quantity per month and company
  tmp2 <- tapply(cereals$quant_mon,list(cereals$month,cereals$manufacturername),mean,na.rm=T)
  
  ## Demand data frame
  colnames(tmp2) <- paste0(colnames(tmp2),"_qty")
  colnames(tmp) <- paste0(colnames(tmp),"_price")
  demand_data <- cbind(tmp,tmp2)
  demand_data <- as.data.frame(demand_data)
  colnames(demand_data) <- make.names(colnames(demand_data), unique=TRUE)

  # Demand curve
  ## Income per month
  demand_data$income_mon <- tapply(cereals$income,cereals$month,mean,na.rm=T) # It is constant
  
  ## Calories
  model_Dominicks <- lm(log(Dominicks_price)~log(Dominicks_qty)+log(Kelloggs_price)+log(General.Mills_price)+log(Kraft_price)+log(Nabisco...Kraft_price)+log(Quaker.Oats_price) + log(Ralston_price),data=demand_data)
  summary(model_Dominicks)
  model_Dominicks <- lm(log(General.Mills_price)~log(General.Mills_qty)+log(Kelloggs_price)+log(Dominicks_price)+log(Kraft_price)+log(Nabisco...Kraft_price)+log(Quaker.Oats_price) + log(Ralston_price),data=demand_data)
  summary(model_Dominicks)
  model_Kelloggs <- lm(log(Kelloggs_price)~log(Kelloggs_qty)+log(Dominicks_price)+log(General.Mills_price)+log(Kraft_price)+log(Nabisco...Kraft_price)+log(Quaker.Oats_price) + log(Ralston_price),data=demand_data)
  summary(model_Kelloggs)
  model_Kraft <- lm(log(Kraft_price)~log(Kraft_qty)+log(Dominicks_price)+log(General.Mills_price)+log(Kelloggs_price)+log(Nabisco...Kraft_price)+log(Quaker.Oats_price) + log(Ralston_price),data=demand_data)
  summary(model_Kraft)
  model_Nabisco...Kraft <- lm(log(Nabisco...Kraft_price)~log(Nabisco...Kraft_qty)+log(Dominicks_price)+log(General.Mills_price)+log(Kelloggs_price)+log(Kraft_price)+log(Quaker.Oats_price) + log(Ralston_price),data=demand_data)
  summary(model_Nabisco...Kraft)
  model_Quaker.Oats <- lm(log(Quaker.Oats_price)~log(Quaker.Oats_qty)+log(Dominicks_price)+log(General.Mills_price)+log(Kelloggs_price)+log(Kraft_price)+log(Nabisco...Kraft_price) + log(Ralston_price),data=demand_data)
  summary(model_Quaker.Oats)
  model_Ralston <- lm(log(Ralston_price)~log(Ralston_qty)+log(Dominicks_price)+log(General.Mills_price)+log(Kelloggs_price)+log(Kraft_price)+log(Nabisco...Kraft_price) + log(Quaker.Oats_price),data=demand_data)
  summary(model_Ralston)
  
  
