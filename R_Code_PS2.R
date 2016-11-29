cereals <- read.csv("./cereals.csv")
sapply(cereals,class)

# Sells per company
table(as.factor(cereals$firm_id))
plot(as.factor(cereals$firm_id))

# Sells per store
table(as.factor(cereals$store))
plot(as.factor(cereals$store))

# When do companies sell their products
plot(as.factor(cereals$firm_id),cereals$week)

# Sells per store and and firm_id (given the company, the sells per store are exactly the same)
plot(as.factor(cereals$store),as.factor(cereals$firm_id),ylab="Company",xlab="store")

# Sells of each product
plot(as.factor(cereals$nitem))
# Sells of each product (on Halloween)
plot(as.factor(cereals$nitem[cereals$halloween == 1]))

# Demand model for all manufacturers
plot(cereals$quant_weekly,cereals$price_weekly,cex=0.3)
abline(lm(price_weekly ~ quant_weekly, data=cereals),col="red")
plot(log(cereals$quant_weekly),log(cereals$price_weekly),cex=0.3)
abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="red")

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

# Evolution consumption fat over time
plot(tapply(cereals$totalfatg*cereals$nsize,as.factor(cereals$week),mean)/tapply(cereals$nsize,as.factor(cereals$week),mean))

# Evolution prices over time (inflation??)
plot(cereals$week,cereals$price_weekly,cex=0.3,pch=20)
abline(lm(cereals$price_weekly ~ cereals$week),col="red")

# Demand model for each manufactuer (red) compared to all manufacturers (blue)
    par(mfrow=c(4,2))
    # Dominicks
    idx <- cereals[,"manufacturername"] == "Dominicks"
    plot(log(cereals$quant_weekly[idx]),log(cereals$price_weekly[idx]),cex=0.3,main="Dominicks")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals[idx,]),col="red")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="blue")
    # General Mills
    idx <- cereals[,"manufacturername"] == "General Mills"
    plot(log(cereals$quant_weekly[idx]),log(cereals$price_weekly[idx]),cex=0.3,main="General Mills")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals[idx,]),col="red")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="blue")
    # Kelloggs
    idx <- cereals[,"manufacturername"] == "Kelloggs"
    plot(log(cereals$quant_weekly[idx]),log(cereals$price_weekly[idx]),cex=0.3,main="Kelloggs")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals[idx,]),col="red")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="blue")
    # Kraft
    idx <- cereals[,"manufacturername"] == "Kraft"
    plot(log(cereals$quant_weekly[idx]),log(cereals$price_weekly[idx]),cex=0.3,main="Kraft")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals[idx,]),col="red")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="blue")
    # Nabisco / Kraft
    idx <- cereals[,"manufacturername"] == "Nabisco / Kraft"
    plot(log(cereals$quant_weekly[idx]),log(cereals$price_weekly[idx]),cex=0.3,main="Nabisco / Kraft")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals[idx,]),col="red")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="blue")
    # Quaker Oats
    idx <- cereals[,"manufacturername"] == "Quaker Oats"
    plot(log(cereals$quant_weekly[idx]),log(cereals$price_weekly[idx]),cex=0.3,main="Quaker Oats")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals[idx,]),col="red")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="blue")
    # Ralston
    idx <- cereals[,"manufacturername"] == "Ralston"
    plot(log(cereals$quant_weekly[idx]),log(cereals$price_weekly[idx]),cex=0.3,main="Ralston")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals[idx,]),col="red")
    abline(lm(log(price_weekly) ~ log(quant_weekly), data=cereals),col="blue")

# Demand
    
cereals$price_oz <- cereals$price_weekly/cereals$nsize
tmp <- tapply(cereals$price_oz,list(cereals$week,cereals$manufacturername),mean,na.rm=T)
tmp2<-tapply(cereals$quant_weekly,list(cereals$week,cereals$manufacturername),mean,na.rm=T)
names(tmp2)<-paste0(names(tmp2),"_qty")
names(tmp)<-paste0(names(tmp),"_price")
demand_data <- cbind(tmp,tmp2)
demand_data <- as.data.frame(demand_data)
names(demand_data) <- make.names(names(demand_data), unique=TRUE)
model_Kelloggs <- lm(log(Kelloggs_price)~log(Kelloggs_qty)+log(Dominicks_price)+log(General.Mills_price)+log(Kraft_price)+log(Nabisco...Kraft_price)+log(Quaker.Oats_price),data=demand_data)
model_Dominicks <- lm(log(Dominicks_price)~log(Dominicks_qty)+log(Kelloggs_price)+log(General.Mills_price)+log(Kraft_price)+log(Nabisco...Kraft_price)+log(Quaker.Oats_price),data=demand_data)
summary(model_Dominicks)

demand_Dominicks <- lm((cereals[cereals[,"manufacturername"] == "Dominicks","price_weekly"]/cereals[cereals[,"manufacturername"] == "Dominicks","nsize"]) ~ cereals[cereals[,"manufacturername"] == "Dominicks","quant_weekly"] + 
                         cereals[cereals[,"manufacturername"] == "General Mills","price_weekly"] + 
                         cereals[cereals[,"manufacturername"] == "Kelloggs","price_weekly"] + 
                         cereals[cereals[,"manufacturername"] == "Kraft","price_weekly"] + 
                         cereals[cereals[,"manufacturername"] == "Nabisco / Kraft","price_weekly"] + 
                         cereals[cereals[,"manufacturername"] == "Quaker Oats","price_weekly"] + 
                         cereals[cereals[,"manufacturername"] == "Ralston","price_weekly"])
lm(cereals[cereals[,"manufacturername"] == "Dominicks","price_weekly"] ~ cereals[cereals[,"manufacturername"] == "Dominicks","quant_weekly"] + cereals[cereals[,"manufacturername"] == "General Mills","price_weekly"])
par(mfrow=c(1,1)) 
hist(cereals[cereals[,"nitem"]==2503051,"quant_weekly"])
