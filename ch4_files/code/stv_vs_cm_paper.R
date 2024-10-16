######
# PR Adoptions among cities choosing c-m charters
# October 2014 (rev. November 30, 2020)
######

### helper functions
burn.empty.cols <- function(df){
	df <- df[,colSums(is.na(df))<nrow(df)]
	return(df)
}

enp <- function(vector){
	enp.actual <- 1/sum((vector[is.na(vector)==F])^2)
	return(enp.actual)
	}

diffit <- function(vec){
	out <- c(NA, diff(vec))
	return(out)
}

### general election data
library(foreign)
edta <- read.csv("../data/elec1960_scrubbed.csv", stringsAsFactors=F)

### reduce general election data to gubernatorial
govdta <- edta[,c(1,2,3,4,5,grep('^G_GOV_', names(edta)))]
govdta <- burn.empty.cols(govdta)

### import GOV into G_GOV as needed

edta$G_GOV_0100[is.na(edta$G_GOV_0100)] <- edta$GOV_0100[is.na(edta$G_GOV_0100)]
edta$G_GOV_0200[is.na(edta$G_GOV_0200)] <- edta$GOV_0200[is.na(edta$G_GOV_0200)]


### city data
ctydta <- read.csv("../data/pr_adopt.csv", stringsAsFactors=F)
ctydta <- ctydta[which(ctydta$diss.case==1),]
ctydta$hascm <- rep(1, nrow(ctydta))

### reduce general election gubernatorial data to counties needed
counties <- unique(subset(ctydta, select=c("icpsr.state.code","icpsr.county.code"))) #from city data
govdtac <- merge(govdta, counties, by.x=c("icpsr_state_code","IDENTIFICATION_NUMBER"), by.y=c("icpsr.state.code","icpsr.county.code"))
govdtac <- govdtac[order(govdtac$icpsr_state_code, govdtac$IDENTIFICATION_NUMBER, govdtac$year),]

### drop any rows where G_GOV_TOTAL is NA (to get rid of missing data years)
govdtac <- govdtac[!is.na(govdtac$G_GOV_TOTAL),]

### fixes for CA 1918 need to go HERE!!!
attach(govdtac)
# County 370
govdtac$G_GOV_0100[icpsr_state_code==71 & IDENTIFICATION_NUMBER==370 & year==1918] <- govdtac$G_GOV_0328[icpsr_state_code==71 & IDENTIFICATION_NUMBER==370 & year==1918]

govdtac$G_GOV_0200[icpsr_state_code==71 & IDENTIFICATION_NUMBER==370 & year==1918] <- govdtac$G_GOV_0589[icpsr_state_code==71 & IDENTIFICATION_NUMBER==370 & year==1918]

govdtac$G_GOV_0328[icpsr_state_code==71 & IDENTIFICATION_NUMBER==370 & year==1918] <- NA

govdtac$G_GOV_0529[icpsr_state_code==71 & IDENTIFICATION_NUMBER==370 & year==1918] <- NA

# County 590
govdtac$G_GOV_0100[icpsr_state_code==71 & IDENTIFICATION_NUMBER==590 & year==1918] <- govdtac$G_GOV_0328[icpsr_state_code==71 & IDENTIFICATION_NUMBER==590 & year==1918]

govdtac$G_GOV_0200[icpsr_state_code==71 & IDENTIFICATION_NUMBER==590 & year==1918] <- govdtac$G_GOV_0589[icpsr_state_code==71 & IDENTIFICATION_NUMBER==590 & year==1918]

govdtac$G_GOV_0328[icpsr_state_code==71 & IDENTIFICATION_NUMBER==590 & year==1918] <- NA

govdtac$G_GOV_0529[icpsr_state_code==71 & IDENTIFICATION_NUMBER==590 & year==1918] <- NA

# County 670
govdtac$G_GOV_0100[icpsr_state_code==71 & IDENTIFICATION_NUMBER==670 & year==1918] <- govdtac$G_GOV_0328[icpsr_state_code==71 & IDENTIFICATION_NUMBER==670 & year==1918]

govdtac$G_GOV_0200[icpsr_state_code==71 & IDENTIFICATION_NUMBER==670 & year==1918] <- govdtac$G_GOV_0589[icpsr_state_code==71 & IDENTIFICATION_NUMBER==670 & year==1918]

govdtac$G_GOV_0328[icpsr_state_code==71 & IDENTIFICATION_NUMBER==670 & year==1918] <- NA

govdtac$G_GOV_0529[icpsr_state_code==71 & IDENTIFICATION_NUMBER==670 & year==1918] <- NA
detach(govdtac)

### calculate enp

## get the election results only
freq <- govdtac[grep('^G_GOV',names(govdtac))]
freq$G_GOV_TOTAL <- NULL

## make df of party percents
freq2 <- sweep(freq, 1, rowSums(freq, na.rm=T), "/")

## stick enpg variable into govdtac
govdtac$enpg <- apply(freq2, 1, enp)

### calculate vol
freq3 <- 100*freq2 # make percentages
freq3$id <- paste(govdtac$icpsr_state_code, govdtac$IDENTIFICATION_NUMBER, sep='.')
freq3[is.na(freq3)]<-0 # set NA to zero
require(plyr)
freqdiff <- ddply(freq3, .(id), colwise(diffit))
diffabs <- abs(freqdiff[-1])
govdtac$vol <- apply(diffabs, 1, sum)/2

############

### code one-party jursidiction
govdtac$oneparty <- ifelse(xor(is.na(govdtac$G_GOV_0100), is.na(govdtac$G_GOV_0200)), 1, 0)

### put "uninformative priors" on any missing GOV_100 or GOV_200 for rows where only one is missing
attach(govdtac)
govdtac$thirdpartyobs <- ifelse(is.na(G_GOV_0100) & is.na(G_GOV_0200), 1, 0) # code no GOV republicans or democrats
detach(govdtac)

# # # # govdtac$G_GOV_0100[is.na(govdtac$G_GOV_0100)] <- 1 #code 1 vote for democrats
# # # # govdtac$G_GOV_0200[is.na(govdtac$G_GOV_0200)] <- 1 #code 1 vote for republicans

### code Dem and Rep pct of total
govdtac$dempct <- govdtac$G_GOV_0100/govdtac$G_GOV_TOTAL
govdtac$reppct <- govdtac$G_GOV_0200/govdtac$G_GOV_TOTAL

### code winning party
govdtac$govwin <- ifelse(govdtac$G_GOV_0100>govdtac$G_GOV_0200,"D","R")

### code zeroes as NA
govdtac[govdtac==0] <- NA
govdtac <- burn.empty.cols(govdtac)

### code RD ratio
govdtac$rdratio <- govdtac$G_GOV_0200/govdtac$G_GOV_0100

### code noncompet, or abs(ratio-1) ... interested in distance from 1:1=1
govdtac$noncompet <- abs(govdtac$rdratio-1)

### code alternative noncompet measure
govdtac$noncompet2 <- abs(1-(govdtac$reppct/govdtac$dempct))
noncompet2b <- 1-(govdtac$reppct/govdtac$dempct)

## code alternative noncompet measure number two (major parties only care about their own margin, not relation to third parties)
govdtac$dempct[is.na(govdtac$dempct)] <- 0
govdtac$reppct[is.na(govdtac$reppct)] <- 0
govdtac$noncompet3 <- abs(govdtac$reppct-govdtac$dempct)

### code RD ratio for pcts of total
govdtac$rdratio.pct <- govdtac$reppct/govdtac$dempct

### code Dem and Rep pct of 2-party
govdtac$dempct2p <- govdtac$G_GOV_0100/(govdtac$G_GOV_0100+govdtac$G_GOV_0200)
govdtac$reppct2p <- govdtac$G_GOV_0200/(govdtac$G_GOV_0100+govdtac$G_GOV_0200)

### code compet2p, or abs(ratio2p-1)
govdtac$compet2p <- abs(govdtac$reppct2p/govdtac$dempct2p-1)

### look at result
glook <- subset(govdtac, select=c("icpsr_state_code","IDENTIFICATION_NUMBER","COUNTY_NAME","year","govwin","rdratio", "rdratio.pct","noncompet","noncompet2","noncompet3","oneparty","G_GOV_0100","G_GOV_0200","G_GOV_TOTAL", "dempct","reppct","dempct2p","reppct2p", "vol", "enpg","rdratio.pct"))

### set up data frames for reform episode regression analysis

test <- merge(ctydta, glook, all.x=T, by.x=c("icpsr.state.code","icpsr.county.code","govyear"), by.y=c("icpsr_state_code","IDENTIFICATION_NUMBER","year"))

### merge df "test" with the census data ("foo" loaded from census.R file)
foo <- subset(read.csv("../data/census_data_cities.csv"), select=c("icpsr.state.code", "icpsr.county.code", "year", "dtotpop","name"))
test2 <- merge(test, foo, by.x=c("icpsr.state.code","icpsr.county.code","census.year"), by.y=c("icpsr.state.code","icpsr.county.code","year"), all.x=T)

### create per capita debt
test2$debtpc <- test2$debt1913oradopt/test2$totpop

# ### plot data (raw)
# attach(test)
# plot(jitter(G_GOV_0100), jitter(G_GOV_0200), type="n", xlab="Total Dem. votes (jittered)", ylab="Total Rep. votes (jittered)", main="Republican as Function of Democratic Gubernatorial Votes \nfor PR (solid dot) and non-PR Adopters")
# points(jitter(G_GOV_0100), jitter(G_GOV_0200), pch=ifelse(haspr==1,16,1))
# abline(0,1)
# detach(test)

### plot data (percentages)
attach(test)
plot(jitter(dempct), jitter(reppct), type="n", xlab="Dem. share of all votes (jittered)", ylab="Rep. share of all votes (jittered)", main="Republican as function of Democratic gubernatorial vote share \nfor PR (solid dot) and non-PR adopters", bty='n')
points(dempct, reppct, pch=ifelse(haspr==1,16,1))
abline(0,1)
text(x=0.68, y=0.7, label="Two-party parity", srt=45)
abline(1,-1)
text(x=0.67, y=0.35, label="No minor parties", srt=-45)
text(dempct, reppct, labels=test$city, cex=0.5)
detach(test)

# ### plot data (percentages of 2-party vote)
# attach(test)
# plot(jitter(dempct2p), jitter(reppct2p), type="n", xlab="Total Dem. votes (jittered)", ylab="Total Rep. votes (jittered)", main="Republican as Function of Democratic Gubernatorial Two-Party Vote Shares \nfor PR (solid dot) and non-PR Adopters")
# points(jitter(dempct2p), jitter(reppct2p), pch=ifelse(haspr==1,16,1))
# abline(c(1,0),c(0,1))
# detach(test)

# library(Zelig)
# z.out.logit <- zelig(haspr ~ noncompet3 + vol + enpg + trounstine.dominance + confederacy, model="logit", data=test, cite=F, robust=F)
# z.out.logit1 <- zelig(haspr ~ noncompet3, data=test, model="logit", cite=F, robust=T)
# z.out.logit2 <- zelig(haspr ~ rdratio, model="logit", data=test, cite=F, robust=T)
# z.out.logit3 <- zelig(haspr ~ enpg, model="logit", data=test, cite=F, robust=T)
# z.out.logit4 <- zelig(haspr ~ vol, model="logit", data=test, cite=F, robust=T)
# z.out.logit5 <- zelig(haspr ~ noncompet3 + dtotpop + darea + dnonwhitepct + dfmratio + dirishbornpct + drentownratio + dpopdens + durb25pct + debtpc, model="logit", data=test2, cite=F)
# z.out.logit6 <- zelig(haspr ~ enpg + vol + durb25pct + dfmratio + drentownratio + debtpc, model="logit", data=test2, cite=F)

# z.out.rel <- relogit(haspr ~ noncompet3 + trounstine.dominance + confederacy, data=test)
# z.out.rel.enp <- relogit(haspr ~ enpg + trounstine.dominance + confederacy, data=test)
# z.out.rel.vol <- relogit(haspr ~ vol + trounstine.dominance + confederacy, data=test)


# z.out.rel2 <- zelig(haspr ~ noncompet2 + trounstine.dominance , data=test, cite=F, model="relogit")
# z.out.rel3 <- zelig(haspr ~ noncompet2 + trounstine.dominance , data=test[which(is.na(test$oneparty)),], cite=F, model="relogit")
# z.out.rel4 <- zelig(haspr ~ enpg + trounstine.dominance , data=test, cite=F, model="relogit")
# z.out.rel5 <- zelig(haspr ~ vol + trounstine.dominance , data=test, cite=F, model="relogit")
# library(texreg)
# texreg(l=list(z.out.rel2, z.out.rel3, z.out.rel4, z.out.rel5), reorder.coef=c(1,2,4,5,3), custom.coef.names=c("Intercept","Non-competitiveness", "Dominance", "Volatility", "Eff. num. gub. parties"), custom.model.names=c("Competition","Competition (no one-party obs.)","Uncertainty","Minor parties"))


z.out.rel997 <- zelig(haspr ~ noncompet3 , data=test2, cite=F, model="relogit")
z.out.rel998 <- zelig(haspr ~ dtotpop , data=test2, cite=F, model="relogit")
z.out.rel999 <- zelig(haspr ~ noncompet3 + dtotpop , data=test2, cite=F, model="relogit")
z.out.rel999 <- zelig(haspr ~ log(noncompet3) + noncompet3 + dtotpop , data=test2, cite=F, model="relogit")

texreg(l=list(z.out.rel997, z.out.rel998, z.out.rel999), reorder.coef=c(), custom.coef.names=c("Intercept","Non-competitiveness", "Population"), custom.model.names=c("Disparity","Population","Combined"))

newdta <- with(test2, data.frame(dtotpop=mean(dtotpop), noncompet3=c(0.05, 0.2)))
newdta <- predict(z.out.rel999, newdata=newdta, type="response")

z.out.rel1 <- relogit(haspr ~ noncompet3, data=test)
z.out.rel2 <- relogit(haspr ~ rdratio.pct, data=test)
z.out.rel3 <- relogit(haspr ~ noncompet3 + dtotpop + drentownratio + dpopdens + debtpc, data=test2)

require(glm)

require(pscl)
hitmiss(z.out.logit)

require(elrm)
elrm.out <- elrm(formula = haspr ~ noncompet, interest = ~noncompet, iter=22000, dataset=test, burnIn=2000)

# ### merge glook result with the towns
# towns <- subset(ctydta, select=c("city","state","icpsr_ctname","ineffect","icpsr.state.code","icpsr.county.code","haspr"))
# gtlook 

## column examination
test[c(1,2,4,5,3,10,91,92,93,102,103)]