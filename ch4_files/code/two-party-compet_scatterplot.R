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

# ### import GOV into G_GOV as needed

# edta$G_GOV_0100[is.na(edta$G_GOV_0100)] <- edta$GOV_0100[is.na(edta$G_GOV_0100)]
# edta$G_GOV_0200[is.na(edta$G_GOV_0200)] <- edta$GOV_0200[is.na(edta$G_GOV_0200)]

### reduce general election data to gubernatorial
govdta <- edta[,c(1,2,3,4,5,grep('^G_GOV_', names(edta)), grep('^GOV_', names(edta)))]
# govdta <- burn.empty.cols(govdta)

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


### code Dem and Rep pct of total

# govdtac$dempct <- govdtac$G_GOV_0100/govdtac$G_GOV_TOTAL
# govdtac$reppct <- govdtac$G_GOV_0200/govdtac$G_GOV_TOTAL

# govdtac$dempct[govdtac$dempct==0] <- govdtac$GOV_0100[govdtac$dempct==0]/govdtac$G_GOV_TOTAL[govdtac$dempct==0]

# govdtac$reppct[govdtac$reppct==0] <- govdtac$GOV_0200[govdtac$reppct==0]/govdtac$G_GOV_TOTAL[govdtac$reppct==0]

### look at result
glook <- subset(govdtac, select=c("icpsr_state_code","IDENTIFICATION_NUMBER","COUNTY_NAME","year","G_GOV_0100","G_GOV_0200", "GOV_0100","G_GOV_TOTAL")) # not in datq

### set up data frames for reform episode regression analysis

test <- merge(ctydta, glook, all.x=T, by.x=c("icpsr.state.code","icpsr.county.code","govyear"), by.y=c("icpsr_state_code","IDENTIFICATION_NUMBER","year"))

reforms <- subset(test, select=c("city", "state", "COUNTY_NAME", "haspr", "adoptyear", "ineffect", "govyear", "G_GOV_0100", "G_GOV_0200", "G_GOV_TOTAL"))

#### PLOT

reforms$plotpch <- rep(NA, nrow(reforms))
reforms$plotpch[reforms$haspr==1] <- 16
reforms$plotpch[reforms$haspr==0] <- 1
reforms$plotpch[reforms$ineffect<1916] <- 8
reforms$plotpch[reforms$adoptyear<1916] <- 8
# reforms$plotpch[which(reforms$adoptyear == 0 && reforms$ineffect<1916)] <- 8

reforms[is.na(reforms)] <- 0

reforms$dempct <- 100*(reforms$G_GOV_0100/reforms$G_GOV_TOTAL)
reforms$reppct <- 100*(reforms$G_GOV_0200/reforms$G_GOV_TOTAL)

pdf('../graphics/noncompet_new.pdf')
plot(reforms$dempct, reforms$reppct, pch=reforms$plotpch, xlim=c(0,100), ylim=c(0, 100), axes=F, xlab="Democratic percent", ylab="Republican percent", main="Manager charters by county competitiveness (gubernatorial)")
abline(0, 1)
abline(100, -1)
axis(1, tick=F)
axis(2, tick=F, las=2)
text(90, 78, "Line of\ntwo-party parity")
# text(12, 100, "Third-party\nboundary")
text(4, 85, "Third-party\nboundary")
legend("top", pch=c(16, 1, 8), legend=c("STV charter", "Non-STV charter", "Pre-1916 charter"))
# text(reforms$dempct[reforms$city=="Kansas City"], reforms$reppct[reforms$city=="Kansas City"], labels="Kansas City (MO)", cex=0.5, pos=1)
dev.off()

#### strings of charter adoptions

pr.only <- reforms[reforms$haspr==1,]
pr.only <- pr.only[order(pr.only$ineffect),]

non.pr <- reforms[reforms$haspr==0,]
non.pr <- non.pr[order(non.pr$ineffect),]

pre.pr <- reforms[reforms$plotpch==8,]
pre.pr <- pre.pr[order(pre.pr$ineffect),]

cat(paste0(pr.only$city, ', ', pr.only$state, ' (', pr.only$ineffect, '); '), sep="")

cat(paste0(non.pr$city, ', ', non.pr$state, ' (', non.pr$ineffect, '); '), sep="")

cat(paste0(pre.pr$city, ', ', pre.pr$state, ' (', pre.pr$ineffect, '); '), sep="")

## check for close non-pr charters

# non.pr$pctdiff <- 

non.pr[order(abs(non.pr$dempct-non.pr$reppct)),]