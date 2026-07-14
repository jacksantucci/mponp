### consequential transfer leakage (i.e., to eventual winners) -- may need to focus on leakage from winners -- OR: need to say "what percent of Charter's winners' vote came from Republicans"

### Repeal efforts from Robert Winters:
# 1952 (Y:23,030/N: 25,062)
# 1953 (Y:16,495/N: 19,372)
# 1957 (Y: 13,708/N: 18,516)
# 1961 (Y: 15,876/N: 16,331)
# 1965 (Y: 14,026/N: 16,562)

##################################################
# REPEAL TABLE FROM WINTERS' DATA
##################################################

# Referendum
# For PR
# Against PR
# Margin for PR

Referendum <- c(1952, 1953, 1957, 1961, 1965)
Replace <- c(23030, 16496, 13708, 15876, 14026)
Retain <-c(25062, 19372, 18516, 16331, 16562)
Margin <- paste0(100*round((Retain-Replace)/(Retain+Replace), 3), '%')

cambridge.repeal.table <- cbind(Referendum, Retain, Replace, Margin)

library(xtable)

xtable(cambridge.repeal.table, caption="Repeal referenda in Cambridge (MA).", label="tab:cambridgeRepeals")


source("functions_transfers.R")

library(readODS)

##################################################
# DATA LOADING AND CLEANING
##################################################

files <- dir('../data/cambridge_transfers')

files2 <- paste0('../data/cambridge_transfers/', files)

# new function below to deal with changes to readODS package (2026-07-14)
read_all_sheets <- function(path) {
  sheet_names <- list_ods_sheets(path)
  setNames(
    lapply(sheet_names, function(s)
      read_ods(path, sheet = s, as_tibble = FALSE, col_names = FALSE, na = character(0))),
    sheet_names
  )
}

dat40s <- read_all_sheets(files2[1])
dat50s <- read_all_sheets(files2[2])
dat60s <- read_all_sheets(files2[3])

# dat40s <- read_ods(files2[1]) # replaced period with underscore on 2026-07-14
# dat50s <- read_ods(files2[2]) # replaced period with underscore on 2026-07-14
# dat60s <- read_ods(files2[3]) # replaced period with underscore on 2026-07-14

########## Cleaning function

clean.cambridge.sheet <- function(sheet){

	# sheet <- dat40s[[1]]
	
	names(sheet) <- sheet[1,]
	exhaust.row <- grep('exhaust', ignore.case=T, sheet[,2])
	sheet <- sheet[2:(exhaust.row-1),]
	final.round <- grep('elected', ignore.case=T, sheet[1,])-1
	sheet <- sheet[,2:final.round]
	
	round.idx <- seq(3, final.round, 2)
	
	sheet <- sheet[,c(1,2,round.idx)]
	
	round.names <- paste0('r', seq_along(round.idx))
	
	names(sheet)[3:ncol(sheet)] <- round.names
	
	sheet$Party[sheet$Party==""] <- "?"
	
	return(sheet)
}

########## Cleaning sheets

the40s <- lapply(dat40s, clean.cambridge.sheet)
the50s <- lapply(dat50s, clean.cambridge.sheet)
the60s <- lapply(dat60s, clean.cambridge.sheet)

camtrans <- c(the40s, the50s, the60s)

# years <- seq(1925, 1955, 2)

getLeakage <- function(transfers.in){
	
	rounds <- transferReport(transfers.in, 4)
	
	winners <- lapply(rounds, function(x) x[x$is.winner==T,])
	
	# ## coding only leakage from winners here
	
	# sender.winner <- lapply(winners, function(x) x$Candidate.send %in% x$Candidate)
	
	# sums <- unlist(lapply(sender.winner, sum))
	
	# winners <- winners[sums>0]
	
	# ## end
	
	by.party <- lapply(winners, function(x) aggregate(rawChange ~ Party + Party.send, data=x, "sum"))
	
	by.party.single <- do.call(rbind, by.party)
	
	# by.party.single$Party[by.party.single$Party %in% c("C-r", "C-d")] <- "C"
	# by.party.single$Party.send[by.party.single$Party.send %in% c("C-r", "C-d")] <- "C"
	
	by.party.agg <- aggregate(rawChange ~ Party + Party.send, data=by.party.single, FUN="sum")
	
	by.sender <- split(by.party.agg, by.party.agg$Party.send)
	
	for (i in 1:length(by.sender)){
		by.sender[[i]]$percent <- by.sender[[i]]$rawChange/sum(by.sender[[i]]$rawChange)
	}
	
	leakage <- lapply(by.sender, function(x) x[x$Party != x$Party.send,])
	
	# next two lines compute leakage as percentage of party (or group's) total first-round vote
	
	r1.totals <- aggregate(as.numeric(r1) ~ Party, data=transfers.in, sum)
	
	for (i in 1:length(leakage)){
		leakage[[i]]$pct.tot.vote <- leakage[[i]]$rawChange/r1.totals[which(r1.totals$Party==leakage[[i]]$Party.send),2]
	}
	
	# leakage2 <- lapply(leakage, function(x) aggregate(percent ~ Party.send, data=x, FUN="sum"))
	
	# leakage3 <- do.call(rbind, leakage2)
	
	return(leakage)
}

foo <- lapply(camtrans, getLeakage)

CtoX <- lapply(foo, function(x) x$CCA)
CtoX <- lapply(CtoX, function(x) x[1,])

CXdf <- do.call(rbind, CtoX)
CXdf$year <- seq(1941, 1969, 2)

XtoC <- lapply(foo, function(x) x$`?`)
XtoC <- lapply(XtoC, function(x) x[1,])

XCdf <- do.call(rbind, XtoC)
XCdf$year <- seq(1941, 1969, 2)

##### Plot CCA leakage

pdf('../graphics/fig7.10_cca_leakage.pdf', width=10, height=7)
plot(CXdf$year, 100*CXdf$pct.tot.vote, type='l', main="CCA transfer leakage to non-CCA winners", ylab="Percent of observed transfers", xlab="Election", axes=F, ylim=c(0,15))
points(CXdf$year, 100*CXdf$pct.tot.vote, pch=16)
# lines(XCdf$year, 100*XCdf$pct.tot.vote, lty=2)
axis(1, tick=F, at=CXdf$year, las=2)
axis(2, tick=F, las=2)
points(x=c(1952, 1953, 1957, 1961, 1964), y=rep(0, 5), pch=rep(25, 5), bg=c(rep("white", 4), "white"))
legend("topright", legend=c("Failed repeal", "Successful repeal"), pch=c(25, 25), pt.bg=c("white", "black"))
dev.off()

##### Plot number of CCA nominees

party.vectors <- lapply(camtrans, function(x) x$Party)

cca.noms <- lapply(party.vectors, table)

cca.noms2 <- unlist(lapply(cca.noms, function(x) x[[2]]))

pdf('../graphics/fig7.11_cca_slate_size.pdf', width=10, height=7)
plot(CXdf$year, cca.noms2, type='l', axes=F, main="Number of CCA candidates", ylab="Number on slate", xlab="Election")
points(CXdf$year, cca.noms2, pch=20)
abline(h=9, lty=3)
text(y=9, x=1964, pos=3, labels="Full slate")
axis(1, tick=F, at=CXdf$year, las=2)
axis(2, tick=F, las=2)
points(x=c(1952, 1953, 1957, 1961, 1964), y=rep(5, 5), pch=rep(25, 5), bg=c(rep("white", 4), "white"))
legend("topright", legend=c("Failed repeal", "Successful repeal"), pch=c(25, 25), pt.bg=c("white", "black"))
dev.off()

### plot ENC and N of candidates

ncand <- unlist(lapply(camtrans, nrow))

vote.props <- lapply(camtrans, function(x) as.numeric(x$r1)/sum(as.numeric(x$r1)))
cenc <- unlist(lapply(vote.props, function(x) 1/sum(x^2)))

plot(XCdf$year, ncand, type='l', axes=F, ylim=c(0, max(ncand)), xlab="Election", ylab="No. of candidates", main="Candidate entry under PR in Cambridge (MA)")
lines(XCdf$year, cenc, lty=2)
axis(1, tick=F, las=2, at=XCdf$year)
axis(2, tick=F, las=2)
legend("topright", lty=c(1,2), legend=c("Raw", "Effective"), bty='n')

### table of council divisions

