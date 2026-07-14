recodeElecData <- function(df, first.transfer.col.no, strip.party=F){
	last.id.col.no <- first.transfer.col.no-1
	dta <- df
	names(dta) <- dta[1,]
	dta <- dta[2:nrow(dta),]
	dta <- data.frame(cbind(dta[,1: last.id.col.no], apply(dta[, first.transfer.col.no:ncol(dta)], 2, as.numeric)))
	dta[is.na(dta)] <- 0
	dta[dta==" "] <- 0
	dta[is.na(dta)] <- 0
	dta[dta=="?"] <- "Z" # unknown party affiliation
	if (strip.party){
		dta$Party <- substr(dta$Party, 1, 1)
	}
	return(dta)
}

rep.row<-function(x,n){
	matrix(rep(x,each=n),nrow=n)
}

getInterRoundTransferReport <- function(transferRoundColNumber, dta){
	thisRound <- as.numeric(dta[,transferRoundColNumber])
	prevRound <- as.numeric(dta[,transferRoundColNumber-1])
	rawChange <- thisRound-prevRound
	transferRowNum <- which(rawChange<0)
	totalTransferAmount <- sum(rawChange[rawChange>0])
	pctsOfTransfer <- rawChange/totalTransferAmount
	pctsOfTransfer[pctsOfTransfer<0] <- 0
	rawChange[rawChange<0] <- 0
	out <- data.frame(cbind(dta[,c(1,2)], rawChange, pctsOfTransfer))
	result1 <- aggregate(pctsOfTransfer ~ Party, data=out, sum)
	result2 <- aggregate(rawChange ~ Party, data=out, sum)
	result <- merge(result1, result2)
	result$thisRoundIs <- rep(names(dta[transferRoundColNumber]), nrow(result))
	result$votesCameFrom <- rep(paste0(dta$Party[transferRowNum], collapse="&"), nrow(result))
	# result2 <- cbind.data.frame(result, rep.row(dta[transferRowNum,1:5], nrow(result)), stringsAsFactors=F)
	# names(result2)[5:9] <- names(dta)[1:5]
	# names(result2)[6] <- "Party2"
	# # result[nrow(result)+1,] <- c("Exhaust", (totalTransferAmount-sum(result$rawChange))/totalTransferAmount, totalTransferAmount-sum(result$rawChange), names(dta[transferRoundColNumber]), paste0(dta$Party[transferRowNum], collapse="&"))	
	return(result)
}

getInterRoundTransferReport2 <- function(transferRoundColNumber, dta){
	thisRound <- as.numeric(dta[,transferRoundColNumber])
	prevRound <- as.numeric(dta[,transferRoundColNumber-1])
	rawChange <- thisRound-prevRound
	transferRowNum <- which(rawChange<0)
	totalTransferAmount <- sum(rawChange[rawChange>0])
	pctsOfTransfer <- rawChange/totalTransferAmount
	pctsOfTransfer[pctsOfTransfer<0] <- 0
	rawChange[rawChange<0] <- 0
	out <- data.frame(cbind(dta[,c(1,2)], rawChange, pctsOfTransfer))
	# result1 <- aggregate(pctsOfTransfer ~ Party, data=out, sum)
	# result2 <- aggregate(rawChange ~ Party, data=out, sum)
	# result <- merge(result1, result2)
	out$thisRoundIs <- rep(names(dta[transferRoundColNumber]), nrow(out))
	# result$votesCameFrom <- rep(paste0(dta$Party[transferRowNum], collapse="&"), nrow(result))
	out2 <- cbind.data.frame(out, rep.row(dta[transferRowNum,1:5], nrow(out)), stringsAsFactors=F)
	names(out2)[6:10] <- names(dta)[1:5]
	names(out2)[6] <- "Candidate.send"
	names(out2)[7] <- "Party.send"
	out2$is.winner <- dta[,ncol(dta)]>0
	out3 <- as.data.frame(apply(out2, 2, unlist), stringsAsFactors=F)
	out3$rawChange <- as.numeric(out3$rawChange)
	out3$pctsOfTransfer <- as.numeric(out3$pctsOfTransfer)
	# # result[nrow(result)+1,] <- c("Exhaust", (totalTransferAmount-sum(result$rawChange))/totalTransferAmount, totalTransferAmount-sum(result$rawChange), names(dta[transferRoundColNumber]), paste0(dta$Party[transferRowNum], collapse="&"))	
	return(out3)
}

## does whole matrix

transferReport <- function(dta, firstTransferCol){
	idx.one <- firstTransferCol+1
	lapply(idx.one:ncol(dta), function(x) getInterRoundTransferReport2(x, dta))
}

## does interparty transfers

# transfer.matrix <- ctrans[[1]]

interpartyTransfers <- function(transfer.matrix, winners.only=T){
	rpt <- lapply(7:ncol(transfer.matrix), function(x) getInterRoundTransferReport2(x, transfer.matrix))
	rpt2 <- do.call(rbind, rpt)
	if (winners.only==T){
		rpt3 <- rpt2[rpt2$is.winner=="TRUE",] # subset to winners
	} else {
		rpt3 <- rpt2
	}
	rpt4 <- aggregate(rawChange ~ Party + Party.send, data=rpt3, sum)
	rpt4.split <- split(rpt4, rpt4$Party.send)
	for (i in 1:length(rpt4.split)){
		rpt4.split[[i]]$pct <- rpt4.split[[i]]$rawChange/sum(rpt4.split[[i]]$rawChange)
	}
	rpt5 <- do.call(rbind, rpt4.split)
	rpt5$from.to <- paste0(rpt5$Party.send, '.', rpt5$Party)
	return(rpt5)
}

intercoalitionTransfers <- function(from.interpartyTransfers){
	ipdf <- from.interpartyTransfers
	ipdf$Party[ipdf$Party=="RD"] <- "VD" # kludge... VD=roosevelt dems
	ipdf$Party.send[ipdf$Party.send=="RD"] <- "VD" # kludge
	ipdf$Party <- substr(ipdf$Party, 1, 1)
	ipdf$Party.send <- substr(ipdf$Party.send, 1, 1)
	agg <- aggregate(rawChange ~ Party + Party.send, data=ipdf, sum)
	agg.split <- split(agg, agg$Party.send)
	for (i in 1:length(agg.split)){
		agg.split[[i]]$pct <- agg.split[[i]]$rawChange/sum(agg.split[[i]]$rawChange)
	}
	out <- do.call(rbind, agg.split)
	return(out)
}

enepFromTransfers <- function(tdf){
	sum.fc <- sum(tdf$r1)
	by.party.fc <- aggregate(r1 ~ Party, tdf, sum)
	by.party.fc$prop <- by.party.fc$r1/sum.fc
	enep <- 1/sum(by.party.fc$prop^2)
	return(enep)
}

enecFromTransfers <- function(tdf){
	tdf$r1 <- as.numeric(tdf$r1)
	sum.fc <- sum(tdf$r1)
	# by.party.fc <- aggregate(r1 ~ Party, tdf, sum)
	prop <- tdf$r1/sum.fc
	enep <- 1/sum(prop^2)
	return(enep)
}