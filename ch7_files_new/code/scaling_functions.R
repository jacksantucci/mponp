combineRCs <- function(rc.obj.list, forDynIRT=T, stripParty=F){
	theVotes <- lapply(rc.obj.list, '[[', 'votes')
	votesDFs <- lapply(theVotes, as.data.frame, stringsAsFactors=F)
	for (i in 1:length(votesDFs)){
		legislator <- rownames(votesDFs[[i]])
		if (stripParty){
			legislator <- substr(legislator, 1, regexpr("\\(", legislator)-2)
		}
		votesDFs[[i]]$legislator <- legislator
	}
	allVotes <- Reduce(function(...) merge(..., by="legislator", all=TRUE), votesDFs)
	theRownames <- allVotes$legislator
	allVotes <- allVotes[,2:ncol(allVotes)]
	allVotes <- apply(allVotes, 2, as.numeric)
	
	if(forDynIRT){
		allVotes[is.na(allVotes)] <- 0
	}
	
	rownames(allVotes) <- theRownames
	return(allVotes)
}

apreFromOC <- function(oc.output){
	errs <- oc.output$rollcalls[,"wrongYea"]+oc.output$rollcalls[,"wrongNay"]
	nays <- oc.output$rollcalls[,"correctNay"] + oc.output$rollcalls[,"wrongNay"]
	yeas <- oc.output$rollcalls[,"correctYea"] + oc.output$rollcalls[,"wrongYea"]
	minVote <- ifelse(yeas > nays, nays, yeas)
	apre <- sum(minVote-errs, na.rm=T)/sum(minVote, na.rm=T)
	return(apre)
}

plotOC2 <- function(oc.output, d1=1, d2=2, legisName=T, textCex=1, xlab='', ylab='', axes=F, main="Jack's OC plot"){
	legs <- oc.output$legislators
	legNames <- substr(rownames(legs), 1, regexpr(' ', rownames(legs))-1)
	coords <- legs[,grep('coord', names(legs))]
	plot(coords[,d1], coords[,d2], bty='n', axes=F, ylab=ylab, xlab=xlab, main=main, pch=NA, sub=paste0(100*round(oc.output$fits[1], 3), "% of votes correctly classified.\nAPRE = ", round(oc.output$fits[2], 3), '.'))
	if(axes){
		axis(1, tick=T)
		axis(2, tick=T, las=2)
	}
	if(legisName){
		text(coords[,d1], coords[,d2], labels=paste0(legNames, '\n', legs$party), cex=textCex)
	} else {
		text(coords[,d1], coords[,d2], labels=paste0(legs$party), cex=textCex)
	}
}