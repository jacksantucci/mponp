## NOTE: check on Saugus and Hopkins election years, also Boulder

d <- read.delim('../data/pr_cities.tsv')

d$lastElection[d$city=="Cambridge"] <- 2021

d <- d[order(d$adoptYear, decreasing=T),]

repeals <- strsplit(d$otherRepealAttempts, split=', ')
repealAttempts <- as.data.frame(do.call(rbind, lapply(repeals, '[', 1:max(sapply(repeals, length))))) 

# adopts <- strsplit(d$otherAdoptAttempts, split=', ')
# adoptAttempts <- as.data.frame(do.call(rbind, lapply(adopts, '[', 1:max(sapply(adopts, length))))) 

pdf('../output/fig1.1_spells_stv.pdf')
plot(1, 1, pch=NA, axes=F, ylim=c(1, nrow(d)), xlim=c(1900, 1970), xlab="", ylab="", sub="Filled triangle: adopted or repealed. Empty triangle: failed adoption or repeal.\n Non-referendum repeal: court (C), state legislature (L), sale by U.S. Congress (S).\n*Federally administered with advisory council.")
for (i in 1:nrow(d)){
	segments(x0=d$firstElection[i], x1=d$lastElection[i], y0=i, lwd=3)
	text(x=1900, y=i, pos=4, paste0(d$city[i], ', ', d$state[i]), cex=3/4)
	points(x=d$adoptYear[i], y=i, pch=24, bg="black")
	points(x=d$repealYear[i], y=i, pch=25, bg="black")
	for (j in 1:ncol(repealAttempts)){
		points(repealAttempts[i,j], i, pch=25, bg="white")
	}
	# for (j in 1:ncol(adoptAttempts)){
		# points(adoptAttempts[i,j], i, pch=24, bg="white")
	# }
	points(x=d$otherAdoptAttempts[i], y=i, pch=24, bg="white")
	text(x=d$repealYear[i], y=i, labels=d$repealCode[i], font=2, pos=4, cex=3/4)
}
axis(1, tick=T, at=seq(1915, 1970, 5))
dev.off()