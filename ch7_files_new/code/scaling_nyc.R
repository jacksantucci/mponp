library(pscl)
library(oc)

source("scaling_functions.R")

# load('../repeal_stv_june2018/data/rcs.nyc.Rdata')
load('../data/rcs.nyc.Rdata')

rcs.nyc2 <- list(NA)
for (i in 1:length(rcs.nyc)){
	rownames(rcs.nyc[[i]]$votes) <- paste0(rownames(rcs.nyc[[i]]$votes), ' (', rcs.nyc[[i]]$legis.data$party, ')')
}

lapply(rcs.nyc, function(x) rownames(x$votes))

## if member appears in data more than once, number them in their rowname

rownames(rcs.nyc[[4]]$votes)[rownames(rcs.nyc[[4]]$votes)=="QuillMJ (AL)"] <- "QuillMJ2 (AL)"
rownames(rcs.nyc[[5]]$votes)[rownames(rcs.nyc[[5]]$votes)=="QuillMJ (AL)"] <- "QuillMJ2 (AL)"

all.cincy <- combineRCs(rcs.nyc, forDynIRT=F)

# ### fix "Replaces..." in rownames
# rownames(all.cincy) <- substr(rownames(all.cincy), regexpr('\\n', rownames(all.cincy))+1, nchar(rownames(all.cincy)))

all.cincy.firstyr.idx <- apply(all.cincy, 1, function(x) which(x %in% c("1", "0"))[[1]])
all.cincy.firstyr <- unlist(lapply(all.cincy.firstyr.idx, function(x) substr(dimnames(all.cincy)[[2]][[x]], 1, 4)))

all.cincy.party <- substr(rownames(all.cincy), regexpr('\\(', rownames(all.cincy))+1, regexpr('\\)', rownames(all.cincy))-1)

all.cincy.rcs <- rollcall(all.cincy, legis.names=rownames(all.cincy))
all.cincy.rcs$legis.data$party <- all.cincy.party
all.cincy.rcs$legis.data$first.year <- as.numeric(all.cincy.firstyr)


set.seed(1776)

oc.cincy <- oc(all.cincy.rcs, polarity=c("BaldwinJC (R)", "EarleGB (F)"))

plot.OCcoords(oc.cincy)

### compute decade average cutting angles

oc.cincy$legislators$font <- NA
# oc.cincy$legislators$font[grep("193", oc.cincy$legislators$first.year)] <- 1
# oc.cincy$legislators$font[grep("194", oc.cincy$legislators$first.year)] <- 2
# oc.cincy$legislators$font[grep("195", oc.cincy$legislators$first.year)] <- 3
oc.cincy$legislators$font[oc.cincy$legislators$first.year<=1941] <- 1
oc.cincy$legislators$font[oc.cincy$legislators$first.year>1941] <- 2

pdf('../graphics/ideal_points_nyc_NAMES.pdf')
plot(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, pch=NA, main="New York City, 1938-47", xlab="1st dimension", ylab="2nd dimension", axes=F, ylim=c(-1, 1), xlim=c(-1, 1))
text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=rownames(oc.cincy$legislators), cex=2/3, font=oc.cincy$legislators$font)
# text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=oc.cincy$legislators$party, cex=1, font=oc.cincy$legislators$font)
axis(1, tick=F)
axis(2, tick=F, las=2)
mtext("Bold = first served 1942 or later")
dev.off()

pdf('../graphics/fig7.4_ideal_points_nyc.pdf')

plot(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, pch=NA, main="New York City, 1938-47", xlab="1st dimension", ylab="2nd dimension", axes=F, ylim=c(-1, 1), xlim=c(-1, 1))

set.seed(50)
oc.cincy$legislators$x <- jitter(oc.cincy$legislators$coord1D, amount=0.05)
oc.cincy$legislators$y <- jitter(oc.cincy$legislators$coord2D, amount=0.05)

text(oc.cincy$legislators$x, oc.cincy$legislators$y, labels=oc.cincy$legislators$party, cex=2/3, font=oc.cincy$legislators$font)
# text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=oc.cincy$legislators$party, cex=1, font=oc.cincy$legislators$font)
axis(1, tick=F)
axis(2, tick=F, las=2)
mtext("Bold = first served 1942 or later")

arrows(x1=oc.cincy$legislators$x[rownames(oc.cincy$legislators)=="IsaacsSM (F.CNP)"], x0=0.4, y1=oc.cincy$legislators$y[rownames(oc.cincy$legislators)=="IsaacsSM (F.CNP)"], y0=-0.1, length=0, lty=2)
text(0.4, -0.075, "Isaacs I", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$x[rownames(oc.cincy$legislators)=="IsaacsSM (R)"], x0=0.4, y1=oc.cincy$legislators$y[rownames(oc.cincy$legislators)=="IsaacsSM (R)"], y0=-0.8, length=0, lty=2)
text(0.4, -0.825, "Isaacs II", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$x[rownames(oc.cincy$legislators)=="DavisBJ (C)"], x0=-0.1, y1=oc.cincy$legislators$y[rownames(oc.cincy$legislators)=="DavisBJ (C)"], y0=-0.7, length=0, lty=2)
text(-0.1, -0.725, "Davis", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$x[rownames(oc.cincy$legislators)=="CacchionePV (C)"], x0=0.6, y1=oc.cincy$legislators$y[rownames(oc.cincy$legislators)=="CacchionePV (C)"], y0=-0.7, length=0, lty=2)
text(0.6, -0.725, "Cacchione", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$x[rownames(oc.cincy$legislators)=="EarleGB (R.CNP)"], x0=0.3, y1=oc.cincy$legislators$y[rownames(oc.cincy$legislators)=="EarleGB (R.CNP)"], y0=-0.4, length=0, lty=2)
text(0.3, -0.375, "Earle IV", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$x[rownames(oc.cincy$legislators)=="SharkeyJT (D)"], x0=-0.5, y1=oc.cincy$legislators$y[rownames(oc.cincy$legislators)=="SharkeyJT (D)"], y0=0.6, length=0, lty=2)
text(-0.5, 0.625, "Sharkey", cex=2/3, font=1)

dev.off()