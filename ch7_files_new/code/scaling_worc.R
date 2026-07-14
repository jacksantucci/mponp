library(pscl)
library(oc)
library(wnominate)

source("scaling_functions.R")

# load('../repeal_stv_june2018/data/rcs.worc.Rdata')
load('../data/rcs.worc.Rdata')

rcs.worc2 <- list(NA)
for (i in 1:length(rcs.worc)){
	rownames(rcs.worc[[i]]$votes) <- paste0(rownames(rcs.worc[[i]]$votes), ' (', rcs.worc[[i]]$legis.data$party, ')')
}

## if member appears in data more than once, number them in their rowname

rownames(rcs.worc[[6]]$votes)[rownames(rcs.worc[[6]]$votes)=="Tomaiolo (d)"] <- "Tomaiolo2 (d)"

# member.terms <- lapply(rcs.worc, function(x) rownames(x$votes))

# term.first.yr <- seq(1950, 1960, 2)

# termVarNames <- paste0("term", term.first.yr)

all.cincy <- combineRCs(rcs.worc, forDynIRT=F)

# ### fix "Replaces..." in rownames
# rownames(all.cincy) <- substr(rownames(all.cincy), regexpr('\\n', rownames(all.cincy))+1, nchar(rownames(all.cincy)))

all.cincy.firstyr.idx <- apply(all.cincy, 1, function(x) which(x %in% c("1", "0"))[[1]])
all.cincy.firstyr <- unlist(lapply(all.cincy.firstyr.idx, function(x) substr(dimnames(all.cincy)[[2]][[x]], 1, 4)))

all.cincy.party <- substr(rownames(all.cincy), regexpr('\\(', rownames(all.cincy))+1, regexpr('\\)', rownames(all.cincy))-1)

all.cincy.rcs <- rollcall(all.cincy, legis.names=rownames(all.cincy))
all.cincy.rcs$legis.data$party <- all.cincy.party
all.cincy.rcs$legis.data$first.year <- as.numeric(all.cincy.firstyr)


set.seed(1776)

oc.cincy <- oc(all.cincy.rcs, polarity=c("Holmstrom (CEA-r)", "Sweeney (CEA-d)"))

plot.OCcoords(oc.cincy)

### compute decade average cutting angles

oc.cincy$legislators$font <- NA
# oc.cincy$legislators$font[grep("193", oc.cincy$legislators$first.year)] <- 1
# oc.cincy$legislators$font[grep("194", oc.cincy$legislators$first.year)] <- 2
# oc.cincy$legislators$font[grep("195", oc.cincy$legislators$first.year)] <- 3
oc.cincy$legislators$font[oc.cincy$legislators$first.year<=1955] <- 1
oc.cincy$legislators$font[oc.cincy$legislators$first.year>1955] <- 2

pdf('../graphics/ideal_points_worc_NAMES.pdf')
plot(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, pch=NA, main="Worcester, 1950-60", xlab="1st dimension", ylab="2nd dimension", axes=F, ylim=c(-1, 1), xlim=c(-1, 1))
text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=rownames(oc.cincy$legislators), cex=2/3, font=oc.cincy$legislators$font)
# text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=oc.cincy$legislators$party, cex=1, font=oc.cincy$legislators$font)
axis(1, tick=F)
axis(2, tick=F, las=2)
mtext("Bold = first served 1956 or later")
dev.off()

pdf('../graphics/fig7.7_ideal_points_worc.pdf')

plot(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, pch=NA, main="Worcester, 1950-60", xlab="1st dimension", ylab="2nd dimension", axes=F, ylim=c(-1, 1), xlim=c(-1, 1))
text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=oc.cincy$legislators$party, cex=2/3, font=oc.cincy$legislators$font)
# text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=oc.cincy$legislators$party, cex=1, font=oc.cincy$legislators$font)
axis(1, tick=F)
axis(2, tick=F, las=2)
mtext("Bold = first served 1956 or later")

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Lane (CEA-r)"], x0=0, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Lane (CEA-r)"], y0=0.8, length=0, lty=2)
text(0, 0.825, "Lane", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Casdin (CEA-d)"], x0=0.15, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Casdin (CEA-d)"], y0=0.6, length=0, lty=2)
text(0.15, 0.625, "Casdin", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="O'BrienJD (CEA-d)"]-0.06, x0=-0.525, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="O'BrienJD (CEA-d)"], y0=0.3, length=0, lty=2)
text(-0.615, 0.3, "O'Brien II", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="O'BrienJD (d)"]+0.01, x0=0.8, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="O'BrienJD (d)"], y0=-0.1, length=0, lty=2)
text(0.9, -0.095, "O'Brien I", cex=2/3, font=1)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Wells (d)"]+0.01, x0=-0.1, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Wells (d)"], y0=-0.1, length=0, lty=2)
text(-0.1, -0.075, "Wells", cex=2/3, font=1)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Favulli (CEA-d)"], x0=0.1, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Favulli (CEA-d)"], y0=0.1, length=0, lty=2)
text(0.1, 0.075, "Favulli II", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Tomaiolo2 (d)"], x0=-0.5, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Tomaiolo2 (d)"]-0.01, y0=-0.3, length=0, lty=2)
text(-0.5, -0.325, "Tomaiolo II", cex=2/3, font=2)

dev.off()