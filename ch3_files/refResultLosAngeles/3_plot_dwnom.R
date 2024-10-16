dat <- read.csv("https://voteview.com/static/data/out/members/HSall_members.csv")

d <- dat

# drop president
d <- d[d$chamber!="President",]

# only House

d <- d[d$chamber=="House",]

# only 63rd Congress (March 4, 1913, to March 4, 1915)

d <- d[d$congress==63,]

d$plotchar <- rep(NA, nrow(d))
d$plotchar[d$party_code==100] <- "D"
d$plotchar[d$party_code==200] <- "R"
d$plotchar[d$party_code==328] <- "I"
d$plotchar[d$party_code==370] <- "P"

d$plotchar[d$bioname=="BELL, Charles Webster"] <- "BELL\n(Prog. Rep.)"

d$plotcol <- ifelse(d$state_abbrev=="CA", "black", "darkgray")
d$plotfont <- ifelse(d$state_abbrev=="CA", 2, 1)
d$plotcex <- ifelse(d$state_abbrev=="CA", 3/3, 2/3)

# plot

pdf("nominate_63rd_us_house.pdf")
plot(d$nominate_dim1, d$nominate_dim2, ylim=c(-1,1), xlim=c(-1,1), pch=NA, xlab="Left-right conservatism", ylab="Off-dimension conservatism", axes=F, main="63rd U.S. House (1913-15):\nIdeological locations of members")
text(d$nominate_dim1, d$nominate_dim2, d$plotchar, col=d$plotcol, font=d$plotfont, cex=d$plotcex)
axis(1, tick=F)
axis(2, tick=F, las=2)
mtext("*CA delegation in black.", 1, line=4, at=0.75, font=3)
dev.off()