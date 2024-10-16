library(readODS)
# d <- read.ods("refResultLosAngeles1913.ods", sheet=1)
d <- read.csv("refResultLosAngeles1913.csv")
# names(d) <- d[1,]
# d <- d[2:nrow(d),]
# d <- apply(d, 2, as.numeric)
# max.num <- which(is.na(d[,2])==1)[1]-1
# d <- d[1:max.num,]

rownames(d) <- d[,1]
d <- d[,2:ncol(d)]

yesses <- d[,grep("yes", dimnames(d)[[2]])]
noes <- d[,grep("no", dimnames(d)[[2]])]
totals <- yesses+noes

##### Compute maximum turnout by precint
# sum of competing harbor commission amendments (9 and 16)
# sum of competing voting amendments (8 and 17)
# sum of competing council meeting/salary amendments (6 and 18)

max.tots <- apply(totals, 1, max)

blanks <- as.data.frame(apply(totals, 2, function(x) max.tots-x))

# # identical(blanks, max.tots-totals)
# prop.blank <- blanks/totals

amend.names <- c("muni.own", "muni.reg", "la.river", "home.rule", "responsible.boards", "meetings.salaries", "terminate.admin", "proportional", "harbor.district", "bonds", "sell.property", "firemen.insure", "police.insure", "no.muni.newspaper", "parks.salaries", "harbor.district.2", "districts", "all.salaries", "dance.halls")

names(yesses) <- paste0('q', seq(1, 19), 'yes')
names(noes) <- paste0('q', seq(1, 19), 'no')
names(blanks) <- paste0('q', seq(1, 19), 'blank')

props <- yesses/(totals+blanks)

names(props) <- amend.names

la.ref.data <- cbind(yesses, noes, blanks)

save(la.ref.data, file="la.ref.data.Rdata")
save(props, file="props.Rdata")