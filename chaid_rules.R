## package and data
## https://stat.ethz.ch/pipermail/r-help//2015-January/424598.html
library("CHAID")
ucb <- as.data.frame(UCBAdmissions)
ucb <- ucb[rep(1:nrow(ucb), ucb$Freq), 1:3]

## fit tree
ch <- chaid(Admit ~ Gender + Dept, data = ucb)
plot(ch)
print(ch)

## get rule path
partykit:::.list.rules.party(ch)

And with that information it is not too hard to set something up that is 
close to what you want, I think:

format_rules <- function(object, ...) {
   ft <- fitted(object)
   ns <- tapply(ft[[2]], ft[[1]], length)
   pr <- tapply(ft[[2]], ft[[1]], function(y)
     min(prop.table(table(y))))
   lb <- tapply(ft[[2]], ft[[1]], function(y)
     names(sort(table(y), decreasing = TRUE))[1])
   rl <- partykit:::.list.rules.party(object)
   paste0(rl, ": ", lb, " (n = ", ns, ", ", round(100 * pr, 2), "%)")
}
writeLines(format_rules(ch))
