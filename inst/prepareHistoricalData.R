x <- read.csv("nCov2019-data.csv", stringsAsFactors = F)
names(x)
colnames(x)[1:6] <- c("province", "city", "time", 
                      "cum_confirm", "cum_heal", "cum_dead")
colnames(x)[9:11] <- c("confirm", "dead", "heal")

x$time = as.Date(sub("(\\d+)\\D+(\\d+)\\D+", "\\1-\\2", 
                   x$time), "%m-%d")
x$province <- sub("\\s*", "", x$province)
object <- structure(list(data = x,
                         time = x$time[1]
                         ), class = "nCov2019History")
saveRDS(object, file="nCov2019History.rds")
object
