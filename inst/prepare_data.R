x <- read.csv("nCov2019-data.csv", stringsAsFactors = F)
names(x)
head(x, 2)

colnames(x)[1:6] <- c("province", "city", "time", 
                      "cum_confirm", "cum_heal", "cum_dead")
colnames(x)[9:11] <- c("confirm", "dead", "heal")

#x$time = as.Date(sub("(\\d+)\\D+(\\d+)\\D+", "\\1-\\2", 
#                   x$time), "%m-%d")
x$time <- as.Date(x$time, "%Y/%m/%d")
head(x$time)

x$province <- gsub("\\s+", "", x$province)
which(grepl("\\s+", x$province))
head(x$province)
#x$province <- sub("\\s*$", "", x$province)
object <- structure(list(data = x,
                         time = x$time[1]
                         ), class = "nCov2019History")
saveRDS(object, file="nCov2019/inst/nCov2019History.rds")
object


################# province data

txt <- "'ɽ��':[117.000923, 36.675807],
'�ӱ�':[115.48333,38.03333],
'����':[125.35000,43.88333],
'������':[127.63333,47.75000],
'����':[123.38333,41.80000],
'���ɹ�':[111.670801, 41.818311],
'�½�':[87.68333,43.76667],
'����':[103.73333,36.03333],
'����':[106.26667,37.46667],
'ɽ��':[112.53333,37.86667],
'����':[108.95000,34.26667],
'����':[113.65000,34.76667],
'����':[117.283042, 31.86119],
'����':[119.78333,32.05000],
'�㽭':[120.20000,30.26667],
'����':[118.30000,26.08333],
'�㶫':[113.23333,23.16667],
'����':[115.90000,28.68333],
'����':[110.35000,20.01667],
'����':[108.320004, 22.82402],
'����':[106.71667,26.56667],
'����':[113.00000,28.21667],
'����':[114.298572, 30.584355],
'�Ĵ�':[104.06667,30.66667],
'����':[102.73333,25.05000],
'����':[91.00000,30.60000],
'�ຣ':[96.75000,36.56667],
'���':[117.20000,39.13333],
'�Ϻ�':[121.55333,31.20000],
'����':[106.45000, 29.56667],
'����': [116.41667,39.91667],
'̨��': [121.30, 25.03],
'���': [114.10000,22.20000],
'����': [113.50000,22.20000]"

pro <- readLines(textConnection(txt))

prov.df <- data.frame(
  name = sub("\\'(.*)\\'.*", "\\1", pro),
  long = as.numeric(sub(".*\\[([0-9\\.]+),.*", "\\1", pro)),
  lat = as.numeric(sub(".*,\\s*([0-9\\.]+)\\].*", "\\1", pro))
)
head(prov.df)
saveRDS(prov.df, file = "prov_location.rds")


####################### country data

require(rvest)

url <- 'http://www.fltacn.com/article_392.html'

n = url %>%
  xml2::read_html()

cc = html_nodes(n, 'table')[[1]] %>% html_table()
cc = cc[-1, ]
nn = cc[,3]
names(nn) = cc[,2]
head(nn)
nn <- c(nn, '������' = "United Arab Emirates")

saveRDS(nn, file="country_translate.rds")