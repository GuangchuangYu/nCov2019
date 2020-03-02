cn = get_map_china()
x = load_nCov2019(lang="zh")
plot(x, region='china', chinamap=cn, date = '2020-2-10',
     continuous_scale=FALSE,
     palette='Blues')

library(magick)

d <- paste0("2020-01-", seq(19,31,3))
d <- c(d, paste0("2020-02-0", seq(1,27,3))) 

img <- image_graph(600, 450, res = 96)
out <- lapply(d, function(date){
  p <- plot(x, region='china', chinamap=cn, date=date,
            label=FALSE, continuous_scale=FALSE)
  print(p)
})
dev.off()

animation <- image_animate(img, fps = 1)
image_write(animation, "ChinaMapAnimated2.gif")
