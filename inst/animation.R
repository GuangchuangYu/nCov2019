library(magick)

d <- c(paste0("2020-01-", 19:31), paste0("2020-02-0", 1:7))

out <- lapply(d, function(date){
  p <- plot(y, region='china', chinamap=cn, date=date,
            label=FALSE, continuous_scale=FALSE)  
})

leg <- cowplot::get_legend(out[[length(out)]])
out <- lapply(out, function(g) {
    ggplotify::as.ggplot(g + ggplot2::theme(legend.position="none")) + 
    ggimage::geom_subview(subview = leg, x=.9, y=.2)
})


img <- image_graph(600, 600, res = 96)
invisible(lapply(out, print))
dev.off()

animation <- image_animate(img, fps = 2)
image_write(animation, "ncov2019.gif")