##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 coord_sf
##' @importFrom ggplot2 geom_sf
##' @importFrom ggplot2 geom_sf_text
##' @importFrom tibble as_tibble
plot_city <- function(x, region, chinamap, 
                    continuous_scale=TRUE, label=TRUE, date, palette = "Reds",
                    font.size = 3.8, font.family = "", title) {
    map <- tibble::as_tibble(chinamap)

    if (x$lang == "zh") {                    
        load(system.file("ncovEnv.rda", package="nCov2019"))
        ncovEnv <- get("ncovEnv")
        setup_city <- get("setup_city", envir = ncovEnv)

        map$NAME <- setup_city(map$NAME)
    }
    map <- do.call('rbind', lapply(region, function(r) {
        stats <- get_city_data(x, r, date)
        citis <- stats$NAME
        citis <- citis[!is.na(citis)]
        citis <- citis[citis %in% map$NAME]
        code <- sub("(\\d{2}).*", "\\1", 
                  map$ADMINCODE[which(map$NAME == citis[1])])
      
        map[grep(paste0("^", code), map$ADMINCODE),]
    }))

    stats <- get_city_data(x, region, date)
    map2 <- dplyr::left_join(map, stats, by='NAME')

    if (!continuous_scale) {
        map2$confirm2 <- cut(map2$confirm, discrete_breaks,
                             include.lowest = T, right=F)
    }

    if (continuous_scale) {
        p <- ggplot(map2, aes_(geometry=~geometry)) +
            geom_sf(aes_(fill=~confirm)) +
            fill_scale_continuous(palette)
    } else {
        p <- ggplot(map2, aes_(geometry=~geometry)) +
            geom_sf(aes_(fill=~confirm2)) +
            fill_scale_discrete(palette)
    }

    p <-p + 
        theme_minimal() + xlab(NULL) + ylab(NULL) +
        labs(title = title, 
             subtitle = paste('confirmed cases:', sum(stats$confirm)),
             caption=paste("accessed date:", time(x))) +
        coord_sf()

    if (label) p <- p + geom_sf_text(aes_(label=~NAME), size=font.size, family=font.family)
    return(p)
}    


##' @importFrom ggplot2 map_data
##' @importFrom ggplot2 coord_equal
plot_world <- function(x, region = "world", continuous_scale=TRUE, palette = "Reds", date, title) {
    if (!missing(date)) {
        tt <- date
    } else {
        tt <- time(x)
    }

    d <- x['global', ]
    if (is(x, "nCov2019History")) {
        d <- subset(d, time == tt)
    }

    names(d) <- sub("cum_", "", names(d))
    nn <- names(d)
    names(d)[nn == "country"] <- "name"

    if (region == "world") {
        total <- sum(d$confirm)                
    } else {
        total <- sum(d$confirm[d$name %in% region])        
    }


    if (x$lang == "zh") {
        nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
        d$name <- nn[as.character(d$name)]
    }
    if ('Taiwan' %in% d$name) {
        TW <- d[which(d$name == 'Taiwan'), "confirm"]
        CN <- d[which(d$name == 'China'), "confirm"]
        d[which(d$name == 'Taiwan'), "confirm"] <- TW + CN
        d[which(d$name == 'China'), "confirm"] <- TW + CN
    } else {
        TW <- d[which(d$name == 'China'),]
        TW$name <- 'Taiwan'
        d <- rbind(d,TW)
    }
    d$name <- sub("United\\sStates.*", "USA", d$name)
    d$name <- sub("Republic\\sof\\sKorea", "South Korea", d$name)
    d$name <- sub("United\\sKingdom.*", "UK", d$name)
    d$name <- sub("Republika\\sSeverna\\sMakedonija", "Macedonia", d$name)
    if (region == "world") region <- "."
    world <- map_data('world', region = region)
    world <- world[world$region != "Antarctica", ]
    w <- merge(world, d, by.x='region', by.y='name', all.x=T)

    w <- w[order(w$order),]
    p <- ggplot(w, aes_(~long, ~lat)) + 
        coord_equal() +
        theme_minimal(base_size = 14) +
        xlab(NULL) + ylab(NULL) +
        labs(title = title, 
            subtitle = paste('confirmed cases:', total),
            caption=paste("accessed date:", tt))

    if (continuous_scale) {
        if (length(unique(w$confirm)) == 1) {
            col <- RColorBrewer::brewer.pal(3, palette)[3]
            p1 <- p +  
                geom_map(aes_(~long, ~lat, map_id = ~region, group=~group), 
                         map=w, data=w, colour='grey', fill = col)
        } else {
            p1 <- p +  
                geom_map(aes_(~long, ~lat, map_id = ~region, group=~group, fill=~confirm), 
                         map=w, data=w, colour='grey')  +
                fill_scale_continuous(palette)
        }
    } else {
        w$confirm2 = cut(w$confirm, discrete_breaks,
                 include.lowest = T, right=F)
        p1 <- p + 
            geom_map(aes_(~long, ~lat, map_id=~region, group=~group, fill=~confirm2), 
                    map=w, data=w, colour='grey') + 
            fill_scale_discrete(palette)
    }            
    return(p1)
}

##' @importFrom ggplot2 coord_map
plot_china <- function(x, chinamap, continuous_scale = TRUE, date, palette = "Reds", title) {
  if (!missing(date)) {
      tt <- date
  } else {
      tt <- time(x)
  }

    if (is(x, "nCov2019")) {
        total <- x$chinaTotal$confirm
    } else if (is(x, "nCov2019History")) {
        total <- sum(extract_history(x, date = date)$confirm)
    } else {
        stop("object not supported...")
    }

  p <- ggplot() + coord_map() +
    theme_minimal() +
    xlab(NULL) + ylab(NULL) +
    labs(title = title, 
       subtitle = paste('confirmed cases:', total),
       caption=paste("accessed date:", tt))

    p + layer_chinamap(x, chinamap, continuous_scale, add_scale=TRUE, date=date, palette = palette)
}



##' @importFrom ggplot2 geom_map
layer_chinamap <- function(x, chinamap, continuous_scale = TRUE, 
                    add_scale=TRUE, date, palette = "Reds", title) {
    if (is(x, "nCov2019")) {
        df <- x[]
    } else if (is(x, "nCov2019History")) {
        df <- extract_history(x, date = date)
    } else {
        stop("object not supported...")
    }

    cn <- chinamap

    if (x$lang == "zh") {
        load(system.file("ncovEnv.rda", package="nCov2019"))
        ncovEnv <- get("ncovEnv")
        setup_province <- get("setup_province", envir = ncovEnv)
        cn$province <- setup_province(cn$province)
    }

    cn2 <- merge(cn, df, by.x='province', by.y='name', all.x=TRUE)
    cn2 <- cn2[order(cn2$order),]

    if (continuous_scale) {
        ly <- list(geom_map(aes_(x=~long, y=~lat, map_id=~id, group=~group, fill=~confirm), 
                        map=cn2, data=cn2, colour='grey'),
                    fill_scale_continuous(palette)     
                )             
    } else {
        cn2$confirm2 <- cut(cn2$confirm, discrete_breaks,
                            include.lowest = T, right=F)
        ly <- list(
                geom_map(aes_(~long, ~lat, map_id=~id, group=~group, 
                        fill=~confirm2), 
                    map=cn2, data=cn2, colour='grey'), 
                fill_scale_discrete(palette)               
            )
    }
    if (!add_scale) ly[[2]] = NULL
    return(ly) 
}

##' @importFrom methods is
##' @importFrom ggplot2 geom_text
##' @method plot nCov2019
##' @export
plot.nCov2019 <- function(x, region="world", chinamap = NULL, 
                        continuous_scale = TRUE, label = TRUE, 
                        font.size = 3.8, font.family = "", palette = "Reds", title = "COVID-19", ...) {
    if ("world" %in% region || is.null(chinamap)) {
        p <- plot_world(x, region = region, continuous_scale = continuous_scale,
                        palette = palette, title = title, ...)
        if (is.null(chinamap)) {
            return(p)
        } else {
            p <- p + layer_chinamap(x, chinamap, continuous_scale,
                                    palette = palette, add_scale=FALSE, title = title, ...)
        }
        return(p)
    }

    if ("china" %in% region) {
        p <- plot_china(x, chinamap, continuous_scale, palette = palette, title = title, ...)
        if (label) {
            prov.df <- readRDS(system.file("prov_location.rds", package="nCov2019"))
            if (x$lang == "en") {
                prov.df$name <- trans_province(prov.df$name)
            }
            p <- p + geom_text(aes_(~long, ~lat, label=~name),
                               data=prov.df, size=font.size, family=font.family)
        }
        return(p)
    }

    plot_city(x, region = region, chinamap = chinamap, 
            continuous_scale = continuous_scale,
            label = label, palette = palette, title = title, ...)
}

##' @method plot nCov2019History
## @param from start date to plot
## @param to end date to plot. Both from and to should be specify, otherwise they will be ignored.
## If both from and to are specify, an animation will be created.
## @param width width of the plot, only works for animation
## @param height height of the plot, only works for animation
##' @export
plot.nCov2019History <- function(x, region="world", chinamap = NULL, 
                                 continuous_scale = TRUE, label = TRUE, 
                                 font.size = 3.8, font.family = "", palette = "Reds",
                                 from = NULL, to = NULL, width = 600, height = 600, filename = "nCov2019.gif", ...) {
    if (is.null(from) || is.null(to)) {
        p <- plot.nCov2019(x = x,
                           region = region,
                           chinamap,
                           continuous_scale = continuous_scale,
                           label = label,
                           font.size = font.size,
                           font.family = font.family,
                           palette = palette, ...)
        return(p)
    }

    from <- as.Date(from)
    to <- as.Date(to)
    d <- seq(from, to, by = 1)

    out <- lapply(d, function(date){
        p <- plot.nCov2019(x = x,
                           region = region,
                           chinamap,
                           continuous_scale = continuous_scale,
                           label = label,
                           font.size = font.size,
                           font.family = font.family,
                           palette = palette, date = date, ...)
    })

    leg <- cowplot::get_legend(out[[length(out)]])
    out <- lapply(out, function(g) {
        ## ggplotify::as.ggplot(g + ggplot2::theme(legend.position="none")) + 
        ##     ggimage::geom_subview(subview = leg, x=.9, y=.2)
        cowplot::plot_grid(g + ggplot2::theme(legend.position="none"),
                           leg, rel_widths = c(1, .3))
    })


    img <- magick::image_graph(600, 600, res = 96)
    invisible(lapply(out, function(p) suppressWarnings(print(p))))
    grDevices::dev.off()

    animation <- magick::image_animate(img, fps = 2)
    msg <- paste0("A gif, ", filename, ", was generated in current directory\n")
    message(msg)
    magick::image_write(animation, filename)
    invisible(animation)
}



