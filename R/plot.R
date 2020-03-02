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
                    font.size = 3.8, font.family = "") {
    map <- tibble::as_tibble(chinamap)

    if (x$lang == "zh") {                    
        load(system.file("ncovEnv.rda", package="nCov2019"))
        ncovEnv <- get("ncovEnv")
        setup_city <- get("setup_city", envir = ncovEnv)

        map$NAME <- setup_city(map$NAME)
    }
    map <- do.call('rbind', lapply(region, function(r) {
        stats <- get_city_data(x, r, date)
        code <- sub("(\\d{2}).*", "\\1", 
                  map$ADMINCODE[which(map$NAME == stats[1,1])])
      
        map[grep(paste0("^", code), map$ADMINCODE),]
    }))

    stats <- get_city_data(x, region, date)
    map2 <- dplyr::left_join(map, stats, by='NAME')

    p <- ggplot(map2, aes_(geometry=~geometry)) + 
        theme_minimal() + xlab(NULL) + ylab(NULL) +
        labs(title = '2019nCov', 
            subtitle = paste('confirmed cases:', sum(stats$confirm)),
            caption=paste("accessed date:", time(x))) +
        coord_sf()

    if (continuous_scale) {
        p <- p + geom_sf(aes_(fill=~confirm)) +
            fill_scale_continuous(palette)
    } else {
        map2$confirm2 <- cut(map2$confirm2, discrete_breaks,
                        include.lowest = T, right=F)
        p <- p + geom_sf(aes_(fill=~confirm2)) +
            fill_scale_discrete(palette)
    }       
    if (label) p <- p + geom_sf_text(aes_(label=~NAME), size=font.size, family=font.family)
    return(p)
}    


##' @importFrom ggplot2 map_data
##' @importFrom ggplot2 coord_equal
plot_world <- function(x, continuous_scale=TRUE, palette = "Reds") {
    d <- x['global', ]
    tt <- sum(d$confirm)
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
    world <- map_data('world')
    world <- world[world$region != "Antarctica", ]
    w <- merge(world, d, by.x='region', by.y='name', all.x=T)

    w <- w[order(w$order),]
    p <- ggplot(w, aes_(~long, ~lat)) + 
        coord_equal() +
        theme_minimal(base_size = 14) +
        xlab(NULL) + ylab(NULL) +
        labs(title = '2019nCov', 
            subtitle = paste('confirmed cases:', tt),
            caption=paste("accessed date:", time(x)))

    if (continuous_scale) {
        p1 <- p +  
            geom_map(aes_(~long, ~lat, map_id = ~region, group=~group, fill=~confirm), 
                    map=w, data=w, colour='grey') + 
            fill_scale_continuous(palette)
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
plot_china <- function(x, chinamap, continuous_scale = TRUE, date, palette = "Reds") {
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
    labs(title = '2019nCov', 
       subtitle = paste('confirmed cases:', total),
       caption=paste("accessed date:", tt))

    p + layer_chinamap(x, chinamap, continuous_scale, add_scale=TRUE, date=date, palette = palette)
}



##' @importFrom ggplot2 geom_map
layer_chinamap <- function(x, chinamap, continuous_scale = TRUE, 
                    add_scale=TRUE, date, palette = "Reds") {
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
plot.nCov2019 <- function(x, region="world", chinamap, 
                        continuous_scale = TRUE, label = TRUE, 
                        font.size = 3.8, font.family = "", palette = "Reds", ...) {
    if ("world" %in% region) {
        p <- plot_world(x, continuous_scale = continuous_scale, palette = palette)
        if (missing(chinamap)) {
            return(p)
        } else {
            p <- p + layer_chinamap(x, chinamap, continuous_scale, palette = palette, add_scale=FALSE)
        }
        return(p)
    }

    if ("china" %in% region) {
        p <- plot_china(x, chinamap, continuous_scale, palette = palette, ...)
        if (label) {
            prov.df <- readRDS(system.file("prov_location.rds", package="nCov2019"))
            if (x$lang == "en") {
                prov.df$name <- trans_province(prov.df$name)
            }
            p <- p + geom_text(aes_(~long, ~lat, label=~name), data=prov.df, size=font.size, family=font.family)
        }
        return(p)
    }

    plot_city(x, region = region, chinamap = chinamap, 
            continuous_scale = continuous_scale,
            label = label, palette = palette, ...)
}

##' @method plot nCov2019History
##' @export
plot.nCov2019History <- plot.nCov2019


