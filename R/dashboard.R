##' @title Shiny app
##' @rdname dashboard
##' @description a shiny app
##' @param lang Your languate, it should be one of "auto", "zh" or "en"
##' @param remote If TRUE, open the online version
##' @importFrom downloader download
##' @export
open_dashboard <- function(lang="auto", remote=FALSE) {
    
    lang <- which_lang(lang) #zh or en
    if (remote) {
        if (lang == 'zh') {
            utils::browseURL('http://www.bcloud.org/v/')
        } else {
            utils::browseURL('http://www.bcloud.org/e/')
        }
    } else {
        package_need <- c('forcats', 'forecast', 'ggrepel', 'lubridate', 'mapproj',
              'pinyin', 'plotly', 'sf', 'shiny', 'shinyBS', 'sp', 'tidyr',
              'graphics', "curl")
        
        package_no <-  package_need[!is.installed(package_need)]
        chinamap_if <- !is.installed("chinamap")
        
        if(length(package_no) != 0 || chinamap_if) {
            p1 <- paste0(package_no,"\t")
            p2 <- ifelse(chinamap_if, "chinamap", "")
            pa <- paste(p1, p2)
            messages2 <- "Running this shiny app requires some additional R packages, download them? (Y/N): "
            ## button2 <- tcltk::tkmessageBox(title='Message', message=messages2, type='yesno')
            ## button2 <- tcltk::tclvalue(button2)
            button2 <- toupper(readline(prompt = messages2))
            if(button2 == 'N'){
                stop("Running this shiny app requires some additional R packages,",
                    pa, ",please install them")
                } else {
                message("Running this shiny app requires some additional R packages,",
                    pa, ",this will take some time")
                # install packages from CRAN
                ## sapply(package_no, utils::install.packages)
                utils::install.packages(package_no)
                # install chinamap from github
                if(chinamap_if) {
                    if(!is.installed("remotes"))
                        utils::install.packages("remotes")
        
                    remotes::install_github("GuangchuangYu/chinamap")
                }
            }
         }
        ## fpath = system.file(package="nCov2019")
        ## cn_city_map <- file.path(system.file(package="nCov2019"), "cn_city_map.rds")

        rds <- 'cn_city_map.rds'
        if (!file.exists(rds)) {
            messages <- "cn_city_map.rds is needed to draw city maps for province,
                this may take a few minutes, download it? (Y/N): "
            ## button <- tcltk::tkmessageBox(title='Message', message=messages, type='yesno')
            ## button <- tcltk::tclvalue(button)
            button <- toupper(readline(prompt = messages))

            if(button == 'Y'){
                ## rds <- tempfile(pattern=".rds")
                url <- 'http://q6k78o1u4.bkt.clouddn.com/cn_city_map.rds'
                if(check_network(url) == 200){
                downloader::download(url, destfile = rds, quiet = FALSE)
                } else {
                    # using github url
                    url <- 'https://raw.githubusercontent.com/GuangchuangYu/map_data/master/cn_city_map.rds'
                    downloader::download(url, destfile = rds, quiet = FALSE)
                }
            }
        } 

        shijie <- readRDS(rds)
        pos <- 1
        envir <- as.environment(pos)
        if (!exists("nCov2019Env", envir = .GlobalEnv)) {
            assign("nCov2019Env", new.env(), envir = envir)
        }
        nCov2019Env <- get("nCov2019Env", envir = .GlobalEnv)
        assign("shijie", shijie, envir = nCov2019Env)

        options(nCov2019_dashboard = TRUE)

    # run shinyApp
    
        if(lang == 'zh'){
            shiny::runApp(appDir = system.file("shinyapps", "v" ,package="nCov2019"))
        } else {
            shiny::runApp(appDir = system.file("shinyapps", "e" ,package="nCov2019"))
        }


    }
}

##' @rdname dashboard
##' @export
dashboard <- open_dashboard

is.installed <- function(packages) {
    vapply(packages, function(pkg) {
        system.file(package = pkg) != ""
    }, logical(1))
}




