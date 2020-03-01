##' @title Shiny app
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
        package_if <- vapply(package_need,
            function(x) system.file(package=x) == "", logical(1))
        
        package_no <-  package_need[package_if]
        
        # chinamap should install from github
        chinamap_if <- system.file(package="chinamap") == ""
        
        if(length(package_no) != 0 || chinamap_if) {
            p1 <- paste0(package_no,"\t")
            p2 <- ifelse(chinamap_if, "chinamap", "")
            pa <- paste(p1, p2)
            messages2 <- "Running this shiny app requires some additional R packages, download them?"
            button2 <- tcltk::tkmessageBox(title='Message', message=messages2, type='yesno')
            button2 <- tcltk::tclvalue(button2)
            if(button2 == 'no'){
                stop("Running this shiny app requires some additional R packages,",
                    pa, ",please install them")
                } else {
                message("Running this shiny app requires some additional R packages,",
                    pa, ",this will take some time")
                # install packages from CRAN
                sapply(package_no, utils::install.packages)
                # install chinamap from github
                if(system.file(package="chinamap") == "") {
                    if(system.file(package="remotes") == "")
                        utils::install.packages("remotes")
        
                remotes::install_github("GuangchuangYu/chinamap")
                }
            }
         }
               
        if (!file.exists('cn_city_map.rds')) {
            messages <- "cn_city_map.rds is needed to draw city maps in the province,
                this may take a long tiem, download it?"
            button <- tcltk::tkmessageBox(title='Message', message=messages, type='yesno')
            button <- tcltk::tclvalue(button)
            if(button == 'yes'){
                cn_city_map.rds <- tempfile(pattern=".rds")
                url = 'https://raw.githubusercontent.com/GuangchuangYu/map_data/master/cn_city_map.rds'
                download(url, destfile = cn_city_map.rds, quiet = TRUE)
                shijie <- readRDS('cn_city_map.rds')
            }
        } else {
            shijie <- readRDS('cn_city_map.rds')
        }
        
    # run shinyApp
    
        if(lang == 'zh'){
            shiny::runApp(appDir = system.file("shinyapps", "v" ,package="nCov2019"))
        } else {
            shiny::runApp(appDir = system.file("shinyapps", "e" ,package="nCov2019"))
        }


    }
}






