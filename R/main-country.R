country_list <- c("China","South Korea","United States","Japan","Iran",
                  "Italy","Germany","United Kingdom")

nCov2019_set_country <- function(country = "China") {
    if (!country %in% country_list) {
        msg <- paste("only the following supported countries have detail information:\n",
                     paste(country_list, collapse=","), "\n",
                     "Please use x['global'] to access information at country level\n")
        stop(msg)
    }

    options(nCov2019.country = country)

    ## in `[` and `summary` method definitions
    ## use `getOption("nCov2019.country")` to get the country setting
    ## and access corresponding detail of selected country and province
}
