open_dashboard <- function(lang="auto", remote=FALSE) {
    lang <- which_lang(lang) #zh or en
    if (remote) {
        if (lang = 'zh') {
            utils::browseURL('http://www.bcloud.org/v/')
        } else {
            utils::browseURL('http://www.bcloud.org/e/')
        }
    } else {
        # run shinyApp
    }
}