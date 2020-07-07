isMobile <- function(){
  shiny::tagList(
    shiny::singleton(
      # detect is user are using following devices
      tags$script("
            $(document).on('shiny:sessioninitialized', function(e){
                var isMobile = /((iPhone)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent);
                Shiny.setInputValue('isMobile',isMobile)
            })
                        ")
    )
  )
}