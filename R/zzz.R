.onLoad <- function(libname = find.package("grattan"), pkgname = "grattan"){
  ggplot2::update_geom_defaults("point", list(colour = Orange))  #but cf. col.3
  ggplot2::update_geom_defaults("bar", list(fill = Orange, colour = "black", width = 0.7))
  ggplot2::update_geom_defaults("line", list(fill = Orange, colour = Orange, size = 2))
  if (require(knitr) && require(devEMF)){
      my_emf <- function(file, width, height){
        devEMF::emf(file, width=width, height=height,
                    family = "Arial")
      }
      
      # knitr chunk opts
      # fig.width must = out.width and same for *.height
      knitr::opts_chunk$set(fig.width=11.692, fig.height=8.267, 
                            out.width="11.692in", out.height="8.267in", 
                            fig.show='hide',echo=FALSE,
                            message=FALSE, warning=FALSE
                            ,dev=c('pdf', 'my_emf')
                            ,fig.ext = c("pdf", "emf")
      )
    }
}