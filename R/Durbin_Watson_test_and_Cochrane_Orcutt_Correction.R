#' durbin_watson_test_and_Cochr_orcutt
#'
#' realiza o teste de durbin watson de autocorrela??o residual e caso ela (autocorr) seja
#' significativa ent?o estima-se novamente a regress?o atrav?s corrigindo a autocorrela??o por
#' meio do m?todo iterativo de cochrane orcutt
#'
#'
#' @export


durbin_watson_test_and_Cochr_orcutt <- function(reg_list,alpha=0.05){

  # Pacotes necess?rios -----------------------------------------------------

  if(!require(lmtest)) stop("Required 'lmtest' package is missing")
  if(!require(orcutt)) stop("Required 'orcutt' package is missing")


  # Objetos a serem populados -----------------------------------------------

  regs_summaries <- list()
  class(regs_summaries) <- "autocor_corrected"

  if(is.null(names(reg_list))){
    dw_test <- data.frame(list_element=paste0('reg_',1:length(reg_list)),
                          dw_Stat=rep(NA,length(reg_list)),
                          p_value=rep(NA,length(reg_list)),
                          rho=rep(NA,length(reg_list)),
                          diagnostic=rep(NA,length(reg_list)))
  } else {
    dw_test <- data.frame(list_element=names(reg_list),
                          dw_Stat=rep(NA,length(reg_list)),
                          p_value=rep(NA,length(reg_list)),
                          rho=rep(NA,length(reg_list)),
                          diagnostic=rep(NA,length(reg_list)))
  }


  # Corpo -------------------------------------------------------------------

  for(i in seq_along(reg_list)){
    dwt <- dwtest(reg_list[[i]],alternative = 'two.sided')

    if(dwt$p.value <= alpha){
      reg_co <- cochrane.orcutt(reg_list[[i]])
      regs_summaries[[i]] <- reg_co$Cochrane.Orcutt

      dw_test[i,2] <- formatC(dwt$statistic,digits = 4,format = 'g')
      dw_test[i,3] <- formatC(dwt$p.value,digits = 4,format = 'g')
      dw_test[i,4] <- formatC(reg_co$rho,digits = 4,format = 'g')
      dw_test[i,5] <- 'Autocorrelation'
    } else {
      regs_summaries[[i]] <- summary(reg_list[[i]])

      dw_test[i,2] <- formatC(dwt$statistic,digits = 4,format = 'g')
      dw_test[i,3] <- formatC(dwt$p.value,digits = 4,format = 'g')
      dw_test[i,4] <- 0
      dw_test[i,5] <- 'No autocorrelation'
    }
    if(!is.null(names(reg_list))){
      names(regs_summaries)[i] <- names(reg_list)[i]
    }
  }
  return(list(DW_test=dw_test,summaries=regs_summaries))
}


##########################
### fun??o para plot da classe "autocor_corrected"
##########################

plot.autocor_corrected <- function(summaries){
  if(!require(ggplot2)) stop("Required 'ggplot2' package is missing")

  if(is.null(names(summaries))){
    for(i in seq_along(summaries)){
      vec_resid <- summaries[[i]]$residuals
      u <- vec_resid[-length(vec_resid)]
      lag_u <- vec_resid[-1]
      plot_data <- data.frame(cbind(u,lag_u))

      print(
        ggplot(plot_data,aes(x=lag_u,y=u))+
          geom_point(alpha=0.6)+
          geom_smooth(method = 'lm',se=F)+
          ggtitle(as.character(i))
      )
    }
  } else {
    list_names <- names(summaries)
    for(name in list_names){
      vec_resid <- summaries[[name]]$residuals
      u <- vec_resid[-length(vec_resid)]
      lag_u <- vec_resid[-1]
      plot_data <- data.frame(cbind(u,lag_u))

      print(
        ggplot(plot_data,aes(x=lag_u,y=u))+
          geom_point(alpha=0.6)+
          ggtitle(name)
      )
    }
  }
}
