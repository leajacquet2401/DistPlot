#' @title
#' Plots of densities
#'
#'@description
#'The function takes 4 arguments and creates 2 plots (a Normal density plot and a Student density plot)
#' @param a
#' is an integer between [-5:5]
#'
#' @param b
#' is an integer between [-5:5]
#'
#' @param mean
#' give the mean for the normal density
#'
#' @param sd
#' give the standard deviation for the normal density
#'
#' @param df
#' give the degree of freedom of the student density
#'
#'@param out
#'is an integer that select the choosen density plot (1= both, 2=normal density, 3=student)
#'
#' @return
#' density plots
#' @examples
#' PlotDens (a=-1, b=1, mean=0, sd=1, df= 1, out=1)
#'
#' @export


PlotDens <- function(a=-1, b = 1, mean=0, sd=1, df=1, out = 1) {
  p1 <- ggplot() + xlim(-5,5)

  theme_set(theme_minimal()+
              theme(legend.position = "top"))

  if (out ==1){

    p <- p1 + stat_function (fun = dnorm,  args = list(mean = mean, sd=sd), geom='area', fill='red',alpha=0.2, xlim=c(a,b)) +
      stat_function (fun = dnorm, args = list(mean = mean, sd=sd), aes(color=paste("Normal (mean = ", mean, "sd=", sd,") \nP(a<X<b) =", round(pnorm(b, mean=mean, sd=sd)-pnorm(a, mean=mean, sd=sd), 3)))) +
      geom_segment (data=NULL, aes(x=a, xend=a, y=0, yend= dnorm(a, mean=mean, sd=sd)), linetype="dashed") +
      geom_segment (data=NULL, aes(x=b, xend=b, y=0, yend=dnorm(b, mean=mean, sd=sd)), linetype='dashed')+
      labs(x="", y="density") +
      annotate(geom = "text", x = -1, y = 0, label = "a") + annotate(geom = "text", x = 1, y = 0, label = "b") +
      stat_function(fun=dt, args=(df=df), geom='area',fill='blue',alpha=0.2, xlim=c(a,b)) +
      stat_function(fun=dt, args=(df=df), aes(color=paste("Student's t-distribution (df = ", df=df, "\nP(a<X<b)=", round(pt(b, df=df) - pt(a, df=df), 3)))) + geom_segment (data=NULL, aes(x=a, xend=a, y=0, yend= dt(a, df=df)), linetype="dashed") +
      geom_segment (data=NULL, aes(x=b, xend=b, y=0, yend=dt(b, df=df)), linetype='dashed')
  }
  if (out == 2){
    p <- p1 + stat_function (fun = dnorm,  args = list(mean = mean, sd=sd), geom='area', fill='red',alpha=0.2, xlim=c(a,b)) +
      stat_function (fun = dnorm, args = list(mean = mean, sd=sd), aes(color=paste("Normal (mean = ", mean, "sd=", sd,") \nP(a<X<b) =", round(pnorm(b, mean=mean, sd=sd)-pnorm(a, mean=mean, sd=sd), 3)))) +
      geom_segment (data=NULL, aes(x=a, xend=a, y=0, yend= dnorm(a, mean=mean, sd=sd)), linetype="dashed") +
      geom_segment (data=NULL, aes(x=b, xend=b, y=0, yend=dnorm(b, mean=mean, sd=sd)), linetype='dashed')+
      labs(x="", y="density") +
      annotate(geom = "text", x = -1, y = 0, label = "a") + annotate(geom = "text", x = 1, y = 0, label = "b")
  }

  if (out ==3){

    p <- p1 + geom_segment (data=NULL, aes(x=a, xend=a, y=0, yend= dt(a, df=df)), linetype="dashed") +
      geom_segment (data=NULL, aes(x=b, xend=b, y=0, yend=dt(b, df=df)), linetype='dashed')+
      labs(x="", y="density") +
      annotate(geom = "text", x = -1, y = 0, label = "a") + annotate(geom = "text", x = 1, y = 0, label = "b") +
      stat_function(fun=dt, args=(df=df), geom='area',fill='blue',alpha=0.2, xlim=c(a,b)) +
      stat_function(fun=dt, args=(df=df), aes(color=paste("Student's t-distribution (df = ", df=df, "\nP(a<X<b)=", round(pt(b, df=df) - pt(a, df=df), 3))))
  }

  return(p)
}
