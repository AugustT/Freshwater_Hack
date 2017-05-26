my.glmergplot <- function(
  # version 0.43
  # written by tiflo@csli.stanford.edu
  # code contributions from Austin Frank, Ting Qian, and Harald Baayen
  # remaining errors are mine (tiflo@csli.stanford.edu)
  #
  # last modified 12/15/10
  #
  # now also supports linear models
  # backtransforms centering and standardization
  #
  # known bugs:
  #   too simple treatment of random effects
  #
  model,
  name.predictor,
  name.outcome= "outcome",
  predictor= NULL,
  
  # is the predictor centered IN THE MODEL?
  # is the predictor transformed before
  # (centered and) ENTERED INTO THE MODEL?
  predictor.centered= if(!is.null(predictor)) { T } else { F },
  predictor.standardized= F,
  predictor.transform= NULL,
  fun= NULL,
  
  type= "hex",
  main= NA,
  xlab= NA,
  ylab= NA,
  xlim= NA,
  ylim= NA,
  legend.position="right",
  fontsize=16,
  col.line= "#333333",
  col.ci= col.line,
  lwd.line= 1.2,
  lty.line= "solid",
  alpha.ci= 3/10,
  hex.mincnt= 1,
  hex.maxcnt= nrow(model@frame) / 2,
  hex.limits = c(round(hex.mincnt), round(hex.maxcnt)),
  hex.limits2 = c(round(match.fun(hex.trans)(hex.mincnt)), round(match.fun(hex.trans)(hex.maxcnt))),
  hex.midpoint = (max(hex.limits) - (min(hex.limits) - 1)) / 2,
  hex.nbreaks = min(5, round(match.fun(hex.trans)(max(hex.limits)) - match.fun(hex.trans)(min(hex.limits))) + 1),
  hex.breaks = round(seq(min(hex.limits), max(hex.limits), length.out=hex.nbreaks)),
  hex.trans = "log10",
  ...
)
{
  if (!is(model, "mer")) {
    stop("argument should be a mer model object")
  }
  if ((length(grep("^glmer", as.character(model@call))) == 1) &
      (length(grep("binomial", as.character(model@call))) == 1)) {
    model.type = "binomial"
  } else {
    if (length(grep("^lmer", as.character(model@call))) == 1) {
      model.type = "gaussian"
    }
  }
  if (!(model.type %in% c("binomial","gaussian"))) {
    stop("argument should be a glmer binomial or gaussian model object")
  }
  if (!is.na(name.outcome)) {
    if (!is.character(name.outcome))
      stop("name.outcome should be a string\n")
  }
  if (!is.na(xlab[1])) {
    if (!is.character(xlab))
      stop("xlab should be a string\n")
  }
  if (!is.na(ylab)) {
    if (!is.character(ylab))
      stop("ylab should be a string\n")
  }
  # load libaries
  require(lme4)
  require(Design)
  require(ggplot2)
  
  if (predictor.standardized) { predictor.centered = T }
  if (is.null(fun)) {
    if (is.na(ylab)) {
      if (model.type == "binomial") { ylab= paste("Predicted log-odds of", name.outcome) }
      if (model.type == "gaussian") { ylab= paste("Predicted ", name.outcome) }
    }
    fun= I
  } else {
    if (is.na(ylab)) {
      if (model.type == "binomial") { ylab= paste("Predicted probability of", name.outcome) }
      if (model.type == "gaussian") { ylab= paste("Predicted ", name.outcome) }
    }
    fun= match.fun(fun)
  }
  if (!is.null(predictor.transform)) {
    predictor.transform= match.fun(predictor.transform)
  } else { predictor.transform= I }
  
  indexOfPredictor= which(names(model@fixef) == name.predictor)
  
  # get predictor
  if (is.null(predictor)) {
    # simply use values from model matrix X
    predictor= model@X[,indexOfPredictor]
    
    # function for predictor transform
    fun.predictor= I
    
    if (is.na(xlab)) { xlab= name.predictor }
  } else {
    # make sure that only defined cases are included
    predictor = predictor[-na.action(model@frame)]
    
    # function for predictor transform
    trans.pred = predictor.transform(predictor)
    m= mean(trans.pred, na.rm=T)
    rms = sqrt(var(trans.pred, na.rm=T) / (sum(ifelse(is.na(trans.pred),0,1)) - 1))
    fun.predictor <- function(x) {
      x= predictor.transform(x)
      if (predictor.centered == T) { x= x - m }
      if (predictor.standardized == T) { x= x / rms }
      return(x)
    }
    if ((is.na(xlab)) & (label(predictor) != "")) {
      xlab= label(predictor)
    }
  }
  # get outcome for binomial or gaussian model
  if (model.type == "binomial") {
    outcome= fun(qlogis(fitted(model)))
  } else {
    outcome= fun(fitted(model))
  }
  ## calculate grand average but exclude effect to be modeled
  ## (otherwise it will be added in twice!)
  ## random effects are all included, even those for predictor (if any).
  ## should random slope terms for the predictor be excluded?
  ## prediction from fixed effects
  if (ncol(model@X) > 2) {
    Xbeta.hat = model@X[, -indexOfPredictor] %*% model@fixef[-indexOfPredictor]
  } else {
    Xbeta.hat = model@X[, -indexOfPredictor] %*% t(model@fixef[-indexOfPredictor])
  }
  
  ## adjustment from random effects
  Zb = crossprod(model@Zt, model@ranef)@x
  
  ## predicted value using fixed and random effects
  Y.hat = Xbeta.hat + Zb
  
  ## intercept is grand mean of predicted values
  ## (excluding fixed effect of predictor)
  ## (including random effects of predictor, if any)
  int = mean(Y.hat)
  
  # slope
  slope <- fixef(model)[name.predictor]
  
  ## error and confidence intervals
  stderr <- sqrt(diag(vcov(model)))
  names(stderr) <- names(fixef(model))
  slope.se <- stderr[name.predictor]
  lower <- -1.96 * slope.se
  upper <- 1.96 * slope.se
  
  # setting graphical parameters
  if (is.na(ylim)) { ylim= c(min(outcome) - 0.05 * (max(outcome) - min(outcome)), max(outcome) + 0.05 * (max(outcome) - min(outcome)) ) }
  if (is.na(xlim)) { xlim= c(min(predictor) - 0.05 * (max(predictor) - min(predictor)), max(predictor) + - 0.05 * (max(predictor) - min(predictor))) }
  
  print("Printing with ...")
  print(paste("   int=", int))
  print(paste("   slope=", slope))
  print(paste("   centered=", predictor.centered))
  print("   fun:")
  print(fun.predictor)
  
  pdata= data.frame( 	predictor=predictor, outcome=outcome	)
  x= seq(xlim[1], xlim[2], length=1000)
  fit= int + slope * fun.predictor(x)
  ldata= data.frame(
    predictor= x,
    outcome= fun(fit),
    transformed.lower= fun(fit + lower),
    transformed.upper= fun(fit + upper)
  )
  theme_set(theme_grey(base_size=fontsize))
  theme_update(axis.title.y=theme_text(angle=90, face="bold", size=fontsize, hjust=.5, vjust=.5))
  theme_update(axis.title.x=theme_text(angle=0, face="bold", size=fontsize, hjust=.5, vjust=.5))
  p <- ggplot(data=pdata, aes(x=predictor, y=outcome)) +
    xlab(xlab) +
    ylab(ylab) +
    xlim(xlim) +
    ylim(ylim) +
    opts(legend.position=legend.position, aspect.ratio=1)
  
  # for degbugging:
  # panel.lines(rep(mean(x),2), c(min(y),max(y)))
  # panel.lines(c(min(x),max(x)), c(mean(y),mean(y)))
  
  if (type == "points") {
    p <- p + geom_point(alpha=3/10)
  } else if (type == "hex") {
    p <- p + geom_hex(bins = 30) +
      scale_fill_gradient2(low= "lightyellow",
                           mid="orange",
                           high=muted("red"),
                           midpoint= hex.midpoint,
                           space="rgb",
                           name= "Count",
                           limits= hex.limits,
                           breaks= hex.breaks,
                           trans = hex.trans
      )
  }
  p + 	geom_ribbon(data=ldata,
                   aes(	x= predictor,
                        ymin=transformed.lower,
                        ymax=transformed.upper
                   ),
                   fill= col.ci,
                   alpha= alpha.ci
  ) +
    geom_line(data=ldata,
              aes(x= predictor,
                  y=outcome
              ),
              colour= col.line,
              size= lwd.line,
              linetype= lty.line,
              alpha=1
    )
}