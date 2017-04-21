
dietPlot = function(data, species, FUN=mean, horiz=TRUE,
                    col=NULL, ylab="%IRI", xlab="Species") {

  FUN = match.fun(FUN)

  if(isTRUE(horiz)) {
    xlab = ylab
    ylab = NULL
  }

  mar   = if(isTRUE(horiz)) c(6,12,2,2) else c(4,4,2,8)
  xpos  = if(isTRUE(horiz)) "bottom" else "right"
  inset = if(isTRUE(horiz)) -0.6 else -0.4
  las   = if(isTRUE(horiz)) 1 else 1
  oma   = 0*c(1,1,1,1)

  fmla = as.formula(sprintf(". ~ %s", species))
  res = aggregate(fmla, data=data, FUN=FUN)
  n = as.numeric(aggregate(fmla, data=data, FUN=length)[,2])
  diet = as.matrix(res[,-1])
  rownames(diet) = as.character(res[,1])
  diet = t(diet)

  ncol  = if(isTRUE(horiz)) ceiling(nrow(diet)/2) else 1
  if(is.null(col)) col = seq(nrow(diet))

  # spp = sprintf("italic('%s') (n = %d)", colnames(diet), n)
  # spp = as.expression(spp)
  spp1 = sprintf("%s (n = %d)", colnames(diet), n)
  spp2 = sprintf("%s\n(n = %d)", colnames(diet), n)

  spp = if(isTRUE(horiz)) spp1 else spp2

  par(mar=mar, oma=oma)
  barplot(diet, names.arg=spp, col=col, legend = TRUE,
          args.legend = list(bty="n", x=xpos,
                             inset=inset, ncol=ncol, xjust=0.1),
          horiz=horiz, axes=FALSE, xlab=xlab, ylab=ylab,
          las=1)

  axP = if(isTRUE(horiz)) 1 else 2
  ax = axTicks(axP)
  axis(axP, at = ax, labels = sprintf("%d%%", ax), las=1)

  return(invisible())
}

