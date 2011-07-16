
slider.mts <- function(x,...)
{
  require(tcltk)

  tt <- tktoplevel()
  tkwm.title(tt, "Ensembles")

  ##

  LeftClick <- function(){
    ref <- as.numeric(tclvalue(ens))
    plot(x[,ref],...)
  }
  tkbind(tt, "<Button-1>", LeftClick)

  ##

  ens <- tclVar("1")
  SliderValueLabel <- tklabel(tt, text=as.character(tclvalue(ens)))
  tkgrid(tklabel(tt,text="Ensemble:"), SliderValueLabel)
  tkconfigure(SliderValueLabel, textvariable=ens)
  #slider <- tkscale(tt, command=LeftClick, from=1, to=100, showvalue=FALSE,
  #  variable=ens, resolution=1, orient="horizontal")
  slider <- tkscale(tt, from=1, to=ncol(x), showvalue=FALSE,
    variable=ens, resolution=1, orient="horizontal")

  tkgrid(slider)
  tkfocus(tt)

  ##

  ref <- as.numeric(tclvalue(ens))
  plot(x[,1],...)
}
