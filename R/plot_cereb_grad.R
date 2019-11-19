require(oro.nifti)
require(neurobase)
# costumize ggplot
require("ggplot2");
theme_set(theme_bw()+theme_linedraw()
          +theme( panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.text.x=element_text(angle=90,hjust=1))
          +theme(strip.background = element_rect(fill="white"), strip.text = element_text(color="black") )
)
require("ggrepel")

script.dir <- rprojroot::is_r_package

#' Plot cerebellar activation map as a function of cerebellar gradients,
#' as published by Guel et al., 
#'
#' @export
#'
#' @param map activation map to be plotted
#' @param gradient1 first gradient (recommended to use the default)
#' @param gradient2 second gradient (recommended to use the default)
#' @param template template (recommended to use the default)
#' @param thr threhold for the activation map
#' @param template.thr template threshold (recommended to use the default)
#' @param slices slices to save on the SUIT-space plot
#' @param outbase filebase, withoput extenson
#' @examples 
#' map=file.path("example", "spmT_0002_LinearTrend_CSm_gt_baseline.nii")
#' gradient_plot(map)
#' gradient_plot(map, save=T, outbase="test")
gradient_plot=function(map,
                       gradient1=file.path(script.dir, "..",
                                           "data", "wcresult_cerebellumonly_gradient1_suit.nii"),
                      gradient2=file.path(script.dir, "..",
                                           "data", "wcresult_cerebellumonly_gradient2_suit.nii"),
                      template=file.path(script.dir, "..",
                                         "data", "SUIT.nii"),
                      thr=2.3,
                      template.thr=30,
                      slices=c(20, 25, 30, 35, 40, 45, 50, 55, 60),
                      save=F,
                      outbase=""
                      )
{
  print(script.dir)
  if (outbase=="")
  {
    outbase=tools::file_path_sans_ext(basename(map))
  }
  out.suit=paste0(outbase, "_suitspace.pdf")
  out.grad=paste0(outbase, "_gradientspace.pdf")
  
  # load volumes
  map=oro.nifti::readNIfTI(map)
  gradient1=oro.nifti::readNIfTI(gradient1)
  gradient2=oro.nifti::readNIfTI(gradient2)
  template=oro.nifti::readNIfTI(template)
  
  # check if lattice is ok
  if (any(dim(map)!=dim(template)))
  {
    stop("Dimensions of 'map' are not compatoible with the SUIT template!")
  }
  #pb <- txtProgressBar(style = 3)
  
  if (save==T)
  {
    pdf(out.suit)
  }  
  map_thr=map
  map_thr[map_thr<thr]=0
  template_thr=template
  template_thr[template_thr<template.thr]=0
  overlay(template_thr, map_thr, bg="white", plot.type="single", z=slices, NA.x=T, NA.y=T)
  if (save==T)
  {
    dev.off() 
  }

  # apply over all axes
  result=apply(which(template>template.thr, arr.ind = T), 1, function(coord){
    list(g1=gradient1[coord[1], coord[2], coord[3] ],
         g2=gradient2[coord[1], coord[2], coord[3] ],
         value=map[coord[1], coord[2], coord[3] ]
      )
    })
  df =  as.data.frame(do.call(rbind, result))
  df$g1=unlist(df$g1) # I have no clue why this is needed
  df$g2=unlist(df$g2)
  df$value=unlist(df$value)
  
  df <- df[order(df$value),] 
  
  maxval=max(df$value)
  minval=min(df$value)
  range=maxval-minval
 


  p=ggplot(df, aes(g2, g1))+geom_point(color="azure3", alpha=0.1)+
      geom_point(data = df[df$value>thr,], aes(g2, g1, color=value, alpha=0.5+0.5*value/maxval))+
      scale_alpha(guide="none") + xlab("Gradient2")+ylab("Gradient1")+
      scale_color_gradientn(colors=hotmetal(round(range*100))[(thr*100):(round(range*100))]) #to mach color to the brain plots
  print(p)
  if (save==T)
  {
    ggsave(out.grad, p, device = "jpeg", width=6, hight=6)
  }
  else
  {
    print(p)
  }
  
  invisible(df)
}