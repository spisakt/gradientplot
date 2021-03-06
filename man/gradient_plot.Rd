% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_cereb_grad.R
\name{gradient_plot}
\alias{gradient_plot}
\title{Plot cerebellar activation map as a function of cerebellar gradients}
\usage{
gradient_plot(map, gradient1 = file.path(root_file("data"),
  "wcresult_cerebellumonly_gradient1_suit.nii"),
  gradient2 = file.path(root_file("data"),
  "wcresult_cerebellumonly_gradient2_suit.nii"),
  template = file.path(root_file("data"), "SUIT.nii"), thr = 2.3,
  template.thr = 30, slices = c(20, 25, 30, 35, 40, 45, 50, 55, 60),
  save = F, outbase = "")
}
\arguments{
\item{map}{activation map to be plotted}

\item{gradient1}{first gradient (recommended to use the default)}

\item{gradient2}{second gradient (recommended to use the default)}

\item{template}{template (recommended to use the default)}

\item{thr}{threhold for the activation map}

\item{template.thr}{template threshold (recommended to use the default)}

\item{slices}{slices to save on the SUIT-space plot}

\item{outbase}{filebase, withoput extenson}
}
\description{
The only function in this package is the plotting function 'gradient_plot'. With default parameters it plots activation in the cerebellar gradient-space, as published by Guell et al. (2018, eLife), however it is easy to use it for plotting with other brain gradients (at leasst two). This is a developer version, use with caution.
}
\examples{
map=file.path("example", "spmT_0002_LinearTrend_CSm_gt_baseline.nii")
gradient_plot(map)
gradient_plot(map, save=T, outbase="test")
}
