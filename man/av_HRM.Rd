% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/av_fun_GVC.R
\name{av_HRM}
\alias{av_HRM}
\title{av_HRM
Master function in the ecosystem : Function HRM (hypothetical repatriation method)
repat_pct can come from an other function to calibrate
VARIANTs options :
OptVarianteDemande : ALL=Normal ; CIdom=CI domestics ; CIall=all CI (dom+exp) ; DFdom=DF domestics ; DFall=all final demand (dom+exp) ;
OptVariantePaysImp : ALL=Normal ; <country>= normal but 1 country only ; horsUE=substitution outside EU only
We implement a repat_pct % shock on the repat_indus product for the repat_country :
repatriation of repat_pct % of this country's imports of this product.
to repatriate 1 Md??? of production, we set the previous repatriation % on 1/prod
The calculations are transversal to TEI and FD: the entire product is uniformly concerned.}
\usage{
av_HRM(
  dl,
  repat_country,
  repat_indus,
  repat_pct,
  verboseCheck = FALSE,
  OptSommeDFenP3_S14 = TRUE,
  OptVarianteDemande = "ALL",
  OptVariantePaysImp = "ALL",
  OptBaseIntermAvantRecalcProd = TRUE
)
}
\arguments{
\item{dl}{datatable}

\item{repat_country}{text country}

\item{repat_indus}{text industry}

\item{repat_pct}{value percentage}

\item{verboseCheck}{binary}

\item{OptSommeDFenP3_S14}{binary}

\item{OptVarianteDemande}{binary}

\item{OptVariantePaysImp}{binary}

\item{OptBaseIntermAvantRecalcProd}{binary}
}
\value{
list of dl and dw
}
\description{
av_HRM
Master function in the ecosystem : Function HRM (hypothetical repatriation method)
repat_pct can come from an other function to calibrate
VARIANTs options :
OptVarianteDemande : ALL=Normal ; CIdom=CI domestics ; CIall=all CI (dom+exp) ; DFdom=DF domestics ; DFall=all final demand (dom+exp) ;
OptVariantePaysImp : ALL=Normal ; <country>= normal but 1 country only ; horsUE=substitution outside EU only
We implement a repat_pct % shock on the repat_indus product for the repat_country :
repatriation of repat_pct % of this country's imports of this product.
to repatriate 1 Md??? of production, we set the previous repatriation % on 1/prod
The calculations are transversal to TEI and FD: the entire product is uniformly concerned.
}
