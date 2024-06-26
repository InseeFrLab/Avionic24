% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/av_fun_GVC.R
\name{av_SPA}
\alias{av_SPA}
\title{av_SPA
Master function in the environment : Structural Path Analysis (SPA)
 Function for calculating the Stuctural Path Analysis (SPA) of a MRIO
 The MRIO can be in long format or it has already undergone a CompoMRIO (dt)
 TypeSPA="VA" or "Emi"
 ListThres = {GenThres=,Thres_L2_1=,Thres_L3_1=,Thres_L3_2=,Thres_L3_3,Thres_L3_4=,Thres_L4_1=,Thres_L4_2=,Thres_L4_3,Thres_L4_4=,Thres_L5_1=,Thres_L5_2=,Thres_L5_3,Thres_L5_4=,Thres_L6_1=,Thres_L6_2=,Thres_L6_3,Thres_L6_4=,Thres_L7_1=,Thres_L7_2=,Thres_L7_3,Thres_L7_4=}
 Fitted FIGARO VA : ListThres = {GenThres=0.001,Thres_L2_1=0.001,Thres_L3_1=0.001,Thres_L3_2=0.001,Thres_L3_3=0.00001,Thres_L3_4=0.001,Thres_L4_1=0.001,Thres_L4_2=0.001,Thres_L4_3=0.000005,Thres_L4_4=0.001,Thres_L5_1=0.001,Thres_L5_2=0.001,Thres_L5_3=0.000001,Thres_L5_4=0.001,Thres_L6_1=0.001,Thres_L6_2=0.001,Thres_L6_3=0.0000005,Thres_L6_4=0.001,Thres_L7_1=0.001,Thres_L7_2=0.001,Thres_L7_3=0.0000001,Thres_L7_4=0.001}
 Fitted FIGARO Emi : ListThres = {GenThres=0.001,Thres_L2_1=0.0001,Thres_L3_1=0.0001,Thres_L3_2=0.00001,Thres_L3_3=0.000005,Thres_L3_4=0.0001,Thres_L4_1=0.0001,Thres_L4_2=0.00001,Thres_L4_3=0.000005,Thres_L4_4=0.00001,Thres_L5_1=0.001,Thres_L5_2=0.0001,Thres_L5_3=0.000004,Thres_L5_4=0.000001,Thres_L6_1=0.0001,Thres_L6_2=0.00001,Thres_L6_3=0.000004,Thres_L6_4=0.0000001,Thres_L7_1=0.0005,Thres_L7_2=0.00005,Thres_L7_3=0.0000005,Thres_L7_4=0.001}}
\usage{
av_SPA(
  dtdl,
  Optdl = FALSE,
  TypeSPA = "VA",
  TypeMRIO = "FIGARO",
  ListThres,
  TargetCountry = "FRA",
  OptRDS = "",
  OptRDSDetail = "",
  OptUE27 = FALSE,
  verbose = FALSE,
  PathEmi = ""
)
}
\arguments{
\item{dtdl}{datatable}

\item{Optdl}{binary}

\item{TypeSPA}{text options}

\item{TypeMRIO}{text}

\item{ListThres}{list thresholds}

\item{TargetCountry}{text country}

\item{OptRDS}{binary}

\item{OptRDSDetail}{binary}

\item{OptUE27}{binary}

\item{verbose}{binary}

\item{PathEmi}{text data link}
}
\value{
dt data table with path analysis results
}
\description{
av_SPA
Master function in the environment : Structural Path Analysis (SPA)
 Function for calculating the Stuctural Path Analysis (SPA) of a MRIO
 The MRIO can be in long format or it has already undergone a CompoMRIO (dt)
 TypeSPA="VA" or "Emi"
 ListThres = {GenThres=,Thres_L2_1=,Thres_L3_1=,Thres_L3_2=,Thres_L3_3,Thres_L3_4=,Thres_L4_1=,Thres_L4_2=,Thres_L4_3,Thres_L4_4=,Thres_L5_1=,Thres_L5_2=,Thres_L5_3,Thres_L5_4=,Thres_L6_1=,Thres_L6_2=,Thres_L6_3,Thres_L6_4=,Thres_L7_1=,Thres_L7_2=,Thres_L7_3,Thres_L7_4=}
 Fitted FIGARO VA : ListThres = {GenThres=0.001,Thres_L2_1=0.001,Thres_L3_1=0.001,Thres_L3_2=0.001,Thres_L3_3=0.00001,Thres_L3_4=0.001,Thres_L4_1=0.001,Thres_L4_2=0.001,Thres_L4_3=0.000005,Thres_L4_4=0.001,Thres_L5_1=0.001,Thres_L5_2=0.001,Thres_L5_3=0.000001,Thres_L5_4=0.001,Thres_L6_1=0.001,Thres_L6_2=0.001,Thres_L6_3=0.0000005,Thres_L6_4=0.001,Thres_L7_1=0.001,Thres_L7_2=0.001,Thres_L7_3=0.0000001,Thres_L7_4=0.001}
 Fitted FIGARO Emi : ListThres = {GenThres=0.001,Thres_L2_1=0.0001,Thres_L3_1=0.0001,Thres_L3_2=0.00001,Thres_L3_3=0.000005,Thres_L3_4=0.0001,Thres_L4_1=0.0001,Thres_L4_2=0.00001,Thres_L4_3=0.000005,Thres_L4_4=0.00001,Thres_L5_1=0.001,Thres_L5_2=0.0001,Thres_L5_3=0.000004,Thres_L5_4=0.000001,Thres_L6_1=0.0001,Thres_L6_2=0.00001,Thres_L6_3=0.000004,Thres_L6_4=0.0000001,Thres_L7_1=0.0005,Thres_L7_2=0.00005,Thres_L7_3=0.0000005,Thres_L7_4=0.001}
}
\examples{
\dontrun{ ListThres = {GenThres=0.001,Thres_L2_1=0.001,Thres_L3_1=0.001,Thres_L3_2=0.001,Thres_L3_3=0.001,Thres_L3_4=0.001,Thres_L4_1=0.001,Thres_L4_2=0.001,Thres_L4_3=0.001,Thres_L4_4=0.001,Thres_L5_1=0.001,Thres_L5_2=0.001,Thres_L5_3=0.001,Thres_L5_4=0.001,Thres_L6_1=0.001,Thres_L6_2=0.001,Thres_L6_3=0.001,Thres_L6_4=0.001,Thres_L7_1=0.001,Thres_L7_2=0.001,Thres_L7_3=0.001,Thres_L7_4=0.001}}
}
