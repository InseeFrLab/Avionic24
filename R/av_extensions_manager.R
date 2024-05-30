#######################################################################
#' av_extend_MRIO_dw
#' Extend MRIO components by adding stressors like Value added or CO2 Emissions
#' You can manage this function to add new cases : other MRIO options or other stressors etc.
#'
#' @param MRIO_dw MRIO must be in wide format (not Long)
#' @param NameMRIO text
#' @param TypExtension text
#' @param Path1 text
#'
#' @return dw data wide (list)
#' @export
#' @import Rcpp
#' @import RcppArmadillo
#' @import tictoc
#' @import xlsx
#' @import dplyr
#' @import data.table
#' @import stringr
#' @import readxl
#' @import ggplot2
#' @examples
#' \dontrun{ Bonus <- CompoMRIO(DT, typeCompo = "OptFullOptionsBonus", OptTab = FALSE)
#' MRIO2 <- av_extend_MRIO_dw(MRIO_dw = Bonus, "FIGARO", TypExtension = "StressVA")
#' MRIO3 <- av_extend_MRIO_dw(MRIO_dw = Bonus, "FIGARO", TypExtension = "StressEmi", Path1 = PathTemp)
#' MRIO4 <- av_extend_MRIO_dw(MRIO3, "FIGARO", TypExtension = "StressVA")}
av_extend_MRIO_dw <- function(MRIO_dw, NameMRIO, TypExtension, Path1 = NULL) {
  # Case Stressor VA with any MRIO (no external information)
  if (TypExtension == "StressVA") {
    PROD <- MRIO_dw[["PROD"]]
    VA <- MRIO_dw[["VA"]]
    f_VA <- VA
    f_VA$value <- f_VA$value / PROD$value
    f_VA <- GereInfNA(f_VA)
    f_VA <- diag(unlist(f_VA$value))
    extension <- as.matrix(f_VA)
    AddRes <- list(VAOverOuput = extension)
    MRIO_dw_extended <- append(MRIO_dw, AddRes)
  }

  # Case Stressor CO2 Emissions with FIGARO
  if (NameMRIO == "FIGARO" & TypExtension == "StressEmi") {
    PROD <- MRIO_dw[["PROD"]]
    VA <- MRIO_dw[["VA"]]
    f_Emi <- PROD
    YearMRIO <- PROD[1, ]$year
    if (YearMRIO > 2019) {
      stop("ERROR : Year outside of FIGARO emissions data")
    }

    # Recover FIGARO Emissions with specific pathway
    Path_FIG22emi <- Path1 # "X:/HAB-Empreinte-carbone/MRIO/04_Bases_emissions_GES/FIGARO 22/CO2/"
    emi_fig22 <- fread(paste0(Path_FIG22emi, "CO2Footprint_", YearMRIO, "_20220404.csv"))
    Vect_emi <- emi_fig22[, sum(obs_value), by = c("ref_area", "industry")]

    # tabpass_geo <- fread("TabPass_geo.csv") # On doit recoder geo de 2 digit a 3 digit...
    tabpass_geo <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_GEO", col_names = TRUE)
    tabpass_geo <- setDT(tabpass_geo)
    tabpass_geo <- unique(tabpass_geo[, c("ISO3_CODE_Official", "FIGARO22_CODE_2")])
    Vect_emi2 <- tabpass_geo[Vect_emi, on = .(FIGARO22_CODE_2 = ref_area), nomatch = 0]
    Vect_emi2[industry == "L68", industry := "L"]
    Vect_emi2$PR <- paste0(Vect_emi2$ISO3_CODE_Official, "_", Vect_emi2$industry)
    ReorderVect_emi <- Vect_emi2[PastePRBR(PROD), on = .(PR = PR), nomatch = 0] # Attention la variable vectREF est générée dans la fonction Contenu
    ReorderVect_emi <- ReorderVect_emi[, c("PR", "V1")]
    setnames(ReorderVect_emi, "V1", "value")
    ReorderVect_emi <- setDF(ReorderVect_emi)
    ReorderVect_emi <- GetRownamesFromFirstCol(ReorderVect_emi)
    ReorderVect_emi <- as.matrix(ReorderVect_emi)
    f_Emi$value <- ReorderVect_emi
    f_Emi <- PastePRBR(f_Emi)
    f_Emi[, PR := NULL]
    f_Emi$value <- f_Emi$value / PROD$value
    f_Emi <- GereInfNA(f_Emi)
    f_Emi <- diag(unlist(f_Emi$value))
    extension <- as.matrix(f_Emi)
    AddRes <- list(EmiOverOuput = extension)
    MRIO_dw_extended <- append(MRIO_dw, AddRes)
  }

  # Case Stressor CO2 Emissions with ICIO
  if (NameMRIO == "ICIO" & TypExtension == "StressEmi") {
    # TECO2 OCDE
    Teco2 <- read.csv("X:/HAB-Empreinte-carbone/MRIO/04_Bases_emissions_GES/TECO2 format ICIO21/IO_GHG_2021_aout2023.csv", header = TRUE) # ,sep="|"
    Teco2 <- setDT(Teco2)

    Empreinte <- Teco2[i..VAR == "FD_CO2" & PAR == "WLD" & IND == "DTOTAL" & COU == "FRA", c("COU", "IND", "TIME", "Value")]
    Inventaire <- Teco2[i..VAR == "PROD_CO2" & TIME == annee & PAR == "WLD" & IND == "DTOTAL" & COU == "FRA", c("COU", "IND", "TIME", "Value")]
    StressorCO2 <- Teco2[i..VAR == "PROD_CO2" & IND != "DTOTAL" & TIME == annee & PAR == "WLD", c("COU", "IND", "TIME", "Value")]
    StressorCO2 <- setDT(StressorCO2)
    setnames(StressorCO2, c("IND", "COU", "Value"), c("Lig_Indus", "Lig_Country", "valueEmi"))
    StressorCO2 <- StressorCO2[, Lig_Indus := as.character(Lig_Indus)][, Lig_Country := as.character(Lig_Country)]
    formatStressor <- MRIO[["PROD"]]
    StressorCO2$Lig_Indus <- substr(StressorCO2$Lig_Indus, 2, length(StressorCO2$Lig_Indus))
    MergeFormat <- StressorCO2[formatStressor, on = .(Lig_Indus, Lig_Country)]
    StressorCO2 <- MergeFormat[, c("Lig_Indus", "Lig_Country", "year", "valueEmi")]

    EmiDirMenages <- Teco2[i..VAR == "PROD_CO2" & TIME == annee & PAR == "WLD" & COU == "FRA" & IND == "DMHH", c("COU", "IND", "TIME", "Value")]
  }

  # Case Stressor Employement from Eurostat for UE27 countries    WARNING: Employement_Eurostat/VA_Eurostat
  if (NameMRIO == "Eurostat" & TypExtension == "StressEmp") { # A FAIRE

    ### Eurostat Employement and value added
    Path_SIOTEurostat <- "X:/HAB-Empreinte-carbone/MRIO/02_Bases ICIO et MRIO/SIOT_Eurostat/"
    Path_FIG22empl <- "X:/HAB-Empreinte-carbone/MRIO/04_Bases_emissions_GES/FIGARO 22/EMPLOYEMENT/"


    BaseSIOTFRA <- fread(paste0(Path_SIOTEurostat, "naio_10_cp1700.tsv")) # Source search for "naio_10_cp1700" on the Eurostat search engine
    BaseEmploi <- fread(paste0(Path_FIG22empl, "nama_10_a64_e.tsv")) # Source: employment data in the national accounts section of the eurostat website

    BaseSIOTFRA_split <- BaseSIOTFRA %>%
      separate(V1, c("unit", "stk_flow", "induse", "prod_na", "geo\time"), ",")
    colnames(BaseSIOTFRA_split) <- BaseSIOTFRA_split[1, ]
    BaseSIOTFRA_split <- BaseSIOTFRA_split[-1, ]
    setnames(BaseSIOTFRA_split, "geo\\time", "geo")
    BaseSIOTFRA_split <- setDT(BaseSIOTFRA_split)

    BaseEmploi_split <- BaseEmploi %>%
      separate(V1, c("unit", "nace_r2", "na_item", "geo\time"), ",")
    colnames(BaseEmploi_split) <- BaseEmploi_split[1, ]
    BaseEmploi_split <- BaseEmploi_split[-1, ]
    setnames(BaseEmploi_split, "geo\\time", "geo")
    BaseEmploi_split <- setDT(BaseEmploi_split)

    annee <- 2019

    # Selections in the table to limit its size
    TES_w <- BaseSIOTFRA_split[geo == "FR" & unit == "MIO_EUR" & stk_flow == "DOM", c("unit", "stk_flow", "induse", "prod_na", "geo", "2019")] # Please note that only the domestic part is included (see DT Avionic).
    setnames(TES_w, "2019", "OBS_VALUE")
    TES_w <- TES_w[, OBS_VALUE := as.numeric(OBS_VALUE)]

    # Import of the code reference table (which is also a transit table)
    REFcodesA64 <- read_excel(paste0(Path_SIOTEurostat, "NomencSIOT.xlsx"), sheet = "INDUSTRY", col_names = TRUE)
    REFcodesA64 <- setDT(REFcodesA64)
    REFcodesOPdemandFin <- read_excel(paste0(Path_SIOTEurostat, "NomencSIOT.xlsx"), sheet = "OP", col_names = TRUE)
    REFcodesOPdemandFin <- setDT(REFcodesOPdemandFin)

    # Selection of TESS components and tabulation
    VA_w <- TES_w[prod_na == "B1G", ]
    VA_w2 <- REFcodesA64[VA_w, on = .(INDUSTRY = induse), nomatch = 0]
    VA_w3 <- VA_w2[, sum(OBS_VALUE), by = .(INDUSTRY_AGG)]
    VA_w <- VA_w3

    # Please note that the nomenclature of the job tables is slightly different, so load another tab that has been adapted.
    REFcodesA64emp <- read_excel(paste0(Path_SIOTEurostat, "NomencSIOT.xlsx"), sheet = "INDUSTRY_EMP", col_names = TRUE)
    REFcodesA64emp <- setDT(REFcodesA64emp)

    # Selections in the table to limit its size
    EMP_w <- BaseEmploi_split[geo == "FR" & unit == "THS_PER" & na_item == "EMP_DC", c("unit", "nace_r2", "na_item", "geo", "2019")]
    setnames(EMP_w, "2019", "OBS_VALUE")
    EMP_w <- EMP_w[, OBS_VALUE := as.numeric(OBS_VALUE)]
    EMP_w$nace_r2 <- paste0("CPA_", EMP_w$nace_r2)

    # Selection and formatting
    EMP_w2 <- REFcodesA64emp[EMP_w, on = .(INDUSTRY = nace_r2)] # EMP_w2<-REFcodesA64[EMP_w,on=.(INDUSTRY=nace_r2),  nomatch=0]
    EMP_w3 <- EMP_w2[, sum(OBS_VALUE), by = .(INDUSTRY_AGG)]
    EMP_w3 <- EMP_w3[-1, ]
    EMP_w <- EMP_w3
    EMP_DF <- setDF(EMP_w)
    EMP_DF <- GetRownamesFromFirstCol(EMP_DF)
    colnames(EMP_DF)[1] <- "EMPLOI"

    # Calculation of jobs/VA
    ratioEmploiVA <- vectDF(EMP_DF$EMPLOI / VA_w$V1)
    rownames(ratioEmploiVA) <- rownames(EMP_DF)
    ratioEmploiVA <- AddRownamesToFirstCol(ratioEmploiVA)
    ratioEmploiVA$Lig_Indus <- substr(ratioEmploiVA$joint, 5, length(ratioEmploiVA$joint))

    saveRDS(ratioEmploiVA, "ratioEmploiVA_2019.rds")
  }

  # Case Stressor GES Emissions with Exiobase

  # Case Stressor CO2? Emissions with WIOD

  # Case Stressor Employement with WIOD



  return(MRIO_dw_extended)
}

#######################################################################
#' EmissionsProd
#'  Function to calculate Inventory Emissions (coming from production)
#'  Be careful : [["EmiOverOuput"]] must have been introduced in our MRIO previously (with av_extend_MRIO_dw() function)
#'
#' @param MRIOinterm MRIO object
#'
#' @return df data frame
#' @export
#'
#' @examples
#' \dontrun{ EmiProd_MRIO <- EmissionsProd(MRIO)}
EmissionsProd <- function(MRIOinterm) {
  Prod <- MRIOinterm[["PROD"]]
  ProdVect <- as.matrix(Prod[, "value"])
  Vect_out <- Mult2_rcpp3(MRIOinterm[["EmiOverOuput"]], ProdVect)
  out <- as.data.frame(Vect_out)
  out$Lig_Country <- Prod$Lig_Country
  out$Lig_Indus <- Prod$Lig_Indus
  out <- setDT(out)
  setnames(out, "V1", "value")
  EmiProdMRIO_byCountry <- out[, sum(value), by = c("Lig_Country")]
  setnames(EmiProdMRIO_byCountry, "V1", "value")
  Sorties <- list(EmiProd = out, by_Country = EmiProdMRIO_byCountry)
  return(Sorties)
}
