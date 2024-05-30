#' BoucleAnneesMADEINs
#' Loop to build insight database on MADE-IN
#' 2 options : (1) On the full MRIO database (if memory allows) (2) by year but need to load all the dl and put them into a list (ListdlAnnual)
#'
#' @param dtdl text
#' @param Optdl binary
#' @param period time period
#' @param OptDonneesBrutes binary
#' @param OptAnnual binary
#' @param ListdlAnnual list
#' @param OptUE27 binary
#'
#' @return dl data long
#' @export
BoucleAnneesMADEINs <- function(dtdl, Optdl = TRUE, period, OptDonneesBrutes = FALSE, OptAnnual = FALSE, ListdlAnnual = list(), OptUE27 = FALSE) {
  if (OptAnnual == FALSE) {
    if (OptDonneesBrutes == FALSE) {
      for (k in period) {
        if (k == period[[1]]) { # initialisation
          Base_Out <- av_MadeIn(dtdl, Optdl = Optdl, k, OptDonneesBrutes = OptDonneesBrutes, OptUE27 = OptUE27)[["ratio_dt"]]
        } else {
          Base_interm <- av_MadeIn(dtdl, Optdl = Optdl, k, OptDonneesBrutes = OptDonneesBrutes, OptUE27 = OptUE27)[["ratio_dt"]]
          Base_Out <- rbind(Base_Out, Base_interm)
          print(k)
        }
      }
    }
    
    if (OptDonneesBrutes == TRUE) {
      for (k in period) {
        if (k == period[[1]]) { # initialisation
          Base_Out2 <- av_MadeIn(dtdl, Optdl = Optdl, k, OptDonneesBrutes = OptDonneesBrutes, OptUE27 = OptUE27)[["baseNumdenom"]][, year := k]
        } else {
          Base_interm <- av_MadeIn(dtdl, Optdl = Optdl, k, OptDonneesBrutes = OptDonneesBrutes, OptUE27 = OptUE27)[["baseNumdenom"]][, year := k]
          Base_Out2 <- rbind(Base_Out2, Base_interm)
          print(k)
        }
      }
    }
  } else { # OptAnnual==TRUE
    NbList <- length(ListdlAnnual)
    for (k in 1:NbList) {
      k_dl <- ListdlAnnual[[k]]
      YearRecup <- k_dl[1, "year"]$year
      Out_interm <- BoucleAnneesMADEINs(k_dl, Optdl = TRUE, YearRecup:YearRecup, OptDonneesBrutes = OptDonneesBrutes, OptAnnual = FALSE, ListdlAnnual = list(), OptUE27 = OptUE27)
      if (k == 1) {
        Base_Out <- Out_interm
      } else {
        Base_Out <- rbind(Base_Out, Out_interm)
      }
    }
    Base_Out2 <- Base_Out
  }
  if (OptDonneesBrutes == FALSE) {
    return(Base_Out)
  } else {
    return(Base_Out2)
  }
}

#############################################################
#' Contenus
#' Master function in the ecosystem
#' Function calculation of contents (in VA, émissions CO2, emploi) : allow footprint calculation along different formats
#' WARNING Needs CompoMRIO with bonus and extensions
#'
#' @param dw wide data MRIO object
#' @param typeContenu text
#' @param MethContenu text
#' @param EmprPays text country
#'
#' @return dl data long
#' @export
#'
#' @examples
#' \dontrun{ List_Contenus <- Contenus(List_Interm, typeContenu = "VA", MethContenu = "MatDF", EmprPays = SelectCountry)}
Contenus <- function(dw, typeContenu, MethContenu="MatDF", EmprPays="FRA")
  # typeContenu=VA;Emi;Emploi   MethContenu=MatDF;DiagDFtot;DiagDFpays
{
  # Extraction of the list of calculation components, and conversion into matrix
  
  # L
  L <- dw[["L"]]
  L <- as.matrix(L)
  
  # DF
  DF <- dw[["DF_tab"]]
  DF$PR <- paste0(DF$Lig_Country, "_", DF$Lig_Indus)
  DF <- DF[, c("year", "Lig_Country", "Lig_Indus") := NULL]
  setcolorder(DF, c("PR", setdiff(names(DF), "PR")))
  DF <- setDF(DF)
  DF <- GetRownamesFromFirstCol(DF)
  DF <- as.matrix(DF)
  
  # DF_TOT
  DF_TOT <- dw[["DF_TOT"]]
  DF_TOT$PR <- paste0(DF_TOT$Lig_Country, "_", DF_TOT$Lig_Indus)
  DF_TOT <- DF_TOT[, c("PR", "value")]
  setcolorder(DF_TOT, c("PR", setdiff(names(DF_TOT), "PR")))
  DF_TOT <- setDF(DF_TOT)
  DF_TOT <- GetRownamesFromFirstCol(DF_TOT)
  DF_TOT <- as.matrix(DF_TOT)
  
  # DF_EmprPays
  DF_EmprPays <- dw[["DF"]][Col_Country == EmprPays, ]
  DF_EmprPays <- DF_EmprPays[, sum(value), by = c("year", "Lig_Indus", "Lig_Country", "Col_Country")]
  setnames(DF_EmprPays, "V1", "value")
  DF_EmprPays <- dcast(DF_EmprPays, year + Lig_Country + Lig_Indus ~ Col_Country, value.var = "value")
  DF_EmprPays$PR <- paste0(DF_EmprPays$Lig_Country, "_", DF_EmprPays$Lig_Indus)
  DF_EmprPays <- DF_EmprPays[, c("year", "Lig_Country", "Lig_Indus") := NULL]
  setcolorder(DF_EmprPays, c("PR", setdiff(names(DF_EmprPays), "PR")))
  DF_EmprPays <- setDF(DF_EmprPays)
  DF_EmprPays <- GetRownamesFromFirstCol(DF_EmprPays)
  DF_EmprPays <- as.matrix(DF_EmprPays)
  
  # Matrix sizes
  nb_ligcol <- nrow(L)
  
  # Stressor identification and recovery
  if (typeContenu == "VA") {
    if ("VAOverOuput" %in% names(dw)) {
      Taux_Stressor <- dw[["VAOverOuput"]]
    } else {
      stop("ERROR : you need to add the right extension to compute calculation")
    }
  }
  if (typeContenu == "Emi") {
    if ("EmiOverOuput" %in% names(dw)) {
      Taux_Stressor <- dw[["EmiOverOuput"]]
    } else {
      stop("ERROR : you need to add the right extension to compute calculation")
    }
  }
  if (typeContenu == "Emploi") {
    if ("EmpOverOuput" %in% names(dw)) {
      Taux_Stressor <- dw[["EmpOverOuput"]]
    } else {
      stop("ERROR : you need to add the right extension to compute calculation")
    }
  }
  
  # Components of calculation
  part1 <- Taux_Stressor
  part2 <- L
  
  if (MethContenu == "MatDF") {
    part3 <- as.matrix(DF)
  }
  if (MethContenu == "DiagDFtot") {
    part3 <- as.matrix(diag(DF_TOT[, 1]))
  }
  if (MethContenu == "DiagDFpays") {
    part3 <- as.matrix(diag(DF_EmprPays[, 1]))
  }
  if (MethContenu == "DFtotpays") {
    part1 <- t(diag(part1))
    part3 <- as.matrix(DF_EmprPays[, 1])
  }
  
  #### Calculation
  
  MatEmpreinte <- CFPcalculationRCPP(part1, part2, part3)
  
  if (MethContenu == "DFtotpays") {
    return(MatEmpreinte)
  }
  
  if (MethContenu == "MatDF") {
    rownames(MatEmpreinte) <- rownames(L)
    colnames(MatEmpreinte) <- colnames(DF)
  }
  if (MethContenu == "DiagDFtot" | MethContenu == "DiagDFpays") {
    rownames(MatEmpreinte) <- rownames(L)
    colnames(MatEmpreinte) <- colnames(L)
  }
  
  MatEmpreinte <- as.data.frame(MatEmpreinte)
  MatEmpreinte_out <- MatEmpreinte
  MatEmpreinte <- AddRownamesToFirstCol(MatEmpreinte)
  MatEmpreinte[, 2:ncol(MatEmpreinte)] <- GereInfNA(MatEmpreinte[, 2:ncol(MatEmpreinte)])
  
  MatEmpreinte_dt <- setDT(MatEmpreinte)
  MatEmpreinte_dt <- melt(MatEmpreinte_dt, id.vars = c("joint"))
  setnames(MatEmpreinte_dt, "joint", "PR")
  setnames(MatEmpreinte_dt, "variable", "BR")
  MatEmpreinte_dt <- SplitPRBR(MatEmpreinte_dt)
  
  if (MethContenu == "DiagDFtot" | MethContenu == "DiagDFpays") {
    #    Production approach to content calculation
    Empreinte_Production <- vectDF(rowSums(MatEmpreinte[, 2:ncol(MatEmpreinte)]))
    rownames(Empreinte_Production) <- rownames(L)
    
    #    CConsumption-based calculation (footprint)
    Empreinte_Conso <- vectDF(colSums(MatEmpreinte[, 2:ncol(MatEmpreinte)]))
    rownames(Empreinte_Conso) <- rownames(L)
  }
  
  
  # Output
  if (MethContenu == "MatDF") {
    interm <- list(MatEmpreinte = MatEmpreinte_out, MatEmpreinte_dt = MatEmpreinte_dt)
  }
  if (MethContenu == "DiagDFtot" | MethContenu == "DiagDFpays") {
    interm <- list(Empreinte_Conso = Empreinte_Conso, Empreinte_Production = Empreinte_Production, MatEmpreinte = MatEmpreinte_out, MatEmpreinte_dt = MatEmpreinte_dt)
  }
  
  return(interm)
}

##############################################################
#' av_MadeIn
#' Function calculation of MADE-IN
#'
#' @param dtdl data
#' @param Optdl binary
#' @param annee year
#' @param OptDonneesBrutes binary
#' @param MadeInPays text country
#' @param OptUE27 binary
#'
#' @return dl data long
#' @export
av_MadeIn <- function(dtdl, Optdl = TRUE, annee, OptDonneesBrutes = FALSE, MadeInPays = "FRA", OptUE27 = FALSE) {
  # Check if input is dt or dl : if dl then calculate dw with VA stressor extension
  if (Optdl == TRUE) {
    List_Interm1 <- CompoMRIO(dtdl, "OptFullOptionsBonus", date = annee, OptUE27 = OptUE27)
    List_Interm <- av_extend_MRIO_dw(List_Interm1, "X", TypExtension = "StressVA")
  } else {
    List_Interm <- dtdl
    List_Interm <- av_extend_MRIO_dw(List_Interm, "X", TypExtension = "StressVA")
  }
  
  List_Contenus <- Contenus(List_Interm, typeContenu = "VA", MethContenu = "MatDF", EmprPays = MadeInPays) # typeContenu=VA;Emi;Emploi   MethContenu=MatDF;DiagDFtot
  
  interm <- List_Contenus[["MatEmpreinte_dt"]]
  interm <- SplitPRBR(interm)
  denom <- interm[, sum(value), by = .(Lig_Indus, Col_Country)]
  num <- interm[Col_Country == Lig_Country, ]
  num <- num[, sum(value), by = .(Lig_Indus, Col_Country)]
  baseNumdenom <- rbind(num[, position := "numerateur"], denom[, position := "denominateur"])
  setnames(baseNumdenom, "V1", "value")
  
  # Ratio by industry
  ratio_parBR <- denom[num, on = .(Lig_Indus, Col_Country)]
  ratio_parBR[, "value"] <- ratio_parBR[, "i.V1"] / ratio_parBR[, "V1"]
  ratio_parBR <- GereInfNA(ratio_parBR)
  Ordre_Col <- c("Lig_Indus", "Col_Country", "value")
  ratio_parBR <- ratio_parBR[, ..Ordre_Col]
  ratio_parBR <- ratio_parBR[, year := annee]
  
  # Ratio total by country
  ratio_Tot <- denom[num, on = .(Lig_Indus, Col_Country)]
  ratio_Tot <- ratio_Tot[, .(sum(V1), sum(i.V1)), by = c("Col_Country")]
  ratio_Tot[, "value"] <- ratio_Tot[, "V2"] / ratio_Tot[, "V1"]
  ratio_Tot <- GereInfNA(ratio_Tot)
  Ordre_Col <- c("Col_Country", "value")
  ratio_Tot <- ratio_Tot[, ..Ordre_Col]
  ratio_Tot <- ratio_Tot[, year := annee][, Lig_Indus := "TOTAL"]
  
  ratio_dt <- rbind(ratio_Tot, ratio_parBR)
  
  # Ratio total by country of origins
  ratio_parBR_tab <- dcast(ratio_parBR, Lig_Indus ~ Col_Country, value.var = "value")
  
  # Out
  interm <- list(ratio_Tot = ratio_Tot, ratio_parBR_tab = ratio_parBR_tab, ratio_parBR = ratio_parBR, ratio_dt = ratio_dt)
  if (OptDonneesBrutes == TRUE) {
    interm <- list(ratio_Tot = ratio_Tot, ratio_parBR_tab = ratio_parBR_tab, ratio_parBR = ratio_parBR, ratio_dt = ratio_dt, baseNumdenom = baseNumdenom)
  }
  return(interm)
}

#' ContVAdesExports
#' Calculation of content of value addend embedded in exports for a given country and a given year
#' Foster-McGregor, N., et R. Stehrer (2013) : “Value added content of trade : A comprehensive approach,” Economics Letters, 120(2), 354–357.
#'
#' @param dtdl data
#' @param Optdl binary
#' @param annee year
#' @param pays text country
#' @param OptUE27 binary
#'
#' @return dl data long
#' @export
ContVAdesExports <- function(dtdl, Optdl = TRUE, annee, pays, OptUE27 = FALSE) {
  if (Optdl == TRUE) {
    Base_depart <- CompoMRIO(dtdl, "OptFullOptionsBonus", annee, OptUE27 = OptUE27)
  } else {
    Base_depart <- dtdl
  }
  
  # Initial framework to set results
  Cadre <- Base_depart[["DF"]]
  Cadre$value <- 0
  Cadre <- ReqSum(Cadre, "Col_Indus") # Sum components of final demand
  Cadre$Col_Indus <- "TOTDF"
  Cadre_tab <- dcast(Cadre, Lig_Country + Lig_Indus ~ Col_Country, value.var = "value")
  
  # Export calculation
  BaseCIDF <- rbind(Base_depart[["CI"]], Base_depart[["DF"]])
  BaseSelect <- BaseCIDF[Lig_Country == pays & Col_Country != pays, ]
  BaseSelect <- ReqSum(BaseSelect, c("Col_Country", "Col_Indus"))
  BaseDom <- BaseSelect
  BaseDom$Col_Country <- pays
  BaseDom$Col_Indus <- "TOTDF"
  
  # Calculation (in negative) of exports from other countries to our pays
  BaseSelect <- BaseCIDF[Lig_Country != pays & Col_Country == pays, ]
  BaseSelect <- ReqSum(BaseSelect, c("Col_Indus")) # on agrège les composantes de demande finale
  BaseSelect$value <- -BaseSelect$value
  BaseXversDom <- BaseSelect
  BaseXversDom$Col_Country <- BaseXversDom$Lig_Country
  BaseXversDom$Col_Indus <- "TOTDF"
  
  # Build calculation components into the framework
  BaseTot <- rbind(BaseDom, BaseXversDom)
  Cadre_tab <- dcast(BaseTot, Lig_Country + Lig_Indus ~ Col_Country, value.var = "value")
  Cadre_tab[is.na(Cadre_tab), ] <- 0
  
  # Calculation
  stressor <- as.matrix(Base_depart[["VA"]]$value, drop = FALSE)
  Prod <- as.matrix(Base_depart[["PROD"]]$value, drop = FALSE)
  Taux_Stressor <- stressor / Prod
  Taux_Stressor <- as.vector(Taux_Stressor)
  part1 <- diag(Taux_Stressor)
  part2 <- as.matrix(Base_depart[["L"]])
  part3 <- as.matrix(Cadre_tab[, 3:ncol(Cadre_tab)])
  MatContVAExports <- CFPcalculationRCPP(part1, part2, part3)
  MatContVAExports <- as.data.frame(MatContVAExports)
  MatContVAExports <- GereInfNA(MatContVAExports)
  rownames(MatContVAExports) <- paste0(Base_depart[["DF_tab"]]$Lig_Country, "_", Base_depart[["DF_tab"]]$Lig_Indus)
  colnames(MatContVAExports) <- unique(Base_depart[["DF_tab"]]$Lig_Country)
  MatContVAExports <- AddRownamesToFirstCol(MatContVAExports)
  
  MatContVAExports <- setDT(MatContVAExports)
  ComposantesVAinX <- melt(MatContVAExports)
  setnames(ComposantesVAinX, "joint", "PR")
  setnames(ComposantesVAinX, "variable", "BR")
  ComposantesVAinX <- SplitPRBR(ComposantesVAinX)
  ComposantesVAinX[, Col_Indus := NULL]
  
  DVAiX <- ComposantesVAinX[Lig_Country == pays & Col_Country == pays, ][, Col_Indus := "TOTAL"][, Compo := "DVAiX"]
  DVAiM <- ComposantesVAinX[Lig_Country == pays & Col_Country != pays, ][, Col_Indus := "TOTAL"][, Compo := "DVAiM"]
  FVAiX <- ComposantesVAinX[Lig_Country != pays & Col_Country == pays, ][, Col_Indus := "TOTAL"][, Compo := "FVAiX"]
  FVAiM <- ComposantesVAinX[Lig_Country != pays & Col_Country != pays, ][, Col_Indus := "TOTAL"][, Compo := "FVAiM"]
  BVAiM <- ComposantesVAinX[Lig_Country != pays & Col_Country != pays & Lig_Country == Col_Country, ][, Col_Indus := "TOTAL"][, Compo := "BVAiM"]
  
  # Add informations for denominator ratio
  BaseCIDF <- rbind(Base_depart[["CI"]], Base_depart[["DF"]])
  BaseExport <- BaseCIDF[Lig_Country == pays & Col_Country != pays, ]
  BaseExport <- ReqSum(BaseExport, c("Col_Country", "Col_Indus"))
  BaseExport <- BaseExport[, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Compo := "Export"][, CountryREF := pays]
  setnames(BaseExport, "year", "yearREF")
  BaseVA <- Base_depart[["VA"]][Col_Country == pays, ][, Lig_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "VA"][, CountryREF := pays]
  setnames(BaseVA, "year", "yearREF")
  
  # Calculation of key indicators
  DVAiM_PR <- DVAiM[, sum(value), by = Lig_Indus][, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "DVAiM_PR"]
  setnames(DVAiM_PR, "V1", "value")
  FVAiX_PR <- FVAiX[, sum(value), by = Lig_Country][, Col_Indus := "TOTAL"][, Lig_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Compo := "FVAiX_PR"]
  setnames(FVAiX_PR, "V1", "value")
  FVAiM_PR <- FVAiM[, sum(value), by = Lig_Indus][, Col_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Col_Country := "TOTAL"][, Compo := "FVAiM_PR"]
  setnames(FVAiM_PR, "V1", "value")
  BVAiM_PR <- BVAiM[, sum(value), by = Lig_Indus][, Col_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Col_Country := "TOTAL"][, Compo := "BVAiM_PR"]
  setnames(BVAiM_PR, "V1", "value")
  VAXnoguera <- DVAiX # initialize Johnson-Noguera indicator
  VAXnoguera$value <- as.data.frame(DVAiX$value + DVAiM_PR$value, drop = FALSE)
  VAXnoguera <- setDT(VAXnoguera)
  VAXnoguera <- VAXnoguera[, Col_Indus := "TOTAL"][, Compo := "VAXnoguera_PR"]
  
  DVAiX_tot <- DVAiX[, sum(value), by = Compo][, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Lig_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "DVAiX_tot"]
  setnames(DVAiX_tot, "V1", "value")
  DVAiM_tot <- DVAiM_PR[, sum(value), by = Compo][, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Lig_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "DVAiM_tot"]
  setnames(DVAiM_tot, "V1", "value")
  FVAiX_tot <- FVAiX_PR[, sum(value), by = Compo][, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Lig_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "FVAiX_tot"]
  setnames(FVAiX_tot, "V1", "value")
  FVAiM_tot <- FVAiM_PR[, sum(value), by = Compo][, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Lig_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "FVAiM_tot"]
  setnames(FVAiM_tot, "V1", "value")
  BVAiM_tot <- BVAiM_PR[, sum(value), by = Compo][, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Lig_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "BVAiM_tot"]
  setnames(BVAiM_tot, "V1", "value")
  VAXnoguera_tot <- VAXnoguera[, sum(value), by = Compo][, Col_Indus := "TOTAL"][, Col_Country := "TOTAL"][, Lig_Indus := "TOTAL"][, Lig_Country := "TOTAL"][, Compo := "VAXnoguera_tot"]
  setnames(VAXnoguera_tot, "V1", "value")
  
  Base_Out <- rbind(DVAiX, DVAiM, FVAiX, FVAiM, BVAiM, DVAiM_PR, FVAiX_PR, FVAiM_PR, BVAiM_PR, VAXnoguera, DVAiX_tot, DVAiM_tot, FVAiX_tot, FVAiM_tot, BVAiM_tot, VAXnoguera_tot)
  Base_Out$CountryREF <- pays
  Base_Out$yearREF <- annee
  Base_Out <- rbind(Base_Out, BaseExport, BaseVA) # Add informations for denominator ratio
  return(Base_Out)
}

#' BouclePaysEtAnneesContVAdesExports
#' Calculation loop of content of value addend embedded in exports for a list of countries and a time period (several years)
#' if OptAnnual=TRUE then dtdl must be a list of dtdl
#' Foster-McGregor, N., et R. Stehrer (2013) : “Value added content of trade : A comprehensive approach,” Economics Letters, 120(2), 354–357.
#'
#' @param dtdl data
#' @param Optdl binary
#' @param period time period
#' @param ListCountries list
#' @param OptUE27 binary
#' @param OptAnnual binary
#'
#' @return dl data long
#' @export
BouclePaysEtAnneesContVAdesExports <- function(dtdl, Optdl = TRUE, period, ListCountries = list(), OptUE27 = FALSE, OptAnnual = FALSE) {
  if (OptAnnual == FALSE) {
    for (k in period) {
      Base_interm <- BouclePaysContVAdesExports(dtdl, Optdl = Optdl, k, ListCountries = ListCountries, OptUE27 = OptUE27)
      
      if (k == period[[1]]) {
        Base_Out <- Base_interm
      } else {
        Base_Out <- rbind(Base_Out, Base_interm)
      }
      print(paste0("year = ", k))
    }
  } else { # OptAnnual==TRUE
    
    nb_years <- length(dtdl)
    for (k in 1:nb_years) {
      Base_interm <- BouclePaysContVAdesExports(dtdl[[k]], Optdl = Optdl, dtdl[[k]][1, "year"]$year, ListCountries = ListCountries, OptUE27 = OptUE27)
      
      if (k == 1) {
        Base_Out <- Base_interm
      } else {
        Base_Out <- rbind(Base_Out, Base_interm)
      }
      print(paste0("year = ", dtdl[[k]][1, "year"]$year))
    }
  }
  return(Base_Out)
}


#' BouclePaysContVAdesExports
#' Calculation loop of content of value addend embedded in exports for a list of countries and a given year
#' Foster-McGregor, N., et R. Stehrer (2013) : “Value added content of trade : A comprehensive approach,” Economics Letters, 120(2), 354–357.
#'
#' @param dtdl data
#' @param Optdl binary
#' @param annee year
#' @param ListCountries list
#' @param OptUE27 binary
#'
#' @return dl data long
#' @export
BouclePaysContVAdesExports <- function(dtdl, Optdl = TRUE, annee, ListCountries = list(), OptUE27 = FALSE) {
  if (length(ListCountries) == 0) { # All countries
    List_Pays <- unique(DT[, "Lig_Country"])
  } else { # eg : c("UE27","USA","CHN","JPN") or c("FRA","DEU","ITA","ESP","GBR") or c("FR","DE","IT","ES","GB")
    List_Pays <- as.data.frame(ListCountries)
  }
  
  for (k in 1:nrow(List_Pays)) {
    if (Optdl == TRUE) {
      dtdl <- dtdl[year == annee, ]
    }
    Base_interm <- ContVAdesExports(dtdl, Optdl = Optdl, annee = annee, pays = as.character(List_Pays[k, 1]), OptUE27 = OptUE27)
    if (k == 1) {
      Base_Out <- Base_interm
    } else {
      Base_Out <- rbind(Base_Out, Base_interm)
    }
  }
  
  return(Base_Out)
}

#' Agreg_Manuf
#' greggation function
#' TypAgreg= Manuf_CT ;	Manuf_IP19 ;	Manuf_CT_lrwiod ;	Manuf_IP19_lrwiod     MRIO= LRWIOD ; WIOD; FIGARO
#' Warning : the variable to agreggate must be named "value"
#' Use of StructDocs to manage classifications
#'
#' @param DT datatable
#' @param MRIO MRIO object
#' @param Var_To_Agreg text variable
#' @param TypAgreg text
#'
#' @return dl data long
#' @export
Agreg_Manuf <- function(DT, MRIO, Var_To_Agreg, TypAgreg) {
  List_var <- colnames(DT)
  List_var_HorsToAgreg <- List_var[!(List_var %in% c(Var_To_Agreg, "value"))]
  List_var_HorsToAgreg <- append(List_var_HorsToAgreg, TypAgreg)
  TabPassIND <- read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_PR", col_names = TRUE)
  TabPass <- TabPassIND[, c("A17", "WIOD56rel2016_CODE", "Figaro64ind", "LR_WIOD_Rev1", "Manuf_CT", "Manuf_IP19", "Manuf_CT_lrwiod", "Manuf_IP19_lrwiod", "ICIO21OECD45_CODE")]
  TabPass <- setDT(TabPass[1:140, ])
  TabPass <- unique(TabPass)
  
  # Concordance tables for each MRIO
  if (MRIO == "LRWIOD") {
    TabPass_mrio <- unique(TabPass[, c(..TypAgreg, "LR_WIOD_Rev1")])
    jointure <- DT[TabPass_mrio, on = .(Lig_Indus = LR_WIOD_Rev1)]
  }
  if (MRIO == "WIOD") {
    TabPass_mrio <- unique(TabPass[, c(..TypAgreg, "WIOD56rel2016_CODE")])
    jointure <- DT[TabPass_mrio, on = .(Lig_Indus = WIOD56rel2016_CODE)]
  }
  if (MRIO == "FIGARO") {
    TabPass_mrio <- unique(TabPass[, c(..TypAgreg, "Figaro64ind")])
    jointure <- DT[TabPass_mrio, on = .(Lig_Indus = Figaro64ind)]
  }
  if (MRIO == "ICIO") {
    TabPass_mrio <- unique(TabPass[, c(..TypAgreg, "ICIO21OECD45_CODE")])
    jointure <- DT[TabPass_mrio, on = .(Lig_Indus = ICIO21OECD45_CODE)]
  }
  if (MRIO != "LRWIOD") {
    Agreg <- na.omit(jointure[, sum(value, na.rm = TRUE), by = List_var_HorsToAgreg]) #  ,MRIO
    setnames(Agreg, c("V1", TypAgreg), c("value", Var_To_Agreg))
    resultat <- Agreg
  } else { # LR-WIOD case is more complicated because D21t22 and LtQ need to be splitten between "Manuf" and "ServMarch"
    Agreg <- na.omit(jointure[, sum(value, na.rm = TRUE), by = List_var_HorsToAgreg]) #  ,MRIO
    setnames(Agreg, c("V1", TypAgreg), c("value", Var_To_Agreg))
    List_var2 <- colnames(Agreg)
    List_var2_HorsToAgreg <- List_var2[!(List_var2 %in% c(Var_To_Agreg, "value"))]
    Somme_PourDcast <- paste(List_var2_HorsToAgreg, collapse = "+")
    Somme_PourDcast <- paste0(Somme_PourDcast, "~", Var_To_Agreg)
    Agreg_tab <- dcast(Agreg, Somme_PourDcast, var.value = "value")
    
    if (TypAgreg == "Manuf_IP19_lrwiod") {
      Agreg_tab <- GereInfNA(Agreg_tab)
      Agreg_tab <- Agreg_tab[, ManufFabC3C5 := ManufFabC3C5 + 0.5 * D21t22]
      Agreg_tab <- Agreg_tab[, Services := Services + 0.5 * D21t22]
      Agreg_tab <- Agreg_tab[, D21t22 := NULL]
      Agreg_re <- melt(Agreg_tab, id.vars = List_var2_HorsToAgreg, measure.vars = colnames(Agreg_tab)[!(colnames(Agreg_tab) %in% List_var2_HorsToAgreg)])
      setnames(Agreg_re, "variable", Var_To_Agreg)
    }
    if (TypAgreg == "Manuf_CT_lrwiod") { # Coefficients comes from real observations on the year 2000
      Agreg_tab <- GereInfNA(Agreg_tab)
      Agreg_tab <- Agreg_tab[, Manuf := Manuf + 0.5 * D21t22]
      Agreg_tab <- Agreg_tab[, ServMarch := ServMarch + 0.5 * D21t22]
      Agreg_tab <- Agreg_tab[, D21t22 := NULL]
      Agreg_tab <- Agreg_tab[, ServNonMarch := 0.53 * LtQ]
      Agreg_tab <- Agreg_tab[, ServMarch := ServMarch + 0.47 * LtQ]
      Agreg_tab <- Agreg_tab[, LtQ := NULL]
      Agreg_re <- melt(Agreg_tab, id.vars = List_var2_HorsToAgreg, measure.vars = colnames(Agreg_tab)[!(colnames(Agreg_tab) %in% List_var2_HorsToAgreg)])
      setnames(Agreg_re, "variable", Var_To_Agreg)
    }
    resultat <- Agreg_re
  }
  return(resultat)
}


#' MadeIn_Manuf
#' Calculation of manuf's made-in by agreggation (2 classifications)
#'
#' @param resMadeIn_LRWIOD datatable
#' @param resMadeIn_WIOD datatable
#' @param resMadeIn_FIGARO datatable
#' @param resMadeIn_ICIO datatable
#' @param OptSaveRDS binary
#'
#' @return dl data long
#' @export
MadeIn_Manuf <- function(resMadeIn_LRWIOD, resMadeIn_WIOD, resMadeIn_FIGARO, resMadeIn_ICIO, OptSaveRDS = FALSE) {
  BASEres_MADEINs_LRWIODb <- resMadeIn_LRWIOD
  BASEres_MADEINs_WIODb <- resMadeIn_WIOD
  BASEres_MADEINs_FIGb <- resMadeIn_FIGARO
  BASEres_MADEINs_ICIOb <- resMadeIn_ICIO
  
  #################################################################### "
  ### WIOD
  # We aggregate on the 2 type of manuf classifications
  wiod_manufCT <- Agreg_Manuf(BASEres_MADEINs_WIODb, "WIOD", "Lig_Indus", "Manuf_CT")
  wiod_manufIP19 <- Agreg_Manuf(BASEres_MADEINs_WIODb, "WIOD", "Lig_Indus", "Manuf_IP19")
  
  # Calculation of made-in
  wiod_manufCT_tab <- dcast(wiod_manufCT, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  wiod_manufIP19_tab <- dcast(wiod_manufIP19, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  wiod_manufCT_tab <- wiod_manufCT_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "WIOD"][, TypeManuf := "manufCT"]
  wiod_manufIP19_tab <- wiod_manufIP19_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "WIOD"][, TypeManuf := "manufIP19"]
  Base_niveaux_MadeIns <- rbind(wiod_manufCT_tab, wiod_manufIP19_tab)
  wiod_manufCT_tab <- wiod_manufCT_tab[, numerateur := NULL][, denominateur := NULL]
  wiod_manufIP19_tab <- wiod_manufIP19_tab[, numerateur := NULL][, denominateur := NULL]
  
  #################################################################### "
  ### FIGARO
  # We aggregate on the 2 type of manuf classifications
  fig_manufCT <- Agreg_Manuf(BASEres_MADEINs_FIGb, "FIGARO", "Lig_Indus", "Manuf_CT")
  fig_manufIP19 <- Agreg_Manuf(BASEres_MADEINs_FIGb, "FIGARO", "Lig_Indus", "Manuf_IP19")
  
  # Calculation of made-in
  fig_manufCT_tab <- dcast(fig_manufCT, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  fig_manufIP19_tab <- dcast(fig_manufIP19, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  fig_manufCT_tab <- fig_manufCT_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "FIGARO"][, TypeManuf := "manufCT"]
  fig_manufIP19_tab <- fig_manufIP19_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "FIGARO"][, TypeManuf := "manufIP19"]
  Base_niveaux_MadeIns <- rbind(Base_niveaux_MadeIns, fig_manufCT_tab, fig_manufIP19_tab)
  fig_manufCT_tab <- fig_manufCT_tab[, numerateur := NULL][, denominateur := NULL]
  fig_manufIP19_tab <- fig_manufIP19_tab[, numerateur := NULL][, denominateur := NULL]
  
  #################################################################### "
  ### LR-WIOD
  # We aggregate on the 2 type of manuf classifications
  lrwiod_manufCT <- Agreg_Manuf(BASEres_MADEINs_LRWIODb, "LRWIOD", "Lig_Indus", "Manuf_CT_lrwiod")
  lrwiod_manufIP19 <- Agreg_Manuf(BASEres_MADEINs_LRWIODb, "LRWIOD", "Lig_Indus", "Manuf_IP19_lrwiod")
  
  # Calculation of made-in
  lrwiod_manufCT_tab <- dcast(lrwiod_manufCT, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  lrwiod_manufIP19_tab <- dcast(lrwiod_manufIP19, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  lrwiod_manufCT_tab <- lrwiod_manufCT_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "LRWIOD"][, TypeManuf := "manufCT"]
  lrwiod_manufIP19_tab <- lrwiod_manufIP19_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "LRWIOD"][, TypeManuf := "manufIP19"]
  Base_niveaux_MadeIns <- rbind(Base_niveaux_MadeIns, lrwiod_manufCT_tab, lrwiod_manufIP19_tab)
  lrwiod_manufCT_tab <- lrwiod_manufCT_tab[, numerateur := NULL][, denominateur := NULL]
  lrwiod_manufIP19_tab <- lrwiod_manufIP19_tab[, numerateur := NULL][, denominateur := NULL]
  
  #################################################################### "
  ### ICIO
  # We aggregate on the 2 type of manuf classifications
  icio_manufCT <- Agreg_Manuf(BASEres_MADEINs_ICIOb, "ICIO", "Lig_Indus", "Manuf_CT")
  icio_manufIP19 <- Agreg_Manuf(BASEres_MADEINs_ICIOb, "ICIO", "Lig_Indus", "Manuf_IP19")
  
  # Calculation of made-in
  icio_manufCT_tab <- dcast(icio_manufCT, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  icio_manufIP19_tab <- dcast(icio_manufIP19, year + Col_Country + Lig_Indus ~ position, var.value = "value")
  icio_manufCT_tab <- icio_manufCT_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "FIGARO"][, TypeManuf := "manufCT"]
  icio_manufIP19_tab <- icio_manufIP19_tab[, RatioMadeIn := numerateur / denominateur][, MRIO := "FIGARO"][, TypeManuf := "manufIP19"]
  Base_niveaux_MadeIns <- rbind(Base_niveaux_MadeIns, icio_manufCT_tab, icio_manufIP19_tab)
  icio_manufCT_tab <- icio_manufCT_tab[, numerateur := NULL][, denominateur := NULL]
  icio_manufIP19_tab <- icio_manufIP19_tab[, numerateur := NULL][, denominateur := NULL]
  
  #################################################################### "
  # Building of the whole database of indicators
  
  wiod_manufCT_tab <- wiod_manufCT_tab[, MRIO := "WIOD"][, TypeManuf := "manufCT"]
  wiod_manufIP19_tab <- wiod_manufIP19_tab[, MRIO := "WIOD"][, TypeManuf := "manufIP19"]
  fig_manufCT_tab <- fig_manufCT_tab[, MRIO := "FIGARO"][, TypeManuf := "manufCT"]
  fig_manufIP19_tab <- fig_manufIP19_tab[, MRIO := "FIGARO"][, TypeManuf := "manufIP19"]
  lrwiod_manufCT_tab <- lrwiod_manufCT_tab[, MRIO := "LRWIOD"][, TypeManuf := "manufCT"]
  lrwiod_manufIP19_tab <- lrwiod_manufIP19_tab[, MRIO := "LRWIOD"][, TypeManuf := "manufIP19"]
  icio_manufCT_tab <- icio_manufCT_tab[, MRIO := "ICIO"][, TypeManuf := "manufCT"]
  icio_manufIP19_tab <- icio_manufIP19_tab[, MRIO := "ICIO"][, TypeManuf := "manufIP19"]
  
  Base_MadeIn_Manuf <- rbind(wiod_manufCT_tab, wiod_manufIP19_tab, fig_manufCT_tab, fig_manufIP19_tab, lrwiod_manufCT_tab, lrwiod_manufIP19_tab, icio_manufCT_tab, icio_manufIP19_tab)
  
  out_list <- list(Base_MadeIn_Manuf, Base_niveaux_MadeIns)
  
  if (OptSaveRDS == TRUE) {
    saveRDS(Base_MadeIn_Manuf, "RESULT_MADEINs_Manuf.rds")
    saveRDS(Base_niveaux_MadeIns, "RESULT_MADEINs_Manuf_Niveau.rds")
  }
  
  return(out_list)
}

#' MadeIn_Retropolation
#' Retropolation of made-in to fit long series inter-MRIO
#' You can manage the MRIO list
#'
#' @param resMadeIn_manuf datatable
#' @param MadeIn_levels datatable
#' @param resMadeIn_LRWIOD datatable
#' @param resMadeIn_WIOD datatable
#' @param resMadeIn_FIGARO datatable
#' @param resMadeIn_ICIO datatable
#' @param OptSaveRDS binary
#'
#' @return dl data long
#' @export
MadeIn_Retropolation <- function(resMadeIn_manuf, MadeIn_levels, resMadeIn_LRWIOD, resMadeIn_WIOD, resMadeIn_FIGARO, resMadeIn_ICIO, OptSaveRDS = FALSE) {
  ##### By sector
  DTManuf <- resMadeIn_manuf
  DTManuf_select1 <- DTManuf[MRIO == "LRWIOD" & year %in% 1965:1999, ]
  DTManuf_select2 <- DTManuf[MRIO == "WIOD" & year %in% 2000:2009, ]
  DTManuf_select3 <- DTManuf[MRIO == "FIGARO" & year %in% 2010:2020, ]
  DTManuf_select <- rbind(DTManuf_select1, DTManuf_select2, DTManuf_select3)
  DTManuf_select_ALL <- dcast(DTManuf_select, TypeManuf + Col_Country + Lig_Indus ~ year, value.var = "RatioMadeIn")
  DTManuf_tab_ALL <- dcast(DTManuf, TypeManuf + Col_Country + Lig_Indus + MRIO ~ year, value.var = "RatioMadeIn")
  
  ##### On the total
  # Loading of results on the MADE-IN
  BASEres_MADEINs_LRWIOD <- resMadeIn_LRWIOD
  BASEres_MADEINs_WIOD <- resMadeIn_WIOD
  BASEres_MADEINs_FIG <- resMadeIn_FIGARO
  BASEres_MADEINs_ICIO <- resMadeIn_ICIO
  # Selection of data and builing of a unique table
  madeinSelect_LRWIOD <- BASEres_MADEINs_LRWIOD[Lig_Indus == "TOTAL", ][, MRIO := "LRWIOD"]
  madeinSelect_WIOD <- BASEres_MADEINs_WIOD[Lig_Indus == "TOTAL", ][, MRIO := "WIOD"]
  madeinSelect_FIG <- BASEres_MADEINs_FIG[Lig_Indus == "TOTAL", ][, MRIO := "FIG"]
  madeinSelect_ICIO <- BASEres_MADEINs_FIG[Lig_Indus == "TOTAL", ][, MRIO := "ICIO"]
  madeinSelect_ALL <- rbind(madeinSelect_LRWIOD, madeinSelect_WIOD, madeinSelect_FIG, madeinSelect_ICIO)
  madeinSelect_tab <- dcast(madeinSelect_ALL, Col_Country + MRIO ~ year, value.var = "value") # Wide format
  DT_select1 <- madeinSelect_LRWIOD[MRIO == "LRWIOD" & year %in% 1965:1999, ]
  DT_select2 <- madeinSelect_WIOD[MRIO == "WIOD" & year %in% 2000:2009, ]
  DT_select3 <- madeinSelect_FIG[MRIO == "FIG" & year %in% 2010:2020, ]
  DT_select <- rbind(DT_select1, DT_select2, DT_select3)
  DT_select_ALL <- dcast(DT_select, Col_Country ~ year, value.var = "value")
  
  ##### Retropolation section : we only retropolate for countries of interest
  Ref_tot <- DT_select_ALL
  Ref_Sectors <- DTManuf_select_ALL
  
  ### Retro on the total
  # We only change series for Germany after 2010 : we keep WIOD on 2010-2014 and after 2014 we use FIGARO evolutions based on 2014 WIOD level.
  Ref_tot[Col_Country == "DEU", c("2010", "2011", "2012", "2013", "2014")] <- madeinSelect_tab[Col_Country == "DEU" & MRIO == "WIOD", c("2010", "2011", "2012", "2013", "2014")]
  for (k in 2015:2020) {
    RefCol1 <- which(colnames(Ref_tot) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(madeinSelect_tab) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Ref_tot[Col_Country == "DEU", RefCol1] <- unlist(Ref_tot[Col_Country == "DEU", RefCol1moins1, with = FALSE]) * unlist(madeinSelect_tab[Col_Country == "DEU" & MRIO == "FIG", RefCol2, with = FALSE]) / unlist(madeinSelect_tab[Col_Country == "DEU" & MRIO == "FIG", RefCol2moins1, with = FALSE])
  }
  
  ### Retro by sector
  # We only change series for Germany Agri and Manuf and ServNonMarch : we keep FIGARO after 2010 and changes before 2010 are :
  #   Agri : btw 2000-2009 we retropolate with WIOD evolutions based on 2010 FIGARO level. Before 2000 we retropolate with LR-WIOD evolutions based on 2000 level calculated just before.
  for (k in 2009:2000) {
    RefCol1 <- which(colnames(Ref_Sectors) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(DTManuf_tab_ALL) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol2plus1, with = FALSE])
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol2plus1, with = FALSE])
  }
  for (k in 1999:1965) {
    RefCol1 <- which(colnames(Ref_Sectors) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(DTManuf_tab_ALL) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "Agri" & TypeManuf == "manufCT", RefCol2plus1, with = FALSE])
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "Agri" & TypeManuf == "manufIP19", RefCol2plus1, with = FALSE])
  }
  
  #   Manuf : btw 2000-2009 we retropolate with WIOD evolutions based on 2010 FIGARO level. Before 2000 we retropolate with LR-WIOD evolutions based on 2000 level calculated just before.
  for (k in 2009:2000) {
    RefCol1 <- which(colnames(Ref_Sectors) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(DTManuf_tab_ALL) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol2plus1, with = FALSE])
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol2plus1, with = FALSE])
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol2plus1, with = FALSE])
  }
  for (k in 1999:1965) {
    RefCol1 <- which(colnames(Ref_Sectors) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(DTManuf_tab_ALL) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "ManufFabC3C5" & TypeManuf == "manufIP19", RefCol2plus1, with = FALSE])
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "ManufNonFab" & TypeManuf == "manufIP19", RefCol2plus1, with = FALSE])
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "Manuf" & TypeManuf == "manufCT", RefCol2plus1, with = FALSE])
  }
  
  #   ServNonMarch : btw 2000-2009 we retropolate with WIOD evolutions based on 2010 FIGARO level. Before 2000 we retropolate with LR-WIOD evolutions based on 2000 level calculated just before.
  for (k in 2009:2000) {
    RefCol1 <- which(colnames(Ref_Sectors) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(DTManuf_tab_ALL) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "WIOD" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol2plus1, with = FALSE])
  }
  for (k in 1999:1965) {
    RefCol1 <- which(colnames(Ref_Sectors) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(DTManuf_tab_ALL) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol1] <- unlist(Ref_Sectors[Col_Country == "DEU" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol1plus1, with = FALSE]) * unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol2, with = FALSE]) / unlist(DTManuf_tab_ALL[Col_Country == "DEU" & MRIO == "LRWIOD" & Lig_Indus == "ServNonMarch" & TypeManuf == "manufCT", RefCol2plus1, with = FALSE])
  }
  
  ### Work on corrected series
  SeriesREF_Tot <- Ref_tot
  SeriesREF_Sectors <- Ref_Sectors
  SeriesREF_Tot <- setDT(SeriesREF_Tot)
  SeriesREF_Sectors <- setDT(SeriesREF_Sectors)
  
  # Put in long format
  SeriesREF_Tot_DT <- melt(SeriesREF_Tot, id.vars = c("Col_Country"), measure.vars = 2:57)
  SeriesREF_Sectors_DT <- melt(SeriesREF_Sectors, id.vars = c("TypeManuf", "Col_Country", "Lig_Indus"), measure.vars = 4:59)
  setnames(SeriesREF_Tot_DT, "variable", "year")
  setnames(SeriesREF_Sectors_DT, "variable", "year")
  
  # Add levels (numerator et denominator)
  Niveaux <- MadeIn_levels # readRDS("Sorties/RESULT_MADEINs_Manuf_Niveau.rds")
  P1 <- Niveaux[MRIO == "LRWIOD" & year %in% 1965:1999 & TypeManuf == "manufCT", ]
  P2 <- Niveaux[MRIO == "WIOD" & year %in% 2000:2009 & TypeManuf == "manufCT", ]
  P3 <- Niveaux[MRIO == "FIGARO" & year %in% 2010:2020 & TypeManuf == "manufCT", ]
  Niveaux <- rbind(P1, P2, P3)
  NiveauxTot <- Niveaux[, MRIO := NULL][, TypeManuf := NULL]
  NiveauxTot_denom <- NiveauxTot[, sum(denominateur), by = c("year", "Col_Country")]
  NiveauxTot_num <- NiveauxTot[, sum(numerateur), by = c("year", "Col_Country")]
  NiveauxTot_numdenom <- rbind(NiveauxTot_denom[, TypeRatio := "denominateur"], NiveauxTot_num[, TypeRatio := "numerateur"])
  setnames(NiveauxTot_numdenom, "V1", "value")
  NiveauxTot_numdenom_tab <- dcast(NiveauxTot_numdenom, year + Col_Country ~ TypeRatio, var.value = "value") # Pour contrôle
  NiveauxTot_numdenom_tab$MadeIn <- NiveauxTot_numdenom_tab$numerateur / NiveauxTot_numdenom_tab$denominateur # Pour contrôle
  
  ##### joint
  # On the total
  SeriesREF_Tot_DT <- na.omit(SeriesREF_Tot_DT)
  SeriesREF_Tot_DT <- SeriesREF_Tot_DT[, year := as.factor(year)]
  NiveauxTot_numdenom_tab <- NiveauxTot_numdenom_tab[, year := as.factor(year)]
  Joint_Tot <- NiveauxTot_numdenom_tab[SeriesREF_Tot_DT, on = .(Col_Country, year)]
  setnames(Joint_Tot, "numerateur", "OLD_numerateur")
  Joint_Tot <- Joint_Tot[, NEW_numerateur := denominateur * value]
  setnames(Joint_Tot, "MadeIn", "OLD_MadeIn")
  
  # by industries
  SeriesREF_Sectors_DT <- na.omit(SeriesREF_Sectors_DT)
  SeriesREF_Sectors_DT <- SeriesREF_Sectors_DT[, year := as.factor(year)]
  Niveaux <- Niveaux[, year := as.factor(year)]
  Joint_Sectors <- Niveaux[SeriesREF_Sectors_DT, on = .(Col_Country, year, Lig_Indus)]
  setnames(Joint_Sectors, "numerateur", "OLD_numerateur")
  Joint_Sectors <- Joint_Sectors[, NEW_numerateur := denominateur * value]
  setnames(Joint_Sectors, "RatioMadeIn", "OLD_RatioMadeIn")
  
  # Sauvegarde en RDS de la table des totaux et de celle par branche
  setcolorder(Joint_Tot, c("year", "Col_Country", "denominateur", "NEW_numerateur", "value", "OLD_MadeIn", "OLD_numerateur"))
  setcolorder(Joint_Sectors, c("year", "Col_Country", "TypeManuf", "Lig_Indus", "denominateur", "NEW_numerateur", "value", "OLD_RatioMadeIn", "OLD_numerateur"))
  
  if (OptSaveRDS == TRUE) {
    saveRDS(Joint_Tot, "RESULT_MADEINs_TotauxAvecNiveaux.rds")
    saveRDS(Joint_Sectors, "RESULT_MADEINs_SectorsAvecNiveaux.rds")
  }
  
  out_list <- list(Joint_Tot = Joint_Tot, Joint_Sectors = Joint_Sectors)
  
  return(out_list)
}

#' Build_MadeIn_byOrigin
#'  Build contents of VA by country of origin
#'
#' @param BDn_LR_WIOD datatable
#' @param BDn_WIOD datatable
#' @param BDn_FIG datatable
#' @param BDn_ICIO datatable
#' @param SelectCountry text country
#' @param OptSaveRDS binary
#'
#' @return dl data long
#' @export
Build_MadeIn_byOrigin <- function(BDn_LR_WIOD, BDn_WIOD, BDn_FIG, BDn_ICIO, SelectCountry = "FRA", OptSaveRDS = FALSE) {
  Interm_LRWIOD <- BDn_LR_WIOD # readRDS("Sorties/BDn_LR_WIOD.rds")
  Interm_WIOD <- BDn_WIOD # readRDS("Sorties/BDn_WIOD.rds")
  Interm_FIG <- BDn_FIG # readRDS("Sorties/BDn_FIG.rds")
  Interm_ICIO <- BDn_ICIO # readRDS("Sorties/BDn_ICIO.rds")
  
  # Initialization
  List_Interm <- CompoMRIO(Interm_LRWIOD, "OptFullOptionsBonus", date = 1965)
  List_Interm <- av_extend_MRIO_dw(MRIO_dw = List_Interm, "LRWIOD", TypExtension = "StressVA")
  List_Contenus <- Contenus(List_Interm, typeContenu = "VA", MethContenu = "MatDF", EmprPays = SelectCountry)
  DT <- List_Contenus[["MatEmpreinte_dt"]][, year := 1965]
  DT_byCountry <- ReqSum(DT, c("Lig_Indus", "Col_Indus"))
  Base_byCountry <- DT_byCountry[, base := "LRWIOD"]
  
  # Loop LRWIOD
  for (k in 1966:2000) {
    List_Interm <- CompoMRIO(Interm_LRWIOD, "OptFullOptionsBonus", date = k)
    List_Interm <- av_extend_MRIO_dw(MRIO_dw = List_Interm, "LRWIOD", TypExtension = "StressVA")
    List_Contenus <- Contenus(List_Interm, typeContenu = "VA", MethContenu = "MatDF", EmprPays = SelectCountry)
    DT <- List_Contenus[["MatEmpreinte_dt"]][, year := k]
    DT_byCountry <- ReqSum(DT, c("Lig_Indus", "Col_Indus"))
    DT_byCountry <- DT_byCountry[, base := "LRWIOD"]
    Base_byCountry <- rbind(Base_byCountry, DT_byCountry)
    print(paste0("LRWIOD : ", k))
  }
  
  # Loop WIOD
  for (k in 2000:2014) {
    List_Interm <- CompoMRIO(Interm_WIOD, "OptFullOptionsBonus", date = k)
    List_Interm <- av_extend_MRIO_dw(MRIO_dw = List_Interm, "WIOD", TypExtension = "StressVA")
    List_Contenus <- Contenus(List_Interm, typeContenu = "VA", MethContenu = "MatDF", EmprPays = SelectCountry)
    DT <- List_Contenus[["MatEmpreinte_dt"]][, year := k]
    DT_byCountry <- ReqSum(DT, c("Lig_Indus", "Col_Indus"))
    DT_byCountry <- DT_byCountry[, base := "WIOD"]
    Base_byCountry <- rbind(Base_byCountry, DT_byCountry)
    print(paste0("WIOD : ", k))
  }
  
  # Loop FIGARO
  for (k in 2010:2020) {
    List_Interm <- CompoMRIO(Interm_FIG, "OptFullOptionsBonus", date = k)
    List_Interm <- av_extend_MRIO_dw(MRIO_dw = List_Interm, "FIGARO", TypExtension = "StressVA")
    List_Contenus <- Contenus(List_Interm, typeContenu = "VA", MethContenu = "MatDF", EmprPays = SelectCountry)
    DT <- List_Contenus[["MatEmpreinte_dt"]][, year := k]
    DT_byCountry <- ReqSum(DT, c("Lig_Indus", "Col_Indus"))
    DT_byCountry <- DT_byCountry[, base := "FIGARO"]
    Base_byCountry <- rbind(Base_byCountry, DT_byCountry)
    print(paste0("FIG : ", k))
  }
  
  # Loop ICIO
  for (k in 1995:2018) { # 1995:2018
    List_Interm <- CompoMRIO(Interm_ICIO, "OptFullOptionsBonus", date = k)
    List_Interm <- av_extend_MRIO_dw(MRIO_dw = List_Interm, "ICIO", TypExtension = "StressVA")
    List_Contenus <- Contenus(List_Interm, typeContenu = "VA", MethContenu = "MatDF", EmprPays = SelectCountry)
    DT <- List_Contenus[["MatEmpreinte_dt"]][, year := k]
    DT_byCountry <- ReqSum(DT, c("Lig_Indus", "Col_Indus"))
    DT_byCountry <- DT_byCountry[, base := "ICIO"]
    Base_byCountry <- rbind(Base_byCountry, DT_byCountry)
    print(paste0("ICIO : ", k))
  }
  
  if (OptSaveRDS == TRUE) {
    saveRDS(Base_byCountry, "RESULT_MADEINs_byCountry.rds")
  }
  
  return(Base_byCountry)
}

#' MadeIn_byOrigin
#' Calculate MAdeIn by country of origin, for analytics
#'
#' @param RESULT_MADEINs_byCountry datatable
#' @param SelectCountries list
#' @param MadeInOf text country 
#' @param OptSaveRDS binary
#'
#' @return dl data long
#' @export
MadeIn_byOrigin <- function(RESULT_MADEINs_byCountry, SelectCountries = c("FRA", "DEU", "GBR", "ESP", "ITA", "USA", "CHN"), MadeInOf = "FRA", OptSaveRDS = FALSE) {
  Select_Country <- SelectCountries
  
  DT <- RESULT_MADEINs_byCountry # readRDS("Sorties/RESULT_MADEINs_byCountry.rds")
  DT_pays <- DT[Col_Country == MadeInOf & Lig_Country != MadeInOf, ]
  DT_pays_TotByCountry <- DT_pays[, sum(value), by = c("year", "base")]
  DT_pays_tab <- dcast(DT_pays, Lig_Country + base ~ year, var.value = "value")
  DT_pays <- DT_pays[DT_pays_TotByCountry, on = .(year, base)]
  DT_pays <- DT_pays[, V1 := value / V1 * 100]
  setnames(DT_pays, "V1", "valuePct")
  DT_pays_Pct <- DT_pays[, value := NULL]
  DT_pays_tabPct <- dcast(DT_pays_Pct, Lig_Country + base ~ year, var.value = "valuePct")
  
  DT_pays_Pct_LRWIOD <- DT_pays_Pct[base == "LRWIOD" & year %in% 1965:1999 & Lig_Country %in% Select_Country, ]
  DT_pays_Pct_WIOD <- DT_pays_Pct[base == "WIOD" & year %in% 2000:2009 & Lig_Country %in% Select_Country, ]
  DT_pays_Pct_FIGARO <- DT_pays_Pct[base == "FIGARO" & year %in% 2010:2020 & Lig_Country %in% Select_Country, ]
  DT_pays_Pct_select <- rbind(DT_pays_Pct_LRWIOD, DT_pays_Pct_WIOD, DT_pays_Pct_FIGARO)
  
  DT_pays_Pct_Interm <- DT_pays_Pct_select[, sum(valuePct), by = c("year", "Col_Country", "base")]
  DT_pays_Pct_Interm <- DT_pays_Pct_Interm[, valuePct := 100 - V1][, Lig_Country := "ROW"]
  DT_pays_Pct_Interm <- DT_pays_Pct_Interm[, V1 := NULL]
  DT_pays_Pct_select <- rbind(DT_pays_Pct_select, DT_pays_Pct_Interm)
  
  DT_pays_Pct_select_tab <- dcast(DT_pays_Pct_select, year ~ Lig_Country, value.var = "valuePct")
  
  return(DT_pays_Pct_select_tab)
}


#' MadeIn_Retropolation_UE27
#' Retropolation of made-in to fit long series inter-MRIO
#'
#' @param resMadeInUE27_LRWIOD datatable
#' @param resMadeInUE27_WIOD datatable
#' @param resMadeInUE27_FIGARO datatable
#' @param resMadeInUE27_ICIO datatable
#' @param OptSaveRDS binary
#'
#' @return dl data long
#' @export
MadeIn_Retropolation_UE27 <- function(resMadeInUE27_LRWIOD, resMadeInUE27_WIOD, resMadeInUE27_FIGARO, resMadeInUE27_ICIO, OptSaveRDS = FALSE) {
  # Chargemet des tables
  MADEINs_LRWIOD <- resMadeInUE27_LRWIOD
  MADEINs_WIOD <- resMadeInUE27_WIOD
  MADEINs_FIGARO <- resMadeInUE27_FIGARO
  MADEINs_ICIO <- resMadeInUE27_ICIO # readRDS("Sorties/RESULT_MADEINsUE27_ICIO.rds")
  
  Full_madeIn <- rbind(MADEINs_LRWIOD[, MRIO := "LRWIOD"], MADEINs_WIOD[, MRIO := "WIOD"], MADEINs_FIGARO[, MRIO := "FIG"], MADEINs_ICIO[, MRIO := "ICIO"])
  
  Full_madeInTot <- Full_madeIn[, sum(value), by = c("MRIO", "year", "position", "Col_Country")]
  setnames(Full_madeInTot, "V1", "value")
  
  # Calculation of made-in ratio
  Full_madeInTab <- dcast(Full_madeInTot, year + Col_Country + MRIO ~ position, value.var = "value")
  Full_madeInTab$MadeIn <- Full_madeInTab$numerateur / Full_madeInTab$denominateur
  
  # Transform to wide
  List_selectCountries <- list("CHN", "JPN", "UE27", "USA")
  
  #  dcast COuntries+MRIO~year et calculer la ref avec juste les années de référence pour les 3 bases ensuite rétropoler
  P1 <- Full_madeInTab[MRIO == "LRWIOD" & year %in% 1965:1999, ]
  P2 <- Full_madeInTab[MRIO == "WIOD" & year %in% 2000:2009, ]
  P3 <- Full_madeInTab[MRIO == "FIG" & year %in% 2010:2020, ]
  Full_madeInTab_REF <- rbind(P1, P2, P3)
  Tab_REF <- dcast(Full_madeInTab_REF, Col_Country ~ year, value.var = "MadeIn")
  
  Tab_w <- dcast(Full_madeInTab, Col_Country + MRIO ~ year, value.var = "MadeIn")
  
  # JPN : before 2000 retropolate with LR-WIOD ecvolutions based on 2000 WIOD levels.
  for (k in 1999:1965) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Tab_REF[Col_Country == "JPN", RefCol1] <- unlist(Tab_REF[Col_Country == "JPN", RefCol1plus1, with = FALSE]) * unlist(Tab_w[Col_Country == "JPN" & MRIO == "LRWIOD", RefCol2, with = FALSE]) / unlist(Tab_w[Col_Country == "JPN" & MRIO == "LRWIOD", RefCol2plus1, with = FALSE])
  }
  
  # UE27 : before 2000 retropolate with LR-WIOD ecvolutions based on 2000 WIOD levels. Btw 2010-2014 keep WIOD levels, after 2014 extrapolate with FIGARO evolutions based on 2014 WIOD level
  for (k in 1999:1965) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1plus1 <- RefCol1 + 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2plus1 <- RefCol2 + 1
    Tab_REF[Col_Country == "UE27", RefCol1] <- unlist(Tab_REF[Col_Country == "UE27", RefCol1plus1, with = FALSE]) * unlist(Tab_w[Col_Country == "UE27" & MRIO == "LRWIOD", RefCol2, with = FALSE]) / unlist(Tab_w[Col_Country == "UE27" & MRIO == "LRWIOD", RefCol2plus1, with = FALSE])
  }
  Tab_REF[Col_Country == "UE27", c("2010", "2011", "2012", "2013", "2014")] <- Tab_w[Col_Country == "UE27" & MRIO == "WIOD", c("2010", "2011", "2012", "2013", "2014")]
  for (k in 2015:2020) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[Col_Country == "UE27", RefCol1] <- unlist(Tab_REF[Col_Country == "UE27", RefCol1moins1, with = FALSE]) * unlist(Tab_w[Col_Country == "UE27" & MRIO == "FIG", RefCol2, with = FALSE]) / unlist(Tab_w[Col_Country == "UE27" & MRIO == "FIG", RefCol2moins1, with = FALSE])
  }
  
  # USA : after 2010 : extrapolate with FIGARO evolutions based on 2010 WIOD level
  Tab_REF[Col_Country == "USA", "2010"] <- Tab_w[Col_Country == "USA" & MRIO == "WIOD", "2010"]
  for (k in 2011:2020) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[Col_Country == "USA", RefCol1] <- unlist(Tab_REF[Col_Country == "USA", RefCol1moins1, with = FALSE]) * unlist(Tab_w[Col_Country == "USA" & MRIO == "FIG", RefCol2, with = FALSE]) / unlist(Tab_w[Col_Country == "USA" & MRIO == "FIG", RefCol2moins1, with = FALSE])
  }
  
  # Put in long format
  Tab_REF_dl <- melt(Tab_REF, id.vars = c("Col_Country"), measure.vars = 2:57)
  setnames(Tab_REF_dl, "variable", "year")
  
  if (OptSaveRDS == TRUE) {
    saveRDS(Tab_REF_dl, "RESULT_MADEINs_UE27.rds")
  }
  
  out <- Tab_REF_dl
  
  return(out)
}
#' RatioNoguera
#' Calculation of Noguera indicator
#'
#' @param BaseResult datatable
#' @param nomMRIO text name MRIO
#'
#' @return dl data long
#' @export
RatioNoguera <- function(BaseResult, nomMRIO) {
  CVAEx_ECOLE <- data.table::copy(BaseResult)
  CVAEx_ECOLE_sel <- CVAEx_ECOLE[Compo %in% c("VAXnoguera_PR", "Export"), ] #  c("VAXnoguera_PR","VA")
  CVAEx_ECOLE_sel_tot <- CVAEx_ECOLE_sel[, .(sum(value)), by = c("yearREF", "CountryREF", "Compo")]
  CVAEx_ECOLE_sel_tot <- rbind(CVAEx_ECOLE_sel_tot, CVAEx_ECOLE_sel[, .(sum(value)), by = c("yearREF", "Compo")][, CountryREF := "ZZWORLD"])
  CVAEx_ECOLE_sel_tot_tab <- dcast(unique(CVAEx_ECOLE_sel_tot), CountryREF + yearREF ~ Compo, value.var = "V1")
  CVAEx_ECOLE_sel_tot_tab$ratioNoguera <- 100 * CVAEx_ECOLE_sel_tot_tab$VAXnoguera_PR / CVAEx_ECOLE_sel_tot_tab$Export
  CVAEx_ECOLE_sel_tot_tab <- GereInfNA(CVAEx_ECOLE_sel_tot_tab)
  CVAEx_ECOLE_sel_tot_tab[, MRIO := nomMRIO]
  return(CVAEx_ECOLE_sel_tot_tab)
}

#' ContentVAExports_Retropolation
#' Retropolation of content in value added of exports WIOD is the reference and LRWIOD is used to retropolate backward, and FIGARO is used to retropolate forward.
#' ICIO is not used here because of systematic retropolation method
#' Very specific task : can be adapted depending on data you're using
#'
#' @param ContVAdesExports_LRWIOD datatable
#' @param ContVAdesExports_WIOD datatable
#' @param ContVAdesExports_FIGARO datatable
#' @param OptSaveRDS binary
#'
#' @return dl data long
#' @export
ContentVAExports_Retropolation <- function(ContVAdesExports_LRWIOD, ContVAdesExports_WIOD, ContVAdesExports_FIGARO, OptSaveRDS = FALSE) {
  # Loading data
  BASEres_ContVAdesExports_LRWIOD <- ContVAdesExports_LRWIOD
  BASEres_ContVAdesExports_WIOD <- ContVAdesExports_WIOD
  BASEres_ContVAdesExports_FIGARO <- ContVAdesExports_FIGARO
  gc()
  
  
  
  NogLRWIOD <- RatioNoguera(BASEres_ContVAdesExports_LRWIOD, "LRWIOD")
  NogWIOD <- RatioNoguera(BASEres_ContVAdesExports_WIOD, "WIOD")
  NogFIRAGO <- RatioNoguera(BASEres_ContVAdesExports_FIGARO, "FIGARO")
  
  Base_Noguera <- rbind(NogLRWIOD, NogWIOD, NogFIRAGO)
  Base_NogueraREF <- data.table::copy(Base_Noguera)
  
  Base_NogueraREF[, MRIOCountry := paste0(CountryREF, "_", MRIO)]
  
  Base_Noguera_tab <- dcast(Base_NogueraREF[MRIO %in% c("LRWIOD", "WIOD", "FIGARO"), ], MRIOCountry ~ yearREF, value.var = "ratioNoguera")
  gc()
  ##################### Retropolation / Extrapolation
  # Connection/retropolation
  # Our reference is WIOD: we retropolate backwards with LRWIOD before 2000 and forwards with FIGARO after 2014.  DT<-Base_NogueraREF
  DT<-Base_Noguera_tab
  DT$CountryREF <- stringr::str_extract(DT$MRIOCountry, "[^_]+")
  DT$MRIO <- substring(stringr::str_extract(DT$MRIOCountry, "_.+$"), 2)
  
  DT_tab<-cbind(DT[,c("CountryREF","MRIO")],DT[,2:(ncol(DT)-2)])
  
  DF_w <- setDF(DT_tab)
  
  # retropolation of totals (take care to use the middle part to process the 2 directions) :
  for (lig in 3:118) {
    for (col in 44:58) {
      datee <- 2020 - 58 + col
      if (DF_w[lig, 2] == "WIOD" & datee > 2014) {
        DF_w[lig, col] <- DF_w[lig, (col - 1)] * DF_w[(lig - 2), col] / DF_w[(lig - 2), (col - 1)]
      }
    }
    # reverse
    for (col in 44:3) {
      datee <- 2020 - 58 + col
      if (DF_w[lig, 2] == "WIOD" & DF_w[(lig - 1), 2] == "LRWIOD" & datee < 2000) {
        DF_w[lig, col] <- DF_w[lig, (col + 1)] * DF_w[(lig - 1), col] / DF_w[(lig - 1), (col + 1)]
      }
    }
  }
  
  DT_w <- setDT(DF_w)
  DT_res_retro <- DT_w[MRIO == "WIOD", ]
  DT_res_retro[, MRIO := NULL]
  
  DT_res_retro_long <- melt(DT_res_retro, id.vars = c("CountryREF")) # colnames(DT_res_retro)[4:60]
  setnames(DT_res_retro_long, "variable", "year")
  DT_res_retro_long <- na.omit(DT_res_retro_long)
  
  if (OptSaveRDS == TRUE) {
    saveRDS(DT_res_retro_long, "RESULT_ContenusVAExport.rds")
  }
  
  out <- DT_res_retro_long
  
  return(out)
}

#' ContentVAExports_Retropolation_UE27
#' Retropolation of content in value added of exports for UE27 and big areas
#' Need to aggregate data before to have only one area EU
#' Very specific task : can be adapted depending on data you're using
#'
#' @param resMadeInUE27_LRWIOD datatable
#' @param resMadeInUE27_WIOD datatable
#' @param resMadeInUE27_FIGARO datatable
#' @param resMadeInUE27_ICIO datatable
#' @param OptGraph binary
#' @param OptSaveRDS binary
#'
#' @return dl data long
#' @export
ContentVAExports_Retropolation_UE27 <- function(resMadeInUE27_LRWIOD, resMadeInUE27_WIOD, resMadeInUE27_FIGARO, resMadeInUE27_ICIO, OptGraph = FALSE, OptSaveRDS = FALSE) {
  ContVAdesExports_LRWIOD <- resMadeInUE27_LRWIOD
  ContVAdesExports_WIOD <- resMadeInUE27_WIOD
  ContVAdesExports_FIGARO <- resMadeInUE27_FIGARO
  ContVAdesExports_ICIO <- resMadeInUE27_ICIO
  gc()
  
  Full_ContVA <- rbind(ContVAdesExports_LRWIOD[, MRIO := "LRWIOD"], ContVAdesExports_WIOD[, MRIO := "WIOD"], ContVAdesExports_FIGARO[, MRIO := "FIG"], ContVAdesExports_ICIO[, MRIO := "ICIO"])
  
  # Calculation of Noguera ratio
  CVAEx <- data.table::copy(Full_ContVA)
  CVAEx_sel <- CVAEx[Compo %in% c("VAXnoguera_PR", "Export"), ] #  c("VAXnoguera_PR","VA")
  CVAEx_sel_tot <- CVAEx_sel[, .(sum(value)), by = c("yearREF", "CountryREF", "Compo", "MRIO")]
  CVAEx_sel_tot <- rbind(CVAEx_sel_tot, CVAEx_sel[, sum(value), by = c("yearREF", "Compo", "MRIO")][, CountryREF := "ZZWORLD"])
  CVAEx_sel_tot_tab <- dcast(unique(CVAEx_sel_tot), CountryREF + yearREF + MRIO ~ Compo, value.var = "V1")
  CVAEx_sel_tot_tab$ratioNoguera <- 100 * CVAEx_sel_tot_tab$VAXnoguera_PR / CVAEx_sel_tot_tab$Export
  CVAEx_sel_tot_tab <- GereInfNA(CVAEx_sel_tot_tab)
  gc()
  
  if (OptGraph == TRUE) {
    DiffEU27 <- CVAEx_sel_tot_tab[CountryREF == "UE27", ]
    ggplot(data = DiffEU27, aes(x = yearREF, y = ratioNoguera, color = MRIO)) +
      geom_line()
    
    DiffUSA <- CVAEx_sel_tot_tab[CountryREF == "USA", ]
    ggplot(data = DiffUSA, aes(x = yearREF, y = ratioNoguera, color = MRIO)) +
      geom_line()
    
    DiffCHN <- CVAEx_sel_tot_tab[CountryREF == "CHN", ]
    ggplot(data = DiffCHN, aes(x = yearREF, y = ratioNoguera, color = MRIO)) +
      geom_line()
  }
  
  
  #  # dcast COuntries+MRIO~year and calculate the ref with just the reference years for the 3 bases, then backcast.
  P1 <- CVAEx_sel_tot_tab[MRIO == "LRWIOD" & yearREF %in% 1965:1999, ]
  P2 <- CVAEx_sel_tot_tab[MRIO == "WIOD" & yearREF %in% 2000:2009, ]
  P3 <- CVAEx_sel_tot_tab[MRIO == "FIG" & yearREF %in% 2010:2020, ]
  CVAEx_sel_tot_tab_REF <- rbind(P1, P2, P3)
  Tab_REF <- unique(CVAEx_sel_tot_tab_REF[, c("CountryREF", "yearREF", "ratioNoguera")])
  Tab_REF$ratioNoguera <- as.numeric(Tab_REF$ratioNoguera)
  Tab_REF <- setDT(Tab_REF)
  Tab_REF <- dcast(Tab_REF, CountryREF ~ yearREF, value.var = "ratioNoguera")
  
  Tab_w <- dcast(CVAEx_sel_tot_tab, CountryREF + MRIO ~ yearREF, value.var = "ratioNoguera")
  gc()
  
  # CHN : After 2010 extrapolate with Figaro evolutions based on 2010 WIOD levels
  Tab_REF[CountryREF == "CHN", "2010"] <- Tab_w[CountryREF == "CHN" & MRIO == "WIOD", "2010"]
  for (k in 2011:2020) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[CountryREF == "CHN", RefCol1] <- unlist(Tab_REF[CountryREF == "CHN", RefCol1moins1, with = FALSE]) * unlist(Tab_w[CountryREF == "CHN" & MRIO == "FIG", RefCol2, with = FALSE]) / unlist(Tab_w[CountryREF == "CHN" & MRIO == "FIG", RefCol2moins1, with = FALSE])
  }
  
  # JPN : btw 2001 and 2010 : extrapolate forward with WIOD evolutions based on 2000 LR-WIOD level
  Tab_REF[CountryREF == "JPN", "2000"] <- Tab_w[CountryREF == "JPN" & MRIO == "LRWIOD", "2000"]
  for (k in 2001:2010) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[CountryREF == "JPN", RefCol1] <- unlist(Tab_REF[CountryREF == "JPN", RefCol1moins1, with = FALSE]) * unlist(Tab_w[CountryREF == "JPN" & MRIO == "WIOD", RefCol2, with = FALSE]) / unlist(Tab_w[CountryREF == "JPN" & MRIO == "WIOD", RefCol2moins1, with = FALSE])
  }
  
  # UE27 : Btw 2010-2011 keep WIOD levels, after 2011 keep FIGARO levels
  Tab_REF[CountryREF == "UE27", c("2010", "2011")] <- Tab_w[CountryREF == "UE27" & MRIO == "WIOD", c("2010", "2011")]
  for (k in 2012:2020) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[CountryREF == "UE27", RefCol1] <- unlist(Tab_REF[CountryREF == "UE27", RefCol1moins1, with = FALSE]) * unlist(Tab_w[CountryREF == "UE27" & MRIO == "FIG", RefCol2, with = FALSE]) / unlist(Tab_w[CountryREF == "UE27" & MRIO == "FIG", RefCol2moins1, with = FALSE])
  }
  
  # USA : btw 2001-2014 extrapolate forward with WIOD evolutions based on 2000 LR-WIOD level. btw 2015-2018 extrapolate forward with ICIO evolutions (exceptionnaly) based on 2014 level previously calculated. After 2019 : extrapolate forward with FIGARO evolutions based on 2018 level previously calculated.
  Tab_REF[CountryREF == "USA", "2000"] <- Tab_w[CountryREF == "USA" & MRIO == "LRWIOD", "2000"]
  for (k in 2001:2014) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[CountryREF == "USA", RefCol1] <- unlist(Tab_REF[CountryREF == "USA", RefCol1moins1, with = FALSE]) * unlist(Tab_w[CountryREF == "USA" & MRIO == "WIOD", RefCol2, with = FALSE]) / unlist(Tab_w[CountryREF == "USA" & MRIO == "WIOD", RefCol2moins1, with = FALSE])
  }
  for (k in 2015:2018) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[CountryREF == "USA", RefCol1] <- unlist(Tab_REF[CountryREF == "USA", RefCol1moins1, with = FALSE]) * unlist(Tab_w[CountryREF == "USA" & MRIO == "ICIO", RefCol2, with = FALSE]) / unlist(Tab_w[CountryREF == "USA" & MRIO == "ICIO", RefCol2moins1, with = FALSE])
  }
  for (k in 2019:2020) {
    RefCol1 <- which(colnames(Tab_REF) == toString(k))
    RefCol1moins1 <- RefCol1 - 1
    RefCol2 <- which(colnames(Tab_w) == toString(k))
    RefCol2moins1 <- RefCol2 - 1
    Tab_REF[CountryREF == "USA", RefCol1] <- unlist(Tab_REF[CountryREF == "USA", RefCol1moins1, with = FALSE]) * unlist(Tab_w[CountryREF == "USA" & MRIO == "FIG", RefCol2, with = FALSE]) / unlist(Tab_w[CountryREF == "USA" & MRIO == "FIG", RefCol2moins1, with = FALSE])
  }
  
  # Put in long format
  Tab_REF_dl <- melt(Tab_REF, id.vars = c("CountryREF"), measure.vars = 2:57)
  setnames(Tab_REF_dl, "variable", "year")
  
  if (OptSaveRDS == TRUE) {
    saveRDS(Tab_REF_dl, "RESULT_ContenusVAExport_UE27.rds")
  }
  
  out <- Tab_REF_dl
  
  return(out)
}


#' BoucleLinkageBwdFwd
#' Function loop to calculate Backward and Forward Linkage indicators databases
#'
#' @param base_dt datatable
#' @param period time period
#'
#' @return dl data long
#' @export
BoucleLinkageBwdFwd <- function(base_dt, period) {
  for (k in period) {
    if (k == period[[1]]) { # initialization
      List_interm <- LinkageBwdFwd(base_dt, k)
      Base_dt1 <- List_interm[["Indic_Bwd_byCountry"]][, year := k]
      Base_dt2 <- List_interm[["Indic_Fwd_byCountry"]][, year := k]
      Base_dt3 <- List_interm[["Indic_Bwd_tot"]][, year := k]
      Base_dt4 <- List_interm[["Indic_Fwd_tot"]][, year := k]
      Base_dt5 <- List_interm[["Indic_Bwd_Prod"]][, year := k]
      Base_dt6 <- List_interm[["Indic_Fwd_Prod"]][, year := k]
      Base_dt7 <- List_interm[["Bwd_Partdom"]][, year := k]
      Base_dt8 <- List_interm[["Fwd_Partdom"]][, year := k]
    } else {
      List_interm <- LinkageBwdFwd(base_dt, k)
      dt1 <- List_interm[["Indic_Bwd_byCountry"]][, year := k]
      dt2 <- List_interm[["Indic_Fwd_byCountry"]][, year := k]
      dt3 <- List_interm[["Indic_Bwd_tot"]][, year := k]
      dt4 <- List_interm[["Indic_Fwd_tot"]][, year := k]
      dt5 <- List_interm[["Indic_Bwd_Prod"]][, year := k]
      dt6 <- List_interm[["Indic_Fwd_Prod"]][, year := k]
      dt7 <- List_interm[["Bwd_Partdom"]][, year := k]
      dt8 <- List_interm[["Fwd_Partdom"]][, year := k]
      
      Base_dt1 <- rbind(Base_dt1, dt1)
      Base_dt2 <- rbind(Base_dt2, dt2)
      Base_dt3 <- rbind(Base_dt3, dt3)
      Base_dt4 <- rbind(Base_dt4, dt4)
      Base_dt5 <- rbind(Base_dt5, dt5)
      Base_dt6 <- rbind(Base_dt6, dt6)
      Base_dt7 <- rbind(Base_dt7, dt7)
      Base_dt8 <- rbind(Base_dt8, dt8)
      
      Base_Out <- list(Indic_Bwd_byCountry = Base_dt1, Indic_Fwd_byCountry = Base_dt2, Indic_Bwd_tot = Base_dt3, Indic_Fwd_tot = Base_dt4, Indic_Bwd_Prod = Base_dt5, Indic_Fwd_Prod = Base_dt6, Indic_Bwd_PartDOM = Base_dt7, Indic_Fwd_PartDOM = Base_dt8)
      print(paste0("year = ", k))
    }
  }
  return(Base_Out)
}



#' LinkageBwdFwd
#' Function Backward and Forward Linkage
#'
#' @param base_dt datatable
#' @param annee year
#'
#' @return dl data long
#' @export
LinkageBwdFwd <- function(base_dt, annee) {
  inter_listTab <- CompoMRIO(base_dt, typeCompo = "OptFullOptionsBonus", date = annee)
  
  L_dt <- setDT(AddRownamesToFirstCol(inter_listTab[["L"]]))
  L_dt <- melt(L_dt)
  setnames(L_dt, "joint", "PR")
  setnames(L_dt, "variable", "BR")
  L_dt_avecDiag <- L_dt
  L_dt_avecDiag <- SplitPRBR(L_dt_avecDiag)
  # L_dt<-L_dt[PR==BR,value:=0] # total linkage if 0 but we prefer keep the multiplicator concept
  L_dt <- SplitPRBR(L_dt)
  
  G_dt <- setDT(AddRownamesToFirstCol(inter_listTab[["InvBGhosh"]]))
  G_dt <- melt(G_dt)
  setnames(G_dt, "joint", "PR")
  setnames(G_dt, "variable", "BR")
  G_dt_avecDiag <- G_dt
  G_dt_avecDiag <- SplitPRBR(G_dt_avecDiag)
  # G_dt<-G_dt[PR==BR,value:=0] # total linkage if 0 but we prefer keep the multiplicator concept
  G_dt <- SplitPRBR(G_dt)
  
  # Sum : numerator
  Bwd_num_avecDiag <- L_dt_avecDiag[, sum(value), by = c("Col_Country", "Col_Indus")]
  Fwd_num_avecDiag <- G_dt_avecDiag[, sum(value), by = c("Lig_Country", "Lig_Indus")]
  
  Bwd_num_dom_avecDiag <- L_dt_avecDiag[Lig_Country == Col_Country, sum(value), by = c("Col_Country", "Col_Indus")]
  Bwd_num_imp_avecDiag <- L_dt_avecDiag[Lig_Country != Col_Country, sum(value), by = c("Col_Country", "Col_Indus")]
  Fwd_num_dom_avecDiag <- G_dt_avecDiag[Lig_Country == Col_Country, sum(value), by = c("Lig_Country", "Lig_Indus")]
  Fwd_num_imp_avecDiag <- G_dt_avecDiag[Lig_Country != Col_Country, sum(value), by = c("Lig_Country", "Lig_Indus")]
  
  Bwd_num <- L_dt[, sum(value), by = c("Col_Country", "Col_Indus")]
  Fwd_num <- G_dt[, sum(value), by = c("Lig_Country", "Lig_Indus")]
  
  Bwd_num_dom <- L_dt[Lig_Country == Col_Country, sum(value), by = c("Col_Country", "Col_Indus")]
  Bwd_num_imp <- L_dt[Lig_Country != Col_Country, sum(value), by = c("Col_Country", "Col_Indus")]
  Fwd_num_dom <- G_dt[Lig_Country == Col_Country, sum(value), by = c("Lig_Country", "Lig_Indus")]
  Fwd_num_imp <- G_dt[Lig_Country != Col_Country, sum(value), by = c("Lig_Country", "Lig_Indus")]
  
  Bwd_num_Partdom <- Bwd_num_dom # init
  Bwd_num_Partdom$V1 <- Bwd_num_dom$V1 / Bwd_num$V1
  setnames(Bwd_num_Partdom, "V1", "Part_Dom")
  Bwd_num_Partdom <- GereInfNA(Bwd_num_Partdom)
  Fwd_num_Partdom <- Fwd_num_dom # init
  Fwd_num_Partdom$V1 <- Fwd_num_dom$V1 / Fwd_num$V1
  setnames(Fwd_num_Partdom, "V1", "Part_Dom")
  Fwd_num_Partdom <- GereInfNA(Fwd_num_Partdom)
  
  
  # Sum : denominator
  Bwd_Tot_denom <- L_dt[, sum(value)]
  Fwd_Tot_denom <- G_dt[, sum(value)]
  Bwd_Prod_denom <- inter_listTab[["PROD"]]
  Fwd_Prod_denom <- inter_listTab[["PROD"]]
  setnames(Fwd_Prod_denom, "Lig_Country", "Col_Country")
  setnames(Fwd_Prod_denom, "Lig_Indus", "Col_Indus")
  Bwd_byCountry_denom <- L_dt[, sum(value), by = "Col_Country"]
  Fwd_byCountry_denom <- G_dt[, sum(value), by = "Lig_Country"]
  
  # Normalized indicators
  Indic_Bwd_tot <- Bwd_num # Init
  Indic_Bwd_tot[, 3] <- Bwd_num[, 3] / Bwd_Tot_denom
  Indic_Fwd_tot <- Fwd_num # Init
  Indic_Fwd_tot[, 3] <- Fwd_num[, 3] / Fwd_Tot_denom
  
  setnames(Fwd_num, c("Lig_Country", "Lig_Indus"), c("Col_Country", "Col_Indus"))
  Indic_Fwd_Prod <- Fwd_Prod_denom[Fwd_num, on = .(Col_Country, Col_Indus)] # Init
  Indic_Fwd_Prod <- Indic_Fwd_Prod[, value := V1 / value]
  Indic_Fwd_Prod <- Indic_Fwd_Prod[, c("Col_Country", "Col_Indus", "value")]
  setnames(Bwd_Prod_denom, "Col_Country", "Lig_Country")
  setnames(Bwd_Prod_denom, "Col_Indus", "Lig_Indus")
  setnames(Bwd_num, c("Col_Country", "Col_Indus"), c("Lig_Country", "Lig_Indus"))
  Indic_Bwd_Prod <- Bwd_Prod_denom[Bwd_num, on = .(Lig_Country, Lig_Indus)] # Init
  Indic_Bwd_Prod <- Indic_Bwd_Prod[, value := V1 / value]
  Indic_Bwd_Prod <- Indic_Bwd_Prod[, c("Lig_Country", "Lig_Indus", "value")]
  
  setnames(Fwd_byCountry_denom, "Lig_Country", "Col_Country")
  Indic_Fwd_byCountry <- Fwd_byCountry_denom[Fwd_num, on = .(Col_Country)] # Init
  Indic_Fwd_byCountry <- Indic_Fwd_byCountry[, value := i.V1 / V1]
  Indic_Fwd_byCountry <- Indic_Fwd_byCountry[, c("Col_Country", "Col_Indus", "value")]
  setnames(Bwd_byCountry_denom, "Col_Country", "Lig_Country")
  Indic_Bwd_byCountry <- Bwd_byCountry_denom[Bwd_num, on = .(Lig_Country)] # Init
  Indic_Bwd_byCountry <- Indic_Bwd_byCountry[, value := i.V1 / V1]
  Indic_Bwd_byCountry <- Indic_Bwd_byCountry[, c("Lig_Country", "Lig_Indus", "value")]
  
  List_out <- list(Indic_Bwd_byCountry = Indic_Bwd_byCountry, Indic_Fwd_byCountry = Indic_Fwd_byCountry, Indic_Bwd_tot = Indic_Bwd_tot, Indic_Fwd_tot = Indic_Fwd_tot, Indic_Bwd_Prod = Indic_Bwd_Prod, Indic_Fwd_Prod = Indic_Fwd_Prod, Bwd_Partdom = Bwd_num_Partdom, Fwd_Partdom = Fwd_num_Partdom, Bwd_brut = Bwd_num_avecDiag, Bwd_DOM_brut = Bwd_num_dom_avecDiag, Bwd_IMP_brut = Bwd_num_imp_avecDiag, Fwd_brut = Fwd_num_avecDiag, Fwd_DOM_brut = Fwd_num_dom_avecDiag, Fwd_IMP_brut = Fwd_num_imp_avecDiag)
  
  return(List_out)
}
#' Herfindahl
#' Function Herfindahl (concentration indicator)
#'
#' @param MRIOinterm MRIO object
#' @param verbose binary
#'
#' @return dl data long
#' @export
Herfindahl <- function(MRIOinterm, verbose = F) {
  CI_dt <- MRIOinterm[["CI"]]
  CI_dt <- CI_dt[Lig_Country != Col_Country, ] # Out of diag
  CI_dt[, MRIO := NULL]
  
  # Calculation of structure
  StructSubstit <- MRIOinterm[["CI"]][Lig_Country != Col_Country, ]
  StructSubstit[, dispers := sd(value, na.rm = T) / mean(value, na.rm = T), by = c("Col_Country", "Col_Indus", "Lig_Indus")] # Calcul indic dispersion par pays de provenance des importations pour chaque produit importe
  StructSubstit[, dispersRelative := dispers / mean(dispers, na.rm = T), by = c("Lig_Country", "Col_Indus", "Lig_Indus")] # Calcul de la dispersion relative : ratio entre la dispersion des importations d un e branche donnée et la moyenne des dispersions pour les branches équivalentes de l'ensemble des pays (48)
  StructSubstit <- GereInfNA(StructSubstit)
  StructSubstit_byIndus <- StructSubstit[, mean(dispersRelative, na.rm = T), by = c("Lig_Indus", "Col_Indus", "Col_Country")] # Calcul de la moyenne de ces dispersion relatives (intermediaire de calcul pour normaliser les résultats car a l'etape precedente l'indicateur de dispersion relative etait calculé hors le propre pays, donc l indicateur differait legerement entre lignes) : on obtient un indicateur unique.
  
  # Add WORLD for comparisons
  CI_TotalWorld <- CI_dt[, sum(value), by = c("Lig_Country", "Col_Indus", "Lig_Indus", "year")]
  setnames(CI_TotalWorld, "V1", "value")
  CI_TotalWorld[, Col_Country := "XXWORLD"]
  CI_dt <- rbind(CI_dt, CI_TotalWorld)
  
  # Add industry total
  CI_TotalColIndus <- CI_dt[, sum(value), by = c("Lig_Country", "Col_Country", "Lig_Indus", "year")]
  setnames(CI_TotalColIndus, "V1", "value")
  CI_TotalColIndus[, Col_Indus := "XXTOTAL"]
  CI_dt <- rbind(CI_dt, CI_TotalColIndus)
  
  CI_byCountry <- CI_dt[, sum(value), by = c("Lig_Country", "Col_Country", "Col_Indus", "year")]
  setnames(CI_byCountry, "V1", "value")
  CI_byIndus <- CI_dt[, sum(value), by = c("Lig_Indus", "Col_Country", "Col_Indus", "year")]
  setnames(CI_byIndus, "V1", "value")
  
  Struct_CI_full <- ReqSum(CI_dt, c("Lig_Country", "Lig_Indus"), OptStruct = T)
  Struct_CI_byCountry <- ReqSum(CI_byCountry[, Lig_Indus := "TOTAL"], c("Lig_Country"), OptStruct = T)
  Struct_CI_byIndus <- ReqSum(CI_byIndus[, Lig_Country := "TOTAL"], c("Lig_Indus"), OptStruct = T)
  
  if (verbose == T) {
    # Controls
    print(Struct_CI_byIndus[, sum(value), by = c("Col_Country", "Col_Indus")])
    print(Struct_CI_byCountry[, sum(value), by = c("Col_Country", "Col_Indus")])
    print(Struct_CI_full[, sum(value), by = c("Col_Country", "Col_Indus")])
  }
  
  # Squared values
  Struct_CI_full$valueSquared <- Struct_CI_full$value * Struct_CI_full$value
  Struct_CI_byCountry$valueSquared <- Struct_CI_byCountry$value * Struct_CI_byCountry$value
  Struct_CI_byIndus$valueSquared <- Struct_CI_byIndus$value * Struct_CI_byIndus$value
  
  # Adjusted Squared values
  Struct_CI_full <- StructSubstit_byIndus[Struct_CI_full, on = .(Col_Country, Col_Indus, Lig_Indus)]
  Struct_CI_full$valueSquaredAdj <- Struct_CI_full$value * Struct_CI_full$value * Struct_CI_full$V1 * Struct_CI_full$V1
  
  # Indicator calculation
  H_full <- Struct_CI_full[, sum(valueSquared), by = c("Col_Country", "Col_Indus")][, IndicH := "full"]
  H_fullAdj <- Struct_CI_full[, sum(valueSquaredAdj), by = c("Col_Country", "Col_Indus")][, IndicH := "fullAdj"]
  H_byCountry <- Struct_CI_byCountry[, sum(valueSquared), by = c("Col_Country", "Col_Indus")][, IndicH := "byCountry"]
  H_byIndus <- Struct_CI_byIndus[, sum(valueSquared), by = c("Col_Country", "Col_Indus")][, IndicH := "byIndus"]
  
  H_Indics <- rbind(H_fullAdj, H_full, H_byCountry, H_byIndus)
  H_Indics[, value := 100 * V1]
  H_Indics[, V1 := NULL]
  
  H_Indics_tab <- dcast(H_Indics, Col_Country + Col_Indus ~ IndicH, value.var = "value")
  H_Indics_tab[Col_Indus == "XXTOTAL", fullAdj := full][Col_Country == "XXWORLD", fullAdj := full] # Car ajustement pas possible sur le total dont donnait NA
  
  res_out <- list(H_Indics = H_Indics, H_Indics_tab = H_Indics_tab, RatioSubstit = StructSubstit_byIndus)
  
  if (verbose == T) {
    print(H_Indics_tab)
  }
  
  return(res_out)
}

#' ImportedContentInVA
#' Function to calculate the imported content in value added
#' it works with 1 country but you can loop all over the contries available in the MRIO.
#'
#' @param pays text country
#'
#' @return dl data long
#' @export
ImportedContentInVA <- function(pays) {
  DF_Pays <- MRIO[["DF"]][Col_Country == pays, sum(value), by = c("Lig_Country", "Lig_Indus", "Col_Country")]
  DF_Pays <- DF_Pays[order(Lig_Country, Lig_Indus)]
  DF_Pays_mat <- matrix(diag(unlist(DF_Pays[, "V1"])), ncol = 2944)
  
  Mat_L <- Prod_dt <- MRIO[["L"]]
  Mat_L_mat <- as.matrix(Mat_L)
  
  Matrice_Empreinte <- CFPcalculationRCPP(f_dt, Mat_L_mat, DF_Pays_mat)
  Matrice_Empreinte[Matrice_Empreinte < 0] <- 0
  Tot_Matrice_Empreinte <- sum(Matrice_Empreinte, na.rm = T)
  
  Matrice_Empreinte <- setDT(as.data.frame(Matrice_Empreinte))
  MaMat <- cbind(MRIO[["CI_tab"]][, 1:3], Matrice_Empreinte)
  colnames(MaMat) <- colnames(MRIO[["CI_tab"]])
  MaMat <- MaMat[, -1]
  MaMat_long <- melt(MaMat, id.vars = c("Lig_Country", "Lig_Indus"))
  MaMat_long$Col_Country <- str_extract(MaMat_long$variable, "[^_]+")
  MaMat_long$Col_Indus <- substring(str_extract(MaMat_long$variable, "_.+$"), 2)
  MaMat_long <- MaMat_long[, variable := NULL]
  MaMat_long <- MaMat_long[Lig_Country != pays, ] # Drop doestic contents
  
  ContenusPays_byBranches <- MaMat_long[, sum(value, na.rm = T), by = c("Lig_Country", "Lig_Indus", "Col_Indus")]
  setnames(ContenusPays_byBranches, "V1", "value")
  
  return(ContenusPays_byBranches[, ContenuDuPays := pays])
}


#' Autarky
#' Function for calculating an autarky situation from a MRIO which is the starting world economy situation
#' The MRIO has already undergone a CompoMRIO, and the Save option does a saveRDS in "MRIO_Autarky.rds".
#'
#' @param dtdl datatable
#' @param Optdl binary
#' @param OptSaveRDS binary
#' @param OptBaseDT binary
#'
#' @return dl data long
#' @export
Autarky <- function(dtdl, Optdl = FALSE, OptSaveRDS = FALSE, OptBaseDT = FALSE) {
  if (Optdl == TRUE) {
    RecupAnnee <- as.numeric(dtdl[1, "year"]$year)
    MRIO <- CompoMRIO(dtdl, typeCompo = "OptFullOptions", date = RecupAnnee)
  } else {
    RecupAnnee <- as.numeric(dtdl[["DF"]][1, "year"])
    MRIO <- data.table::copy(dtdl)
  }
  
  ######################## Build Autarky
  # Keep CI and FD
  CIFD <- rbind(MRIO[["CI"]], MRIO[["DF"]])
  
  # Split domestic/imported
  CIFD_dom <- CIFD[Lig_Country == Col_Country, ]
  CIFD_imp <- CIFD[Lig_Country != Col_Country, sum(value, na.rm = T), by = c("Lig_Indus", "Col_Indus", "year", "Col_Country")]
  setnames(CIFD_imp, "V1", "value")
  
  # Allocate all imports to the domestic component --> autarky
  BindCIFD <- rbind(CIFD_dom[, Lig_Country := NULL], CIFD_imp)
  Autarky <- BindCIFD[, sum(value, na.rm = T), by = c("Lig_Indus", "Col_Indus", "year", "Col_Country")]
  setnames(Autarky, "V1", "value")
  
  # Addition of off-diagonal null elements to return to classic MRIO format.
  NulHorsDiag <- CIFD[Lig_Country != Col_Country, ] # init
  NulHorsDiag <- NulHorsDiag[, value := 0]
  Autarky <- Autarky[, Lig_Country := Col_Country]
  Autarky <- rbind(Autarky, NulHorsDiag)
  
  if (OptSaveRDS == TRUE) {
    saveRDS(Autarky, paste0("MRIO_Autarky.rds"))
  }
  
  ### Rebuild of the MRIO Autarky components
  MRIOAutarky <- CompoMRIO(Autarky, typeCompo = "OptFullOptionsBonus", date = RecupAnnee)
  
  if (OptBaseDT == TRUE) {
    MRIOAutarky <- list(MRIO = MRIOAutarky, DT = Autarky)
  }
  
  return(MRIOAutarky)
}

#' av_HRM
#' Master function in the ecosystem : Function HRM (hypothetical repatriation method)
#' repat_pct can come from an other function to calibrate
#' VARIANTs options :
#' OptVarianteDemande : ALL=Normal ; CIdom=CI domestics ; CIall=all CI (dom+exp) ; DFdom=DF domestics ; DFall=all final demand (dom+exp) ;
#' OptVariantePaysImp : ALL=Normal ; <country>= normal but 1 country only ; horsUE=substitution outside EU only
#' We implement a repat_pct % shock on the repat_indus product for the repat_country :
#' repatriation of repat_pct % of this country's imports of this product.
#' to repatriate 1 Md??? of production, we set the previous repatriation % on 1/prod
#' The calculations are transversal to TEI and FD: the entire product is uniformly concerned.
#'
#' @param dl datatable
#' @param repat_country text country
#' @param repat_indus text industry
#' @param repat_pct value percentage
#' @param verboseCheck binary 
#' @param OptSommeDFenP3_S14 binary
#' @param OptVarianteDemande binary
#' @param OptVariantePaysImp binary
#' @param OptBaseIntermAvantRecalcProd binary
#'
#' @return list of dl and dw
#' @export
av_HRM <- function(dl, repat_country, repat_indus, repat_pct, verboseCheck = FALSE, OptSommeDFenP3_S14 = TRUE, OptVarianteDemande = "ALL", OptVariantePaysImp = "ALL", OptBaseIntermAvantRecalcProd = TRUE) {
  if (OptSommeDFenP3_S14 == TRUE) {
    Base_init <- SommeDFenP3_S14(data.table::copy(dl))
  } else {
    Base_init <- data.table::copy(dl)
  }
  
  ### Works simultaneously on A matrix and final demand (FD)
  MRIOdep <- CompoMRIO(Base_init, typeCompo = "OptFullOptions", date = as.numeric(Base_init[1, "year"]))
  MRIOhyb <- rbind(MRIOdep[["A"]], MRIOdep[["DF"]], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
  Base_init <- MRIOhyb
  
  # 	Calculation steps :
  
  # 1/ Extract the Lig_Indus from the relevant Lig_Country and calculate repat_pct % for this line. We
  # obtain an amount for each MRIO column.
  Delta <- Base_init[Lig_Indus == repat_indus & Lig_Country == repat_country, ]
  Delta[, "value"] <- repat_pct * Delta[, "value"]
  
  
  ### VARIANTS (options CI or DF / dom or all)
  if (OptVarianteDemande == "CIall") {
    MRIOhyb <- rbind(MRIOdep[["A"]], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
    Delta <- Base_init[Lig_Indus == repat_indus & Lig_Country == repat_country, ]
    Delta[, "value"] <- repat_pct * Delta[, "value"]
    Delta <- rbind(Delta, MRIOdep[["DF"]][Lig_Indus == repat_indus & Lig_Country == repat_country, ][, value := 0])
    MRIOhyb <- rbind(MRIOdep[["A"]], MRIOdep[["DF"]], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
  }
  if (OptVarianteDemande == "CIdom") {
    MRIOhyb <- rbind(MRIOdep[["A"]][Col_Country == repat_country, ], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
    Delta <- Base_init[Lig_Indus == repat_indus & Lig_Country == repat_country, ]
    Delta[, "value"] <- repat_pct * Delta[, "value"]
    Delta <- rbind(Delta, MRIOdep[["A"]][Col_Country != repat_country, ][Lig_Indus == repat_indus & Lig_Country == repat_country, ][, value := 0], MRIOdep[["DF"]][Lig_Indus == repat_indus & Lig_Country == repat_country, ][, value := 0])
    MRIOhyb <- rbind(MRIOdep[["A"]], MRIOdep[["DF"]], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
  }
  if (OptVarianteDemande == "DFall") {
    MRIOhyb <- rbind(MRIOdep[["DF"]], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
    Delta <- Base_init[Lig_Indus == repat_indus & Lig_Country == repat_country, ]
    Delta[, "value"] <- repat_pct * Delta[, "value"]
    Delta <- rbind(Delta, MRIOdep[["A"]][Lig_Indus == repat_indus & Lig_Country == repat_country, ][, value := 0])
    MRIOhyb <- rbind(MRIOdep[["A"]], MRIOdep[["DF"]], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
  }
  if (OptVarianteDemande == "DFdom") {
    MRIOhyb <- rbind(MRIOdep[["DF"]][Col_Country == repat_country, ], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
    Delta <- Base_init[Lig_Indus == repat_indus & Lig_Country == repat_country, ]
    Delta[, "value"] <- repat_pct * Delta[, "value"]
    Delta <- rbind(Delta, MRIOdep[["DF"]][Col_Country != repat_country, ][Lig_Indus == repat_indus & Lig_Country == repat_country, ][, value := 0], MRIOdep[["A"]][Lig_Indus == repat_indus & Lig_Country == repat_country, ][, value := 0])
    MRIOhyb <- rbind(MRIOdep[["A"]], MRIOdep[["DF"]], MRIOdep[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"])
    Base_init <- MRIOhyb
  }
  
  
  # 2/ Extract Lig_Indus for all countries outside the shocked country, both on line and not on
  #     domestic parts (Lig_country==Col_Country)
  Lig_Ventil <- Base_init[Lig_Indus == repat_indus & Lig_Country != repat_country & Lig_Country != Col_Country, ]
  
  ### VARIANTs (options 1 country affected or outside the EU)
  if (OptVariantePaysImp == "horsUE") {
    ListUE <- list("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
    Lig_Ventil <- Base_init[Lig_Indus == repat_indus & Lig_Country != repat_country & Lig_Country != Col_Country & !(Lig_Country %in% ListUE), ]
  }
  if (OptVariantePaysImp != "ALL" & OptVariantePaysImp != "horsUE") {
    Lig_Ventil <- Base_init[Lig_Indus == repat_indus & Lig_Country != repat_country & Lig_Country == OptVariantePaysImp, ] # on selectionne le pays impose
  }
  
  
  # 3/ Total and structure are calculated directly in queries
  #     to avoid any adherence to formats).
  Lig_Ventil_totaux <- Lig_Ventil[, sum(value), by = .(year, Lig_Indus, Col_Indus, Col_Country)]
  Lig_Ventil_totaux <- Lig_Ventil_totaux[Lig_Ventil, on = .(year, Lig_Indus, Col_Indus, Col_Country)]
  Lig_Ventil_totaux[, "value"] <- Lig_Ventil_totaux[, "value"] / Lig_Ventil_totaux[, "V1"]
  
  Ordre_Col <- c("year", "Lig_Country", "Col_Country", "Lig_Indus", "Col_Indus", "value")
  Lig_Ventil_totaux <- Lig_Ventil_totaux[, ..Ordre_Col]
  Lig_Ventil_totaux <- GereInfNA(Lig_Ventil_totaux)
  
  if (verboseCheck == TRUE) { # check : Check that the structure sums to 1
    print("Check that the structure sums to 1 :")
    print(Lig_Ventil_totaux[, sum(value), by = .(year, Lig_Indus, Col_Indus, Col_Country)])
  }
  
  # 4/ Next, we distribute the amount from point 1 over the structure of point 3. We convert them to negative
  # as they will have to be subtracted. We obtain the amounts to be subtracted from the MRIO data.
  Lig_Ventil_delta <- Delta[Lig_Ventil_totaux, on = .(Lig_Indus, Col_Country, Col_Indus)]
  Lig_Ventil_delta <- Lig_Ventil_delta[, value_delta := -value * i.value]
  Ordre_Col <- c("year", "i.Lig_Country", "Col_Country", "Lig_Indus", "Col_Indus", "value_delta")
  Lig_Ventil_delta <- Lig_Ventil_delta[, ..Ordre_Col]
  setnames(Lig_Ventil_delta, "value_delta", "value")
  setnames(Lig_Ventil_delta, "i.Lig_Country", "Lig_Country")
  
  # 5/ We rbind the initial base with the base of point 1 (which is to be added) and the base of
  # point 4 (what is to be subtracted)
  # First, we create the delta base and check that the additions are properly
  # offset by withdrawals
  Delta <- Delta[, type_correc := "Ajout"]
  Lig_Ventil_delta <- Lig_Ventil_delta[, type_correc := "Retranche"]
  base_correc_HRM <- rbind(Delta, Lig_Ventil_delta)
  
  if (verboseCheck == TRUE) { # Check : Check that the structure sums to 0
    print("Check that the structure sums to 0 :")
    print(base_correc_HRM[, sum(value), by = .(Col_Country, Col_Indus)])
  }
  
  #     We add the initial base and its status
  Base_init <- Base_init[, type_correc := "Initial"]
  Base_HRM_detail <- rbind(Base_init, base_correc_HRM)
  
  
  # 6/ We sum according to all the MRIO dimensions except the status variable.
  # automatically sums the amounts in point 1 and subtracts the breakdown in point 4.
  Base_HRM <- Base_HRM_detail[, sum(value), by = .(year, Lig_Country, Col_Country, Lig_Indus, Col_Indus)]
  setnames(Base_HRM, "V1", "value")
  
  if (OptBaseIntermAvantRecalcProd == TRUE) {
    SaveIntermMRIO <- Base_HRM
  }
  
  ### Recalculation of production using the Léontief equation, then recalculation
  #     of intermediate consumption levels based on technical coefficients and new production.
  yearRecup <- as.numeric(Base_HRM[1, "year"])
  MRIOsale <- CompoMRIO(Base_HRM, typeCompo = "OptFullOptions", date = yearRecup) # Just to retrieve the DF_TOT and formatting of A
  vecteurDF <- MRIOsale[["DF_TOT"]]
  MatrixImoinsA <- as.matrix(diag(nrow(MRIOsale[["CI_tab"]])) - MRIOsale[["CI_tab"]][, 4:ncol(MRIOsale[["CI_tab"]])]) # Be careful : here CI is A in fact
  M1 <- as.matrix(MatrixImoinsA)
  V2 <- as.matrix(vectDF(vecteurDF$value), drop = FALSE)
  Prod_HRM_tab <- Mult2_rcpp3(inversion_rcpp3(M1), V2)
  
  CI_HRM_tab <- Mult2_rcpp3(as.matrix(MRIOsale[["CI_tab"]][, 4:ncol(MRIOsale[["CI_tab"]])]), diag(c(Prod_HRM_tab)))
  DF_HRM_tab <- MRIOsale[["DF_tab"]]
  
  # Formatting, melt and database
  colnames(CI_HRM_tab) <- colnames(MRIOsale[["CI_tab"]][, 4:ncol(MRIOsale[["CI_tab"]])])
  CI_HRM_tab <- data.table(MRIOsale[["CI_tab"]][, 1:3], CI_HRM_tab)
  CI_HRM_tab <- setDT(CI_HRM_tab)
  DF_HRM_tab <- setDT(DF_HRM_tab)
  CI_HRM_melt <- melt(CI_HRM_tab, id.vars = c("year", "Lig_Country", "Lig_Indus"), measure.vars = c(4:ncol(CI_HRM_tab)))
  DF_HRM_melt <- melt(DF_HRM_tab, id.vars = c("year", "Lig_Country", "Lig_Indus"), measure.vars = c(4:ncol(DF_HRM_tab)))
  CI_HRM_melt$Col_Country <- str_extract(CI_HRM_melt$variable, "[^_]+")
  CI_HRM_melt$Col_Indus <- substring(str_extract(CI_HRM_melt$variable, "_.+$"), 2)
  DF_HRM_melt$Col_Country <- str_extract(DF_HRM_melt$variable, "[^_]+")
  DF_HRM_melt$Col_Indus <- substring(str_extract(DF_HRM_melt$variable, "_.+$"), 2)
  CI_HRM_melt <- CI_HRM_melt[, variable := NULL]
  DF_HRM_melt <- DF_HRM_melt[, variable := NULL]
  Prod_HRM <- MRIOsale[["PROD"]]
  Prod_HRM$value <- Prod_HRM_tab
  Prod_HRM <- Prod_HRM[, Col_Country := "PROD"][, Col_Indus := "TOTAL"]
  HRM_dt <- rbind(Prod_HRM, CI_HRM_melt, DF_HRM_melt)
  
  if (OptBaseIntermAvantRecalcProd == TRUE) {
    Return_List <- list(Prod_HRM_tab = Prod_HRM, CI_HRM_tab = CI_HRM_tab, DF_HRM_tab = DF_HRM_tab, HRM_Base = HRM_dt, SaveIntermMRIO = SaveIntermMRIO)
  } else {
    Return_List <- list(Prod_HRM_tab = Prod_HRM, CI_HRM_tab = CI_HRM_tab, DF_HRM_tab = DF_HRM_tab, HRM_Base = HRM_dt)
  }
  
  return(Return_List)
}


#' Attr_TxSimu_HRM_100MoE
#' Function to affect the right % corresponding to 100Mo of VA or PROD.
#' Why 100Mo ? / because as we exchange imports with production we need to have enough imports. --> Linear Model => results*10 with no problem.
#' TypeTx = "VA" or "PROD"
#'
#' @param Produit text industry
#' @param pays text country
#' @param annee year
#' @param TypeTx text
#' @param OptSourceRDS binary
#' @param OptMRIOlong binary
#'
#' @return numeric value
#' @export
Attr_TxSimu_HRM_100MoE <- function(Produit, pays, annee, TypeTx = "VA", OptSourceRDS = "XXXXX", OptMRIOlong = NULL) {
  if (TypeTx == "PROD") {
    if (OptSourceRDS != "XXXXX") {
      interm <- readRDS(OptSourceRDS)
      Tx <- interm[Lig_Indus == Produit & Lig_Country == pays & year == annee, "TxSimuHRM_100MoE"]
    } else {
      if (is.null(OptMRIOlong)) {
        stop("Avionic Error : you need to feed a MRIO in long format")
      }
      Prod_tx <- OptMRIOlong[Col_Country == "PROD", ]
      Prod_tx$TxSimuHRM_100MoE <- 1 / Prod_tx$value * 100 # for 100M€
      Prod_tx <- GereInfNA(Prod_tx)
      Tx <- Prod_tx[Lig_Indus == Produit & Lig_Country == pays & year == annee, "TxSimuHRM_100MoE"]
    }
  }

  if (TypeTx == "VA") {
    if (OptSourceRDS != "XXXXX") {
      interm <- readRDS(OptSourceRDS)
      Tx <- interm[Col_Indus == Produit & Col_Country == pays & year == annee, "TxSimuHRM_100MoE"]
    } else {
      if (is.null(OptMRIOlong)) {
        stop("Avionic Error : you need to feed a MRIO in long format")
      }
      VA_tx <- CompoMRIO(OptMRIOlong, typeCompo = "VA", date = OptMRIOlong[1, "year"]$year, OptTab = FALSE, OptUE27 = FALSE)
      VA_tx$TxSimuHRM_100MoE <- 1 / VA_tx$value * 100 # for 100M€
      VA_tx <- GereInfNA(VA_tx)
      Tx <- VA_tx[Col_Indus == Produit & Col_Country == pays & year == annee, "TxSimuHRM_100MoE"]
    }
  }
  ### Code to build a database that can be saved in .RDS and used again and again for massive use (better time)
  # Prod_FIG<-DT[Col_Country=="PROD",]
  # Prod_FIG$TxSimuHRM_100MoE<-1/Prod_FIG$value*100 # pour 100M€
  # Prod_FIG<-GereInfNA(Prod_FIG)
  # saveRDS(Prod_FIG,"Sorties/Base_REF_ProductionFIG.rds")
  return(as.numeric(Tx))
}


#' IndicVariant_IndusCountry
#' Function to calculate differences of indicators resulting from a HRM variant for instance
#'
#' @param IndusREF text industry
#' @param CountryREF text country
#' @param ListCountryREF list
#' @param MRIO MRIO object
#' @param MRIObis MRIO object
#' @param RatioEmploiVA text data link
#'
#' @return dl data long
#' @export
IndicVariant_IndusCountry <- function(IndusREF, CountryREF = "FRA", ListCountryREF = c("FRA", "DEU", "GBR", "ESP", "ITA"), MRIO, MRIObis, RatioEmploiVA) {
  ################################################################################
  ### Made-IN ####################################################################
  MadeIn_MRIO <- av_MadeIn(MRIO, Optdl = FALSE, 2019, OptDonneesBrutes = FALSE)
  MadeIn_MRIObis <- av_MadeIn(MRIObis, Optdl = FALSE, 2019, OptDonneesBrutes = FALSE)

  Diff_MadeIn_HRM <- MadeIn_MRIO[["ratio_Tot"]]
  Diff_MadeIn_HRM$value <- 100 * (MadeIn_MRIObis[["ratio_Tot"]]$value - MadeIn_MRIO[["ratio_Tot"]]$value)

  # --> Output Made-in :
  Val_diff_MadeIn <- Diff_MadeIn_HRM[Col_Country %in% ListCountryREF, c("Col_Country", "value")][, TypeRes := "2_DiffMadeInPct"]

  ## Level for made-in agreggation
  MadeIn_Niv_MRIO <- av_MadeIn(MRIO, Optdl = FALSE, 2019, OptDonneesBrutes = TRUE)[["baseNumdenom"]][Col_Country == "FRA" & Lig_Indus == IndusREF, ]
  MadeIn_Niv_MRIObis <- av_MadeIn(MRIObis, Optdl = FALSE, 2019, OptDonneesBrutes = TRUE)[["baseNumdenom"]][Col_Country == "FRA" & Lig_Indus == IndusREF, ]
  MadeIn_Niv_MRIO <- MadeIn_Niv_MRIO[, TypeRes := paste0("2bis_DiffMadeInNiv_", position, "_Avant")]
  MadeIn_Niv_MRIObis <- MadeIn_Niv_MRIObis[, TypeRes := paste0("2bis_DiffMadeInNiv_", position, "_Apres")]

  # --> Output Made-in :
  Val_diff_MadeIn_Niv <- rbind(MadeIn_Niv_MRIO, MadeIn_Niv_MRIObis)
  Val_diff_MadeIn_Niv <- Val_diff_MadeIn_Niv[, position := NULL][, Lig_Indus := NULL]

  ################################################################################
  ### VA ####################"V1","value")

  Compar_VA <- MRIObis[["VA"]] # init
  Compar_VA$value <- MRIObis[["VA"]]$value - MRIO[["VA"]]$value
  Compar_VA <- Compar_VA[, sum(value), by = c("Col_Country")]
  setnames(Compar_VA, "V1", "value")
  Compar_VA <- Compar_VA[order(-abs(value))]

  Compar_VA_HRM <- Compar_VA[Col_Country %in% ListCountryREF, c("Col_Country", "value")][, TypeRes := "1_DiffProdVANiv"]

  ################################################################################
  ### Agregated CO2 Footprint ####################################################

  EmpreinteCO2MRIOp <- Contenus(MRIO, typeContenu = "Emi", MethContenu = "MatDF", EmprPays = CountryREF)
  interm_MRIO <- EmpreinteCO2MRIOp[["MatEmpreinte_dt"]][, year := "2019"] # intermediary for Reqsum
  interm_MRIO <- ReqSum(interm_MRIO, c("Lig_Country", "Col_Indus", "Lig_Indus"))

  EmpreinteCO2MRIObisp <- Contenus(MRIObis, typeContenu = "Emi", MethContenu = "MatDF", EmprPays = CountryREF) # Be careful : we need to keep initial Emi / Ouptut : calculated in a previous chunk
  interm_MRIObis <- EmpreinteCO2MRIObisp[["MatEmpreinte_dt"]][, year := "2019"] # intermediary for Reqsum
  interm_MRIObis <- ReqSum(interm_MRIObis, c("Lig_Country", "Col_Indus", "Lig_Indus"))

  Res_empreinte <- interm_MRIO # init
  Res_empreinte$value <- interm_MRIObis$value - interm_MRIO$value
  Res_empreinte$valuePct <- (interm_MRIObis$value - interm_MRIO$value) / interm_MRIO$value * 100
  Res_empreinte <- Res_empreinte[order(-abs(value))]

  P1 <- Res_empreinte[Col_Country %in% ListCountryREF, c("Col_Country", "value")][, TypeRes := "4_DiffAggFootprintCO2Niv"]
  P2 <- Res_empreinte[Col_Country %in% ListCountryREF, c("Col_Country", "valuePct")][, TypeRes := "5_DiffAggFootprintCO2Pct"]
  setnames(P2, "valuePct", "value")
  Res_empreinte_select <- rbind(P1, P2)

  ################################################################################
  ### Employement ################################################################

  VA_deb <- MRIO[["VA"]][Col_Country == "FRA", ]
  VA_ajust <- MRIObis[["VA"]][Col_Country == "FRA", ]
  dvaFRA_tab <- VA_ajust # init
  dvaFRA_tab$value <- VA_ajust$value - VA_deb$value
  ratioEmploiVA <- RatioEmploiVA
  ratioEmploiVA$Lig_Indus <- dvaFRA_tab$Col_Indus

  Estim_emploi <- dvaFRA_tab
  Estim_emploi$EstimeEmploi <- Estim_emploi$value * ratioEmploiVA$vect
  Estim_emploiDirect <- Estim_emploi[Col_Indus == IndusREF, ][, TypeRes := "3bis_DiffEmploisDirectNiv"][, value := NULL]
  Estim_emploiTot <- Estim_emploi[, sum(EstimeEmploi, na.rm = TRUE), by = c("year", "Col_Country")][, TypeRes := "3_DiffEmploisNiv"]
  setnames(Estim_emploiDirect, c("Col_Country", "EstimeEmploi"), c("Lig_Country", "value"))
  Estim_emploiDirect <- Estim_emploiDirect[, c("Lig_Country", "TypeRes", "value")]
  setnames(Estim_emploiTot, c("Col_Country", "V1"), c("Lig_Country", "value"))
  Estim_emploiTot <- Estim_emploiTot[, c("Lig_Country", "TypeRes", "value")]

  ################################################################################
  ### Inventory emissions ########################################################

  EmiProdAvant <- EmissionsProd(MRIO)
  EmiProdHRM <- EmissionsProd(MRIObis)

  Compar_EmiProd <- EmiProdAvant[["by_Country"]] # init
  Compar_EmiProd$value <- EmiProdHRM[["by_Country"]]$value - EmiProdAvant[["by_Country"]]$value
  Compar_EmiProd <- Compar_EmiProd[, sum(value), by = c("Lig_Country")]
  setnames(Compar_EmiProd, "V1", "value")
  Compar_EmiProd <- Compar_EmiProd[order(-abs(value))]

  Compar_EmiProdDirect <- EmiProdHRM[["EmiProd"]][Lig_Country == "FRA" & Lig_Indus == IndusREF, "value"] # init
  Compar_EmiProdDirect$value <- EmiProdHRM[["EmiProd"]][Lig_Country == "FRA" & Lig_Indus == IndusREF, "value"]$value - EmiProdAvant[["EmiProd"]][Lig_Country == "FRA" & Lig_Indus == IndusREF, "value"]$value
  Res_EmiProdDirect <- EmiProdHRM[["EmiProd"]][Lig_Country == "FRA" & Lig_Indus == IndusREF, "value"][, TypeRes := "6bis_DiffEmiProdDirect"]
  Res_EmiProdDirect[1, "value"] <- Compar_EmiProdDirect
  Res_EmiProdDirect <- Res_EmiProdDirect[, Lig_Country := "FRA"]

  Res_EmiProd <- Compar_EmiProd[, TypeRes := "6_DiffEmiProd"]
  Res_EmiProd <- Res_EmiProd[Lig_Country %in% ListCountryREF, ]

  Res_EmiWorld <- data.table(value = sum(Compar_EmiProd$value), Lig_Country = CountryREF, TypeRes = "7_DiffEmiWorld")

  ################################################################################
  ### External Balance ###########################################################

  SoldeExt <- SoldExtPays(MRIO)
  setnames(SoldeExt, "value", "SoldExt_MRIO")
  SoldeExtBis <- SoldExtPays(MRIObis)
  setnames(SoldeExtBis, "value", "SoldExt_MRIObis")
  SoldeDiff <- SoldeExtBis[SoldeExt, on = .(Lig_Country)]
  SoldeDiff$EffetHRM <- SoldeDiff$SoldExt_MRIObis - SoldeDiff$SoldExt_MRIO
  SoldeDiff$EffetHRMPct <- 100 * (SoldeDiff$SoldExt_MRIObis - SoldeDiff$SoldExt_MRIO) / (abs(SoldeDiff$SoldExt_MRIO))
  SoldeDiff <- GereInfNA(SoldeDiff)
  SoldeDiffP1 <- SoldeDiff[, c("Lig_Country", "EffetHRM")][, TypeRes := "8_DiffSoldExtNiv"]
  SoldeDiffP2 <- SoldeDiff[, c("Lig_Country", "EffetHRMPct")][, TypeRes := "9_DiffSoldExtPct"]
  setnames(SoldeDiffP1, "EffetHRM", "value")
  setnames(SoldeDiffP2, "EffetHRMPct", "value")
  Res_SoldExt <- rbind(SoldeDiffP1, SoldeDiffP2)
  Res_SoldExt <- Res_SoldExt[Lig_Country %in% ListCountryREF, ]

  ################################################################################
  ### formatting results #########################################################

  setnames(Res_empreinte_select, "Col_Country", "Lig_Country")
  setnames(Val_diff_MadeIn, "Col_Country", "Lig_Country")
  setnames(Val_diff_MadeIn_Niv, "Col_Country", "Lig_Country")
  setnames(Compar_VA_HRM, "Col_Country", "Lig_Country")
  BaseOut <- rbind(Val_diff_MadeIn, Val_diff_MadeIn_Niv, Compar_VA_HRM, Res_empreinte_select, Estim_emploiDirect, Estim_emploiTot, Res_EmiProd, Res_EmiProdDirect, Res_EmiWorld, Res_SoldExt)

  return(BaseOut)
}


##' av_SPA
##' Master function in the environment : Structural Path Analysis (SPA)
##'  Function for calculating the Stuctural Path Analysis (SPA) of a MRIO
##'  The MRIO can be in long format or it has already undergone a CompoMRIO (dt)
##'  TypeSPA="VA" or "Emi"
##'  ListThres = {GenThres=,Thres_L2_1=,Thres_L3_1=,Thres_L3_2=,Thres_L3_3,Thres_L3_4=,Thres_L4_1=,Thres_L4_2=,Thres_L4_3,Thres_L4_4=,Thres_L5_1=,Thres_L5_2=,Thres_L5_3,Thres_L5_4=,Thres_L6_1=,Thres_L6_2=,Thres_L6_3,Thres_L6_4=,Thres_L7_1=,Thres_L7_2=,Thres_L7_3,Thres_L7_4=}
##'  Fitted FIGARO VA : ListThres = {GenThres=0.001,Thres_L2_1=0.001,Thres_L3_1=0.001,Thres_L3_2=0.001,Thres_L3_3=0.00001,Thres_L3_4=0.001,Thres_L4_1=0.001,Thres_L4_2=0.001,Thres_L4_3=0.000005,Thres_L4_4=0.001,Thres_L5_1=0.001,Thres_L5_2=0.001,Thres_L5_3=0.000001,Thres_L5_4=0.001,Thres_L6_1=0.001,Thres_L6_2=0.001,Thres_L6_3=0.0000005,Thres_L6_4=0.001,Thres_L7_1=0.001,Thres_L7_2=0.001,Thres_L7_3=0.0000001,Thres_L7_4=0.001}
##'  Fitted FIGARO Emi : ListThres = {GenThres=0.001,Thres_L2_1=0.0001,Thres_L3_1=0.0001,Thres_L3_2=0.00001,Thres_L3_3=0.000005,Thres_L3_4=0.0001,Thres_L4_1=0.0001,Thres_L4_2=0.00001,Thres_L4_3=0.000005,Thres_L4_4=0.00001,Thres_L5_1=0.001,Thres_L5_2=0.0001,Thres_L5_3=0.000004,Thres_L5_4=0.000001,Thres_L6_1=0.0001,Thres_L6_2=0.00001,Thres_L6_3=0.000004,Thres_L6_4=0.0000001,Thres_L7_1=0.0005,Thres_L7_2=0.00005,Thres_L7_3=0.0000005,Thres_L7_4=0.001}
#'
#' @param dtdl datatable
#' @param Optdl binary
#' @param TypeSPA text options
#' @param TypeMRIO text
#' @param ListThres list thresholds
#' @param TargetCountry text country
#' @param OptRDS binary
#' @param OptRDSDetail binary
#' @param OptUE27 binary
#' @param verbose binary
#' @param PathEmi text data link 
#'
#' @return dt data table with path analysis results
#' @export
#'
#' @examples
#' \dontrun{ ListThres = {GenThres=0.001,Thres_L2_1=0.001,Thres_L3_1=0.001,Thres_L3_2=0.001,Thres_L3_3=0.001,Thres_L3_4=0.001,Thres_L4_1=0.001,Thres_L4_2=0.001,Thres_L4_3=0.001,Thres_L4_4=0.001,Thres_L5_1=0.001,Thres_L5_2=0.001,Thres_L5_3=0.001,Thres_L5_4=0.001,Thres_L6_1=0.001,Thres_L6_2=0.001,Thres_L6_3=0.001,Thres_L6_4=0.001,Thres_L7_1=0.001,Thres_L7_2=0.001,Thres_L7_3=0.001,Thres_L7_4=0.001}}
av_SPA <- function(dtdl, Optdl = FALSE, TypeSPA = "VA", TypeMRIO = "FIGARO", ListThres, TargetCountry = "FRA", OptRDS = "", OptRDSDetail = "", OptUE27 = FALSE, verbose = FALSE, PathEmi = "") {
  tic()

  # Finding reference year according to initial format of the MRIO
  if (Optdl == TRUE) {
    yearRecup <- as.numeric(dtdl[1, "year"])
  } else {
    yearRecup <- as.numeric(dtdl[["CI"]][1, "year"])
  }

  # Check if input is dt or dl : if dl then calculate dt with optional stressor extension
  if (Optdl == TRUE) {
    MRIO <- CompoMRIO(dtdl, "OptFullOptionsBonus", date = dtdl[1, "year"]$year, OptUE27 = OptUE27)
    if (TypeSPA == "VA") {
      MRIO <- av_extend_MRIO_dw(MRIO, TypeMRIO, TypExtension = "StressVA")
    }
    if (TypeSPA == "Emi") {
      MRIO <- av_extend_MRIO_dw(MRIO, TypeMRIO, TypExtension = "StressEmi", Path1 = PathEmi)
    }
  } else {
    List_Interm <- dtdl
    if (TypeSPA == "VA") {
      MRIO <- av_extend_MRIO_dw(dtdl, TypeMRIO, TypExtension = "StressVA")
    }
    if (TypeSPA == "Emi") {
      MRIO <- av_extend_MRIO_dw(dtdl, TypeMRIO, TypExtension = "StressEmi", Path1 = PathEmi)
    }
  }
  MRIO2 <- data.table::copy(MRIO)

  # Recuperation of MRIO components for calculations
  A_dt <- MRIO[["A"]]
  A_dt <- AjoutPRBR(A_dt)

  if (TypeSPA == "VA") {
    f_dt <- vectDF(diag(MRIO[["VAOverOuput"]]))
    f_dt <- GereInfNA(f_dt)
  }
  if (TypeSPA == "Emi") {
    f_dt <- vectDF(diag(MRIO[["EmiOverOuput"]]))
    f_dt <- GereInfNA(f_dt)
  }

  interm1 <- MRIO[["DF"]][Col_Country == TargetCountry, ]
  interm2 <- interm1[, sum(value), by = c("year", "Lig_Country", "Lig_Indus")]
  setnames(interm2, "V1", "value")
  PourJointure <- MRIO[["DF_TOT"]][, c("Lig_Country", "Lig_Indus")]
  y_dt <- interm2[PourJointure, on = .(Lig_Country, Lig_Indus)]

  interm <- y_dt
  interm$value <- f_dt$vect
  f_dt <- AjoutPRBR(interm)
  y_dt <- AjoutPRBR(y_dt)
  A_dt <- A_dt[, c("PR", "BR", "value")]
  f_dt <- f_dt[, c("PR", "value")]
  y_dt <- y_dt[, c("PR", "value")]
  setnames(y_dt, "PR", "BR")


  ###################
  # Layers 0 and 1 : In the calculation of layers: for FIy and FAy there is no problem because the data.table joins do not exceed 10M rows.

  # Layer 0 : FIy
  Layer_0 <- y_dt[f_dt, on = .(BR = PR), nomatch = 0]
  setnames(Layer_0, c("value", "i.value", "BR"), c("y_i", "f_i", "name_i"))
  Layer_0 <- Layer_0[, c("name_i", "f_i", "y_i")]
  Layer_0$force_fy <- Layer_0$f_i * Layer_0$y_i
  if (verbose == TRUE) {
    print(paste0("Layer 0 : done  | force L0 = ", sum(Layer_0$force_fy)))
  }


  # Layer 1 : FAy
  Layer_1 <- y_dt[A_dt, on = .(BR = BR), nomatch = 0]
  Layer_1 <- Layer_1[f_dt, on = .(PR = PR), nomatch = 0]
  setnames(Layer_1, c("value", "i.value.1", "i.value", "BR", "PR"), c("y_j", "f_i", "a_ij", "name_j", "name_i"))
  Layer_1 <- Layer_1[, c("name_i", "name_j", "f_i", "a_ij", "y_j")]
  Layer_1_save <- data.table::copy(Layer_1)
  Layer_1$force_fay <- Layer_1$f_i * Layer_1$a_ij * Layer_1$y_j
  Layer_1 <- Layer_1[abs(force_fay) > 0, ] # no need to keep the 0
  gc(reset = TRUE)
  if (verbose == TRUE) {
    print(paste0("Layer 1 : done  | force L1 = ", sum(Layer_1$force_fay)))
  }


  ###################
  # Layer 2 : Pour FA^2y : see the method for understanding the optimizations made
  # Starting selection on the left; We search for x in FA such that x=min(FA) such that x * max(Ay) > threshold
  # Start selection on the right; We search for x in Ay such that x=min(Ay) such that max(FA) * x > threshold

  interm_fay <- Layer_1_save[, ay := a_ij * y_j][, fa := f_i * a_ij]

  interm_fa_select <- interm_fay[abs(fa) > ListThres[["GenThres"]] / max(ay), ]
  if (verbose == TRUE) {
    print(paste0("L2 : Number of lines of fa_select : ", nrow(interm_fa_select)))
  }
  interm_ay_select <- interm_fay[abs(ay) > ListThres[["GenThres"]] / max(fa), ]
  if (verbose == TRUE) {
    print(paste0("L2 : Number of lines of ay_select : ", nrow(interm_ay_select)))
  }

  # Calcul avec une fonction

  Joint_simple <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_j", allow.cartesian = T][abs(f_i_deb * a_ij_deb * a_jk_fin * y_k_fin) > ListThres[["Thres_L2_1"]], by = "name_j"]
    interm <- interm[, force_faay := f_i_deb * a_ij_deb * a_jk_fin * y_k_fin][abs(force_faay) > ListThres[["Thres_L2_1"]], ][, c("name_i", "name_j", "name_k", "f_i_deb", "a_ij_deb", "a_jk_fin", "y_k_fin", "force_faay")]
    interm <- interm[abs(force_faay) > ListThres[["Thres_L2_1"]]]
    setnames(interm, c("f_i_deb", "a_ij_deb", "a_jk_fin", "y_k_fin"), c("f_i", "a_ij", "a_jk", "y_k"))
    return(interm)
  }

  deb_alter <- data.table::copy(interm_fa_select)
  fin_alter <- data.table::copy(interm_ay_select)
  deb_alter <- deb_alter[, ay := NULL][, fa := NULL]
  setnames(deb_alter, c("f_i", "a_ij", "y_j"), c("f_i_deb", "a_ij_deb", "y_j_deb"))
  setnames(fin_alter, c("name_i", "name_j", "f_i", "a_ij", "y_j"), c("name_j", "name_k", "f_i_fin", "a_jk_fin", "y_k_fin"))

  Layer_2func <- fin_alter[, .(Joint_simple(deb_alter, fin_alter)), on = "name_j", allow.cartesian = TRUE]
  gc(reset = TRUE)

  Layer_2 <- Layer_2func
  if (verbose == TRUE) {
    print(paste0("Layer 2 : done  | force L2 = ", sum(Layer_2$force_faay)))
  }


  ###################
  # Layer 3 : For FA^3y : We want to build on the previous point by taking the product of FAxA and Ay for FAxA above threshold and Ay above threshold. For FAxA above threshold, the max of FA must verify maxFA * x > threshold where x>threshold/maxFA. We can therefore select the tech a_ij coefficients that are greater than threshold/maxFA. We then perform our matrix product as in the previous point, respecting the threshold overshoot for the 2 members.

  ### We begin with FAxA
  # Select the Ay branches with the strongest effect, to make sure you don't miss them and be less strict about the threshold for them. As these are strong Ay, even weak fAA can be enough to reach the threshold.
  select1 <- interm_ay_select[abs(ay) > mean(ay), ]
  select2 <- unlist(unique(select1[, "name_i"]))

  debAA <- data.table::copy(interm_fa_select) # We start from the beginning, already sorted to limit cross-breeding.
  finAA1 <- data.table::copy(A_dt)
  finAA2 <- data.table::copy(A_dt)
  finAA1 <- finAA1[abs(value) > ListThres[["Thres_L3_1"]], ] # You can play with the thresholds, but don't exceed 5M * 800k, otherwise you'll exceed the machine's capacity.
  finAA2 <- finAA2[BR %in% select2 & abs(value) > ListThres[["Thres_L3_2"]], ]
  finAA <- unique(rbind(finAA1, finAA2))
  if (verbose == TRUE) {
    print(paste0("L3 Number of lines of debAA : ", nrow(debAA)))
  }
  if (verbose == TRUE) {
    print(paste0("L3 Number of lines of finAA1 : ", nrow(finAA1)))
  }
  if (verbose == TRUE) {
    print(paste0("L3 Number of lines of finAA2 : ", nrow(finAA2)))
  }
  if (verbose == TRUE) {
    print(paste0("L3 Number of lines of finAA : ", nrow(finAA)))
  }
  setnames(finAA, "PR", "name_j")
  debAA[, y_j := NULL][, fa := NULL][, ay := NULL] # not useful

  Joint_simpleAA <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_j", allow.cartesian = T][abs(f_i * a_ij * value) > ListThres[["Thres_L3_3"]], by = "name_j"]
    interm <- interm[, force_faa := f_i * a_ij * value][abs(force_faa) > ListThres[["Thres_L3_3"]], ] # Target : not more than 5 million lines
    setnames(interm, c("f_i", "a_ij", "BR", "value"), c("f_i", "a_ij", "name_k", "a_jk"))
    return(interm)
  }

  Layer_31func <- finAA[, .(Joint_simpleAA(debAA, finAA)), on = "name_j", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_31func <- Layer_31func[, c("name_i", "name_j", "name_k", "f_i", "a_ij", "a_jk", "force_faa")]

  ### Next step to calculate FAxA xAy
  Joint_simpleAAy <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_k", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * y_l) > ListThres[["Thres_L3_4"]], by = "name_k"]
    interm <- interm[, force_faaay := f_i * a_ij * a_jk * a_kl * y_l][abs(force_faaay) > ListThres[["Thres_L3_4"]], ][, c("name_i", "name_j", "name_k", "name_l", "f_i", "a_ij", "a_jk", "a_kl", "y_l", "force_faaay")]
    interm <- interm[abs(force_faaay) > ListThres[["Thres_L3_4"]]]
    return(interm)
  }

  deb_alter <- data.table::copy(Layer_31func)
  fin_alter <- data.table::copy(interm_ay_select)
  setnames(fin_alter, c("name_i", "name_j", "a_ij", "y_j"), c("name_k", "name_l", "a_kl", "y_l"))
  fin_alter[, f_i := NULL][, ay := NULL][, fa := NULL] # Not useful

  Layer_32func <- fin_alter[, .(Joint_simpleAAy(deb_alter, fin_alter)), on = "name_k", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_3 <- Layer_32func
  if (verbose == TRUE) {
    print(paste0("Layer 3 : done  | force L3 = ", sum(Layer_3$force_faaay)))
  }


  ###################
  # Layer 4 : For FA^4y: same: product of FAxAxA and Ay. For FAxAxA above the threshold

  ### We begin with FAxAxA
  # Select the Ay branches with the strongest effect, to make sure you don't miss them and be less strict about the threshold for them. As these are strong Ay, even weak fAA can be enough to reach the threshold.
  select1 <- interm_ay_select[abs(ay) > mean(ay), ]
  select2 <- unlist(unique(select1[, "name_i"]))

  debAAA <- data.table::copy(Layer_31func) # We start from the already sorted beginning of the previous Layer
  finAAA1 <- data.table::copy(A_dt)
  finAAA2 <- data.table::copy(A_dt)
  finAAA1 <- finAAA1[abs(value) > ListThres[["Thres_L4_1"]], ] # You can play with the thresholds, but don't exceed 5M * 800k, otherwise you'll exceed the machine's capacity.
  finAAA2 <- finAAA2[BR %in% select2 & abs(value) > ListThres[["Thres_L4_2"]], ]
  finAAA <- unique(rbind(finAAA1, finAAA2))
  if (verbose == TRUE) {
    print(paste0("L4 Number of lines of debAAA : ", nrow(debAAA)))
  }
  if (verbose == TRUE) {
    print(paste0("L4 Number of lines of finAAA1 : ", nrow(finAAA1)))
  }
  if (verbose == TRUE) {
    print(paste0("L4 Number of lines of finAAA2 : ", nrow(finAAA2)))
  }
  if (verbose == TRUE) {
    print(paste0("L4 Number of lines of finAAA : ", nrow(finAAA)))
  }
  setnames(finAAA, "PR", "name_k")
  debAAA[, force_faa := NULL] # Not useful

  Joint_simpleAAA <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_k", allow.cartesian = T][abs(f_i * a_ij * a_jk * value) > ListThres[["Thres_L4_3"]], by = "name_k"]
    interm <- interm[, force_faaa := f_i * a_ij * a_jk * value][abs(force_faaa) > ListThres[["Thres_L4_3"]], ] # The aim with this threshold is to have 5M lines at most.
    setnames(interm, c("BR", "value"), c("name_l", "a_kl"))
    return(interm)
  }

  Layer_41func <- finAAA[, .(Joint_simpleAAA(debAAA, finAAA)), on = "name_k", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_41func <- Layer_41func[, c("name_i", "name_j", "name_k", "name_l", "f_i", "a_ij", "a_jk", "a_kl", "force_faaa")]

  ### Next step to calculate FAxAxA xAy
  Joint_simpleAAAy <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_l", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * a_lm * y_m) > ListThres[["Thres_L4_4"]], by = "name_l"]
    interm <- interm[, force_faaaay := f_i * a_ij * a_jk * a_kl * a_lm * y_m][abs(force_faaaay) > ListThres[["Thres_L4_4"]], ][, c("name_i", "name_j", "name_k", "name_l", "name_m", "f_i", "a_ij", "a_jk", "a_kl", "a_lm", "y_m", "force_faaaay")]
    interm <- interm[abs(force_faaaay) > ListThres[["Thres_L4_4"]]]
    return(interm)
  }

  deb_alter <- data.table::copy(Layer_41func)
  fin_alter <- data.table::copy(interm_ay_select)
  setnames(fin_alter, c("name_i", "name_j", "a_ij", "y_j"), c("name_l", "name_m", "a_lm", "y_m"))
  fin_alter[, f_i := NULL][, ay := NULL][, fa := NULL]

  Layer_42func <- fin_alter[, .(Joint_simpleAAAy(deb_alter, fin_alter)), on = "name_l", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_4 <- Layer_42func
  if (verbose == TRUE) {
    print(paste0("Layer 4 : done  | force L4 = ", sum(Layer_4$force_faaaay)))
  }


  ###################
  # Layer 5 : For FA^5y: same: product of FAxAxAxA and Ay. For FAxAxAxA above threshold
  ### We begin with FAxAxAxA
  select1 <- interm_ay_select[abs(ay) > mean(ay), ]
  select2 <- unlist(unique(select1[, "name_i"]))

  debAAAA <- data.table::copy(Layer_41func) # We start from the already sorted beginning of the previous Layer
  finAAAA1 <- data.table::copy(A_dt)
  finAAAA2 <- data.table::copy(A_dt)
  finAAAA1 <- finAAA1[abs(value) > ListThres[["Thres_L5_1"]], ] # You can play with the thresholds, but don't exceed 5M * 800k, otherwise you'll exceed the machine's capacity.
  finAAAA2 <- finAAAA2[BR %in% select2 & abs(value) > ListThres[["Thres_L5_2"]], ]
  finAAAA <- unique(rbind(finAAAA1, finAAAA2))
  if (verbose == TRUE) {
    print(paste0("L5 Number of lines of debAAAA : ", nrow(debAAAA)))
  }
  if (verbose == TRUE) {
    print(paste0("L5 Number of lines of finAAAA1 : ", nrow(finAAAA1)))
  }
  if (verbose == TRUE) {
    print(paste0("L5 Number of lines of finAAAA2 : ", nrow(finAAAA2)))
  }
  if (verbose == TRUE) {
    print(paste0("L5 Number of lines of finAAAA : ", nrow(finAAAA)))
  }
  setnames(finAAAA, "PR", "name_l")
  debAAAA[, force_faa := NULL]

  Joint_simpleAAAA <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_l", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * value) > ListThres[["Thres_L5_3"]], by = "name_l"]
    interm <- interm[, force_faaaa := f_i * a_ij * a_jk * a_kl * value][abs(force_faaaa) > ListThres[["Thres_L5_3"]], ]
    setnames(interm, c("BR", "value"), c("name_m", "a_lm"))
    return(interm)
  }

  Layer_51func <- finAAAA[, .(Joint_simpleAAAA(debAAAA, finAAAA)), on = "name_l", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_51func <- Layer_51func[, c("name_i", "name_j", "name_k", "name_l", "name_m", "f_i", "a_ij", "a_jk", "a_kl", "a_lm", "force_faaaa")]

  ### Next step with FAxAxA xAy
  Joint_simpleAAAAy <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_m", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * a_lm * a_mn * y_n) > ListThres[["Thres_L5_4"]], by = "name_m"]
    interm <- interm[, force_faaaaay := f_i * a_ij * a_jk * a_kl * a_lm * a_mn * y_n][abs(force_faaaaay) > ListThres[["Thres_L5_4"]], ][, c("name_i", "name_j", "name_k", "name_l", "name_m", "name_n", "f_i", "a_ij", "a_jk", "a_kl", "a_lm", "a_mn", "y_n", "force_faaaaay")]
    interm <- interm[abs(force_faaaaay) > ListThres[["Thres_L5_4"]]]
    return(interm)
  }

  deb_alter <- data.table::copy(Layer_51func)
  fin_alter <- data.table::copy(interm_ay_select)
  setnames(fin_alter, c("name_i", "name_j", "a_ij", "y_j"), c("name_m", "name_n", "a_mn", "y_n"))
  fin_alter[, f_i := NULL][, ay := NULL][, fa := NULL]

  Layer_52func <- fin_alter[, .(Joint_simpleAAAAy(deb_alter, fin_alter)), on = "name_m", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_5 <- Layer_52func
  if (verbose == TRUE) {
    print(paste0("Layer 5 : done  | force L5 = ", sum(Layer_5$force_faaaaay)))
  }


  ###################
  # Layer 6 : For FA^6y: same: product of FAxAxAxAxA and Ay. For FAxAxAxAxA above the threshold
  ### We begin with FAxAxAxAxA
  select1 <- interm_ay_select[abs(ay) > mean(ay), ]
  select2 <- unlist(unique(select1[, "name_i"]))

  debAAAAA <- data.table::copy(Layer_51func) # We start from the already sorted beginning of the previous Layer
  finAAAAA1 <- data.table::copy(A_dt)
  finAAAAA2 <- data.table::copy(A_dt)
  finAAAAA1 <- finAAAAA1[abs(value) > ListThres[["Thres_L6_1"]], ]
  finAAAAA2 <- finAAAAA2[BR %in% select2 & abs(value) > ListThres[["Thres_L6_2"]], ]
  finAAAAA <- unique(rbind(finAAAAA1, finAAAAA2))
  if (verbose == TRUE) {
    print(paste0("L6 Number of lines of debAAAAA : ", nrow(debAAAAA)))
  }
  if (verbose == TRUE) {
    print(paste0("L6 Number of lines of finAAAAA1 : ", nrow(finAAAAA1)))
  }
  if (verbose == TRUE) {
    print(paste0("L6 Number of lines of finAAAAA2 : ", nrow(finAAAAA2)))
  }
  if (verbose == TRUE) {
    print(paste0("L6 Number of lines of finAAAAA : ", nrow(finAAAAA)))
  }
  setnames(finAAAAA, "PR", "name_m")
  debAAAAA[, force_faa := NULL]

  Joint_simpleAAAAA <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_m", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * a_lm * value) > ListThres[["Thres_L6_3"]], by = "name_m"]
    interm <- interm[, force_faaaaa := f_i * a_ij * a_jk * a_kl * a_lm * value][abs(force_faaaaa) > ListThres[["Thres_L6_3"]], ] # The aim with this threshold is to have 5M lines at most.
    setnames(interm, c("BR", "value"), c("name_n", "a_mn"))
    return(interm)
  }


  Layer_61func <- finAAAAA[, .(Joint_simpleAAAAA(debAAAAA, finAAAAA)), on = "name_m", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_61func <- Layer_61func[, c("name_i", "name_j", "name_k", "name_l", "name_m", "name_n", "f_i", "a_ij", "a_jk", "a_kl", "a_lm", "a_mn", "force_faaaaa")]

  ### Next step to calculate FAxAxA xAy
  Joint_simpleAAAAAy <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_n", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * a_lm * a_mn * a_no * y_o) > ListThres[["Thres_L6_4"]], by = "name_n"]
    interm <- interm[, force_faaaaaay := f_i * a_ij * a_jk * a_kl * a_lm * a_mn * a_no * y_o][abs(force_faaaaaay) > ListThres[["Thres_L6_4"]], ][, c("name_i", "name_j", "name_k", "name_l", "name_m", "name_n", "name_o", "f_i", "a_ij", "a_jk", "a_kl", "a_lm", "a_mn", "a_no", "y_o", "force_faaaaaay")]
    interm <- interm[abs(force_faaaaaay) > ListThres[["Thres_L6_4"]]]
    return(interm)
  }

  deb_alter <- data.table::copy(Layer_61func)
  fin_alter <- data.table::copy(interm_ay_select)
  setnames(fin_alter, c("name_i", "name_j", "a_ij", "y_j"), c("name_n", "name_o", "a_no", "y_o"))
  fin_alter[, f_i := NULL][, ay := NULL][, fa := NULL]

  Layer_62func <- fin_alter[, .(Joint_simpleAAAAAy(deb_alter, fin_alter)), on = "name_n", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_6 <- Layer_62func
  if (verbose == TRUE) {
    print(paste0("Layer 6 : done  | force L6 = ", sum(Layer_6$force_faaaaaay)))
  }


  ###################
  # Layer 7

  ### We begin with FAxAxAxAxAxA
  select1 <- interm_ay_select[abs(ay) > mean(ay), ]
  select2 <- unlist(unique(select1[, "name_i"]))

  debAAAAAA <- data.table::copy(Layer_61func) # We start from the already sorted beginning of the previous Layer
  finAAAAAA1 <- data.table::copy(A_dt)
  finAAAAAA2 <- data.table::copy(A_dt)
  finAAAAAA1 <- finAAAAAA1[abs(value) > ListThres[["Thres_L7_1"]], ] # You can play with the thresholds, but don't exceed 5M * 800k, otherwise you'll exceed the machine's capacity.
  finAAAAAA2 <- finAAAAAA2[BR %in% select2 & abs(value) > ListThres[["Thres_L7_2"]], ]
  finAAAAAA <- unique(rbind(finAAAAAA1, finAAAAAA2))
  if (verbose == TRUE) {
    print(paste0("L7 Number of lines of debAAAAAA : ", nrow(debAAAAAA)))
  }
  if (verbose == TRUE) {
    print(paste0("L7 Number of lines of finAAAAAA1 : ", nrow(finAAAAAA1)))
  }
  if (verbose == TRUE) {
    print(paste0("L7 Number of lines of finAAAAAA2 : ", nrow(finAAAAAA2)))
  }
  if (verbose == TRUE) {
    print(paste0("L7 Number of lines of finAAAAAA : ", nrow(finAAAAAA)))
  }
  setnames(finAAAAAA, "PR", "name_n")
  debAAAAAA[, force_faa := NULL]

  Joint_simpleAAAAAA <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_n", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * a_lm * a_mn * value) > ListThres[["Thres_L7_3"]], by = "name_n"]
    interm <- interm[, force_faaaaaa := f_i * a_ij * a_jk * a_kl * a_lm * a_mn * value][abs(force_faaaaaa) > ListThres[["Thres_L7_3"]], ] # The aim with this threshold is to have 5M lines at most.
    setnames(interm, c("BR", "value"), c("name_o", "a_no"))
    return(interm)
  }

  Layer_71func <- finAAAAAA[, .(Joint_simpleAAAAAA(debAAAAAA, finAAAAAA)), on = "name_n", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_71func <- Layer_71func[, c("name_i", "name_j", "name_k", "name_l", "name_m", "name_n", "name_o", "f_i", "a_ij", "a_jk", "a_kl", "a_lm", "a_mn", "a_no", "force_faaaaa")]

  ### Next step to calculate FAxAxAxAxA xAy
  Joint_simpleAAAAAAy <- function(deb, fin) {
    deb_w <- data.table::copy(deb)
    fin_w <- data.table::copy(fin)
    interm <- fin_w[deb_w, on = "name_o", allow.cartesian = T][abs(f_i * a_ij * a_jk * a_kl * a_lm * a_mn * a_no * a_op * y_p) > ListThres[["Thres_L7_4"]], by = "name_o"]
    interm <- interm[, force_faaaaaaay := f_i * a_ij * a_jk * a_kl * a_lm * a_mn * a_no * a_op * y_p][abs(force_faaaaaaay) > ListThres[["Thres_L7_4"]], ][, c("name_i", "name_j", "name_k", "name_l", "name_m", "name_n", "name_o", "name_p", "f_i", "a_ij", "a_jk", "a_kl", "a_lm", "a_mn", "a_no", "a_op", "y_p", "force_faaaaaaay")]
    interm <- interm[abs(force_faaaaaaay) > ListThres[["Thres_L7_4"]]]
    return(interm)
  }

  deb_alter <- data.table::copy(Layer_71func)
  fin_alter <- data.table::copy(interm_ay_select)
  setnames(fin_alter, c("name_i", "name_j", "a_ij", "y_j"), c("name_o", "name_p", "a_op", "y_p"))
  fin_alter[, f_i := NULL][, ay := NULL][, fa := NULL]

  Layer_72func <- fin_alter[, .(Joint_simpleAAAAAAy(deb_alter, fin_alter)), on = "name_o", allow.cartesian = TRUE]
  gc(reset = TRUE)
  Layer_7 <- Layer_72func
  if (verbose == TRUE) {
    print(paste0("Layer 7 : done  | force L7 = ", sum(Layer_7$force_faaaaaaay)))
  }


  ###################
  L0 <- Layer_0[, deb := name_i][, fin := name_i][, path := paste0(name_i)][, f_i := NULL][, y_i := NULL][, name_i := NULL]
  setnames(L0, "force_fy", "Pathvalue")
  L1 <- Layer_1[, deb := name_i][, fin := name_j][, path := paste0(name_i, "~", name_j)][, f_i := NULL][, y_j := NULL][, a_ij := NULL][, name_i := NULL][, name_j := NULL]
  setnames(L1, "force_fay", "Pathvalue")
  L2 <- Layer_2[, deb := name_i][, fin := name_k][, path := paste0(name_i, "~", name_j, "~", name_k)][, f_i := NULL][, y_k := NULL][, a_ij := NULL][, a_jk := NULL][, name_i := NULL][, name_j := NULL][, name_k := NULL]
  setnames(L2, "force_faay", "Pathvalue")
  L3 <- Layer_3[, deb := name_i][, fin := name_l][, path := paste0(name_i, "~", name_j, "~", name_k, "~", name_l)][, f_i := NULL][, y_l := NULL][, a_ij := NULL][, a_jk := NULL][, a_kl := NULL][, name_i := NULL][, name_j := NULL][, name_k := NULL][, name_l := NULL]
  setnames(L3, "force_faaay", "Pathvalue")
  L4 <- Layer_4[, deb := name_i][, fin := name_m][, path := paste0(name_i, "~", name_j, "~", name_k, "~", name_l, "~", name_m)][, f_i := NULL][, y_m := NULL][, a_ij := NULL][, a_jk := NULL][, a_kl := NULL][, a_lm := NULL][, name_i := NULL][, name_j := NULL][, name_k := NULL][, name_l := NULL][, name_m := NULL]
  setnames(L4, "force_faaaay", "Pathvalue")
  L5 <- Layer_5[, deb := name_i][, fin := name_n][, path := paste0(name_i, "~", name_j, "~", name_k, "~", name_l, "~", name_m, "~", name_n)][, f_i := NULL][, y_n := NULL][, a_ij := NULL][, a_jk := NULL][, a_kl := NULL][, a_lm := NULL][, a_mn := NULL][, name_i := NULL][, name_j := NULL][, name_k := NULL][, name_l := NULL][, name_m := NULL][, name_n := NULL]
  setnames(L5, "force_faaaaay", "Pathvalue")
  L6 <- Layer_6[, deb := name_i][, fin := name_o][, path := paste0(name_i, "~", name_j, "~", name_k, "~", name_l, "~", name_m, "~", name_n, "~", name_o)][, f_i := NULL][, y_o := NULL][, a_ij := NULL][, a_jk := NULL][, a_kl := NULL][, a_lm := NULL][, a_mn := NULL][, a_no := NULL][, name_i := NULL][, name_j := NULL][, name_k := NULL][, name_l := NULL][, name_m := NULL][, name_n := NULL][, name_o := NULL]
  setnames(L6, "force_faaaaaay", "Pathvalue")
  L7 <- Layer_7[, deb := name_i][, fin := name_o][, path := paste0(name_i, "~", name_j, "~", name_k, "~", name_l, "~", name_m, "~", name_n, "~", name_o, "~", name_p)][, f_i := NULL][, y_p := NULL][, a_ij := NULL][, a_jk := NULL][, a_kl := NULL][, a_lm := NULL][, a_mn := NULL][, a_no := NULL][, a_op := NULL][, name_i := NULL][, name_j := NULL][, name_k := NULL][, name_l := NULL][, name_m := NULL][, name_n := NULL][, name_o := NULL][, name_p := NULL]
  setnames(L7, "force_faaaaaaay", "Pathvalue")

  ###############
  Path_L0to7 <- rbind(L0, L1, L2, L3, L4, L5, L6, L7)

  ###############
  if (OptRDSDetail != "") {
    MesLayers <- list(L0 = Layer_0, L1 = Layer_1, L2 = Layer_2, L3 = Layer_3, L4 = Layer_4, L5 = Layer_5, L6 = Layer_6, L7 = Layer_7)
    saveRDS(MesLayers, OptRDSDetail)
  }

  if (OptRDS != "") {
    saveRDS(Path_L0to7, OptRDS)
  }

  toc()
  return(Path_L0to7)
}

##################################################################################
#' av_Diff_SPA
#' Function for calculating the difference of Stuctural Path Analysis (SPA) for variant analysis
#' The MRIO can only be in long format here
#' TypeSPA="VA" or "Emi"
#' ListThres = {GenThres=,Thres_L2_1=,Thres_L3_1=,Thres_L3_2=,Thres_L3_3,Thres_L3_4=,Thres_L4_1=,Thres_L4_2=,Thres_L4_3,Thres_L4_4=,Thres_L5_1=,Thres_L5_2=,Thres_L5_3,Thres_L5_4=,Thres_L6_1=,Thres_L6_2=,Thres_L6_3,Thres_L6_4=,Thres_L7_1=,Thres_L7_2=,Thres_L7_3,Thres_L7_4=}
#' Fitted FIGARO VA : ListThres = {GenThres=0.001,Thres_L2_1=0.001,Thres_L3_1=0.001,Thres_L3_2=0.001,Thres_L3_3=0.00001,Thres_L3_4=0.001,Thres_L4_1=0.001,Thres_L4_2=0.001,Thres_L4_3=0.000005,Thres_L4_4=0.001,Thres_L5_1=0.001,Thres_L5_2=0.001,Thres_L5_3=0.000001,Thres_L5_4=0.001,Thres_L6_1=0.001,Thres_L6_2=0.001,Thres_L6_3=0.0000005,Thres_L6_4=0.001,Thres_L7_1=0.001,Thres_L7_2=0.001,Thres_L7_3=0.0000001,Thres_L7_4=0.001}
#' Fitted FIGARO Emi : ListThres = {GenThres=0.001,Thres_L2_1=0.0001,Thres_L3_1=0.0001,Thres_L3_2=0.00001,Thres_L3_3=0.000005,Thres_L3_4=0.0001,Thres_L4_1=0.0001,Thres_L4_2=0.00001,Thres_L4_3=0.000005,Thres_L4_4=0.00001,Thres_L5_1=0.001,Thres_L5_2=0.0001,Thres_L5_3=0.000004,Thres_L5_4=0.000001,Thres_L6_1=0.0001,Thres_L6_2=0.00001,Thres_L6_3=0.000004,Thres_L6_4=0.0000001,Thres_L7_1=0.0005,Thres_L7_2=0.00005,Thres_L7_3=0.0000005,Thres_L7_4=0.001}
#'
#' @param MRIOdt1 datatable
#' @param MRIOdt2 datatable
#' @param ListThres list thresholds
#' @param TypContenu text options
#' @param TypeMRIO text
#' @param PathEmi text path
#' @param TargetCountry text country option
#'
#' @return dt data table same format as path analysis (SPA)
#' @export
#'
#' @examples
#' \dontrun{ ListThres = {GenThres=0.001,Thres_L2_1=0.001,Thres_L3_1=0.001,Thres_L3_2=0.001,Thres_L3_3=0.001,Thres_L3_4=0.001,Thres_L4_1=0.001,Thres_L4_2=0.001,Thres_L4_3=0.001,Thres_L4_4=0.001,Thres_L5_1=0.001,Thres_L5_2=0.001,Thres_L5_3=0.001,Thres_L5_4=0.001,Thres_L6_1=0.001,Thres_L6_2=0.001,Thres_L6_3=0.001,Thres_L6_4=0.001,Thres_L7_1=0.001,Thres_L7_2=0.001,Thres_L7_3=0.001,Thres_L7_4=0.001}
#' res<-av_Diff_SPA(MRIOdt_REF, MRIOdt_bis, ListThres, TypContenu = "Emi", PathEmi = PathTemp,)}
av_Diff_SPA <- function(MRIOdt1, MRIOdt2, ListThres, TypContenu = "VA", TypeMRIO = "FIGARO", PathEmi = "", TargetCountry = "FRA") {
  if (TypContenu == "VA") {
    Layers_1 <- av_SPA(MRIOdt1, Optdl = TRUE, TypeSPA = "VA", TypeMRIO = TypeMRIO, ListThres, TargetCountry = TargetCountry, OptRDS = "", OptRDSDetail = "", OptUE27 = FALSE, verbose = FALSE, PathEmi = "")
    Layers_2 <- av_SPA(MRIOdt2, Optdl = TRUE, TypeSPA = "VA", TypeMRIO = TypeMRIO, ListThres, TargetCountry = TargetCountry, OptRDS = "", OptRDSDetail = "", OptUE27 = FALSE, verbose = FALSE, PathEmi = "")
  } else {
    Layers_1 <- av_SPA(MRIOdt1, Optdl = TRUE, TypeSPA = "Emi", TypeMRIO = TypeMRIO, ListThres, TargetCountry = TargetCountry, OptRDS = "", OptRDSDetail = "", OptUE27 = FALSE, verbose = FALSE, PathEmi = PathEmi)
    Layers_2 <- av_SPA(MRIOdt2, Optdl = TRUE, TypeSPA = "Emi", TypeMRIO = TypeMRIO, ListThres, TargetCountry = TargetCountry, OptRDS = "", OptRDSDetail = "", OptUE27 = FALSE, verbose = FALSE, PathEmi = PathEmi)
  }

  # Change unities for consistency with KTCO2
  if (TypContenu == "Emi") {
    Layers_1$Pathvalue <- Layers_1$Pathvalue / 10
    Layers_2$Pathvalue <- Layers_2$Pathvalue / 10
  }

  # The aim is to compare the initial path with the newly varied path.
  Layers_1_rank <- Layers_1[, ranking := rank(-Pathvalue)] # var Ranking
  Layers_2_rank <- Layers_2[, ranking := rank(-Pathvalue)] # var Ranking

  Variante_Compar <- Layers_2_rank[Layers_1_rank, on = .(path = path), nomatch = 0]

  Variante_Compar <- Variante_Compar[, diffRanking := ranking - i.ranking][, diffValue := 10 * (Pathvalue - i.Pathvalue)][order(-abs(diffValue))]

  return(Variante_Compar)
}


##################################################################################
#' av_fun_VarPostTransition
#' Function for robustness checks by applying structural changes at the wolrd economy to suit climate change
#' Parameters description :
#' - paramEmploi : Employment intensity (value = 0.9 => 10% drop in the employment intensity of the automotive industry)
#' - paramA : Intermediate consumption substitution intensity: value = 0.9 => the technical coefficient of each country's automotive industry (C29) in products C29 and C28 (domestic and imported) is reduced by 10%, and the sum of these two amounts is added to intermediate consumption of product C29 in product C27, for each country.
#' - paramDF : The intensity of final consumption substitution: value = 0.9 => each country's final demand for oil products (C19) is reduced by 10%, and this amount is transferred to electricity products (D35) for each country.
#' The adjustments made are therefore zero-sum on intermediate consumption and final demand. The simulations are called by their X-Y-Z parameters: for example, 09-08-07 will correspond to a parameter of 0.9 for employment, 0.8 for intermediate consumption and 0.7 for final demand.
#'
#' @param paramEmploi value percentage
#' @param paramA value percentage
#' @param paramDF value percentage
#' @param OptRDS binary
#'
#' @return comparison table
#' @export
#'
#' @examples
#' \dontrun{ av_fun_VarPostTransition(paramEmploi = 1, paramA = 1, paramDF = 1)
#' av_fun_VarPostTransition(paramEmploi = 0.9, paramA = 0.8, paramDF = 0.7)}
av_fun_VarPostTransition <- function(paramEmploi, paramA, paramDF, OptRDS = FALSE) {
  tic()

  MRIO <- readRDS(paste0(PathTest, "HRMwide_REF_FIG2019_100MoE.rds"))
  DT <- readRDS(paste0(PathTest, "BDn_FIG_2019.rds"))
  RatioEmploiVA <- readRDS(paste0(PathTest, "ratioEmploiVA_2019.rds"))
  gc()

  # Calculating HRM for C29 based on this original MRIO
  IndusREF <- "C29"
  Pct_Attrib_IndusREF <- Attr_TxSimu_HRM_100MoE(IndusREF, "FRA", 2019, TypeTx = "VA", OptSourceRDS = "XXXXX", OptMRIOlong = BASE_FIG2019)
  interm <- av_HRM(DT, "FRA", IndusREF, Pct_Attrib_IndusREF, verboseCheck = F, OptSommeDFenP3_S14 = TRUE, OptVarianteDemande = "ALL", OptVariantePaysImp = "horsUE", OptBaseIntermAvantRecalcProd = FALSE)
  base_long_interm <- interm[["HRM_Base"]]
  base_wide_interm <- CompoMRIO(base_long_interm, typeCompo = "OptFullOptionsBonus", date = 2019) # SommeDFenP3_S14(
  MRIO2_interm <- av_extend_MRIO_dw(MRIO_dw = base_wide_interm, "FIGARO", TypExtension = "StressVA")
  MRIO3_interm <- av_extend_MRIO_dw(MRIO_dw = MRIO2_interm, "FIGARO", TypExtension = "StressEmi", Path1 = PathTemp)
  MRIO3_interm[["EmiOverOuput"]] <- MRIO[["EmiOverOuput"]] # Be careful : we need to keep initial Emi / Ouptut because Ouput is recalculated by HRM and emissions does not fit this new output but initial one.
  if (OptRDS == TRUE) {
    saveRDS(base_long_interm, paste0(PathTest, "HRMlong_", IndusREF, "_HorsUE_100MoE_VarPostTransition.rds"))
    saveRDS(MRIO3_interm, paste0(PathTest, "HRMwide_", IndusREF, "_HorsUE_100MoE_VarPostTransition.rds"))
  }
  gc()

  # Results of the complete variant
  if (OptRDS == TRUE) {
    MRIObis <- readRDS(paste0(PathTest, "HRMwide_", IndusREF, "_HorsUE_100MoE_VarPostTransition.rds"))
  } else {
    MRIObis <- MRIO3_interm
  }
  Indic_interm_HorsUE <- IndicVariant_IndusCountry(IndusREF, CountryREF = "FRA", ListCountryREF = c("FRA", "DEU", "GBR", "ESP", "ITA"), MRIO, MRIObis, RatioEmploiVA)
  gc()

  ##################################

  # Changes on Employment intensity (Emp/VA) for automobile commodities
  RatioEmploiVA <- readRDS(paste0(PathTest, "ratioEmploiVA_2019.rds"))
  coeffEmploiVA <- paramEmploi # 0.999
  RatioEmploiVA_ajust <- setDT(RatioEmploiVA)
  RatioEmploiVA_ajust <- RatioEmploiVA_ajust[Lig_Indus == "C29", vect := coeffEmploiVA * vect]

  # Changes on DF : we cut a share of the household consumption in C19, which is switched in D35 consumption to keep the same aggregated consumption (and VA).
  DF_Out_Of_seclect <- MRIO[["DF"]][!(Lig_Indus %in% c("C19", "D35")), ]
  DF_seclect_w_tab <- dcast(DF_seclect, Col_Country + Col_Indus + Lig_Country ~ Lig_Indus, value.var = "value")

  coeffDF <- paramDF # 0.999
  DF_seclect_w_tab$C19_new <- DF_seclect_w_tab$C19 * coeffDF
  DF_seclect_w_tab$D35_new <- DF_seclect_w_tab$D35 + DF_seclect_w_tab$C19 * (1 - coeffDF)
  DF_seclect_w_tab <- DF_seclect_w_tab[, C19 := NULL][, D35 := NULL]
  setnames(DF_seclect_w_tab, c("C19_new", "D35_new"), c("C19", "D35"))
  DF_new <- melt(DF_seclect_w_tab)
  setnames(DF_new, "variable", "Lig_Indus")
  DF_new <- DF_new[, year := "2019"]
  DF_new <- GereInfNA(DF_new)
  DF_new <- rbind(DF_Out_Of_seclect, DF_new)
  DF_new$Lig_Indus <- as.character(DF_new$Lig_Indus)
  DF_new_tab <- dcast(DF_new, Lig_Country + Lig_Indus ~ Col_Country + Col_Indus, value.var = "value")


  # Changes on A : We cut a share of IC of C29 industry into C29 and C28 commodities, which sum is switched to C27 to keep technicol coefficients sum unchanged.

  A_Out_Of_seclect_1 <- MRIO[["A"]][!(Col_Indus == "C29"), ]
  A_Out_Of_seclect_2 <- MRIO[["A"]][!(Lig_Indus %in% c("C27", "C28", "C29")), ]
  A_Out_Of_seclect_2bis <- A_Out_Of_seclect_2[Col_Indus == "C29", ]
  A_Out_Of_seclect <- rbind(A_Out_Of_seclect_1, A_Out_Of_seclect_2bis)
  A_seclect <- MRIO[["A"]][Lig_Indus %in% c("C27", "C28", "C29") & Col_Indus == "C29", ]
  A_seclect_tab <- dcast(A_seclect, Lig_Country + Col_Country + Col_Indus ~ Lig_Indus, value.var = "value")

  coeffA <- paramA # 0.999
  A_seclect_tab$C27_new <- A_seclect_tab$C27 + A_seclect_tab$C28 * (1 - coeffA) + A_seclect_tab$C29 * (1 - coeffA)
  A_seclect_tab$C28_new <- A_seclect_tab$C28 * coeffA
  A_seclect_tab$C29_new <- A_seclect_tab$C29 * coeffA

  A_seclect_tab <- A_seclect_tab[, C29 := NULL][, C28 := NULL][, C27 := NULL]
  setnames(A_seclect_tab, c("C29_new", "C28_new", "C27_new"), c("C29", "C28", "C27"))
  A_new <- melt(A_seclect_tab)
  setnames(A_new, "variable", "Lig_Indus")
  A_new <- A_new[, year := "2019"]
  A_new <- rbind(A_Out_Of_seclect, A_new)
  A_new$Lig_Indus <- as.character(A_new$Lig_Indus)
  A_new_tab <- dcast(A_new, Lig_Country + Lig_Indus ~ Col_Country + Col_Indus, value.var = "value")

  # Re-calculation of the new output with Leontieff equation
  vecteurDF <- vectDF(rowSums(DF_new_tab[, 3:ncol(DF_new_tab)]))
  MatrixImoinsA <- as.matrix(diag(nrow(MRIO[["CI_tab"]])) - A_new_tab[, 3:ncol(A_new_tab)])
  M1 <- as.matrix(MatrixImoinsA)
  V2 <- as.matrix(vecteurDF$vect, drop = FALSE)
  Prod_ajust <- Mult2_rcpp3(inversion_rcpp3(M1), V2)
  CI_ajust <- Mult2_rcpp3(as.matrix(A_new_tab[, 3:ncol(A_new_tab)]), diag(c(Prod_ajust)))
  DF_ajust <- DF_new_tab


  # Formatting of the new MRIO
  colnames(CI_ajust) <- colnames(MRIO[["CI_tab"]][, 4:ncol(MRIO[["CI_tab"]])])
  CI_ajust <- data.table(MRIO[["CI_tab"]][, 1:3], CI_ajust)
  CI_ajust <- setDT(CI_ajust)
  DF_ajust <- setDT(DF_ajust)
  CI_ajust_melt <- melt(CI_ajust, id.vars = c("year", "Lig_Country", "Lig_Indus"), measure.vars = c(4:ncol(CI_ajust)))
  DF_ajust_melt <- melt(DF_ajust, id.vars = c("Lig_Country", "Lig_Indus"), measure.vars = c(3:ncol(DF_ajust)))
  CI_ajust_melt$Col_Country <- str_extract(CI_ajust_melt$variable, "[^_]+")
  CI_ajust_melt$Col_Indus <- substring(str_extract(CI_ajust_melt$variable, "_.+$"), 2)
  DF_ajust_melt$Col_Country <- str_extract(DF_ajust_melt$variable, "[^_]+")
  DF_ajust_melt$Col_Indus <- substring(str_extract(DF_ajust_melt$variable, "_.+$"), 2)
  CI_ajust_melt <- CI_ajust_melt[, variable := NULL]
  DF_ajust_melt <- DF_ajust_melt[, variable := NULL]
  Prod_ajust2 <- MRIO[["PROD"]]
  Prod_ajust2$value <- Prod_ajust
  Prod_ajust2 <- Prod_ajust2[, Col_Country := "PROD"][, Col_Indus := "TOTAL"]
  Prod_ajust <- Prod_ajust2[, TypMRIO := NULL]
  BASE_FIG_ajust <- rbind(Prod_ajust, CI_ajust_melt, DF_ajust_melt[, year := "2019"])

  base_long_interm <- BASE_FIG_ajust
  base_wide_interm <- CompoMRIO(base_long_interm, typeCompo = "OptFullOptionsBonus", date = 2019) # SommeDFenP3_S14(
  MRIO2_interm <- av_extend_MRIO_dw(MRIO_dw = base_wide_interm, "FIGARO", TypExtension = "StressVA")
  MRIO3_interm_P1 <- av_extend_MRIO_dw(MRIO_dw = MRIO2_interm, "FIGARO", TypExtension = "StressEmi", Path1 = PathTemp)
  MRIO3_interm_P1[["EmiOverOuput"]] <- MRIO[["EmiOverOuput"]] # Be careful : we need to keep initial Emi / Ouptut because Ouput is recalculated by HRM and emissions does not fit this new output but initial one.
  if (OptRDS == TRUE) {
    saveRDS(base_long_interm, paste0(PathTest, "Struct_Long_VarPostTransition_P1.rds"))
    saveRDS(MRIO3_interm_P1, paste0(PathTest, "Struct_Wide_VarPostTransition_P1.rds"))
  }
  gc()

  # Calculating HRM for C29 based on this new MRIO
  IndusREF <- "C29"
  Pct_Attrib_IndusREF <- Attr_TxSimu_HRM_100MoE(IndusREF, "FRA", 2019, TypeTx = "VA", OptSourceRDS = "XXXXX", OptMRIOlong = BASE_FIG2019)
  interm <- av_HRM(BASE_FIG_ajust[, TypMRIO := NULL], "FRA", IndusREF, Pct_Attrib_IndusREF, verboseCheck = F, OptSommeDFenP3_S14 = TRUE, OptVarianteDemande = "ALL", OptVariantePaysImp = "horsUE", OptBaseIntermAvantRecalcProd = FALSE)
  base_long_interm <- interm[["HRM_Base"]]
  base_wide_interm <- CompoMRIO(base_long_interm, typeCompo = "OptFullOptionsBonus", date = 2019) # SommeDFenP3_S14(
  MRIO2_interm <- av_extend_MRIO_dw(MRIO_dw = base_wide_interm, "FIGARO", TypExtension = "StressVA")
  MRIO3_interm_TOT <- av_extend_MRIO_dw(MRIO_dw = MRIO2_interm, "FIGARO", TypExtension = "StressEmi", Path1 = PathTemp)
  MRIO3_interm_TOT[["EmiOverOuput"]] <- MRIO[["EmiOverOuput"]] # Be careful : we need to keep initial Emi / Ouptut because Ouput is recalculated by HRM and emissions does not fit this new output but initial one.
  if (OptRDS == TRUE) {
    saveRDS(base_long_interm, paste0(PathTest, "HRMlong_", IndusREF, "_100MoE_VarPostTransition.rds"))
    saveRDS(MRIO3_interm_TOT, paste0(PathTest, "HRMwide_", IndusREF, "_100MoE_VarPostTransition.rds"))
  }
  gc()

  # Results of the complete variant
  if (OptRDS == TRUE) {
    MRIObis <- readRDS(paste0(PathTest, "HRMwide_", IndusREF, "_100MoE_VarPostTransition.rds"))
  } else {
    MRIObis <- MRIO3_interm_TOT
  }
  Indic_interm <- IndicVariant_IndusCountry(IndusREF, CountryREF = "FRA", ListCountryREF = c("FRA", "DEU", "GBR", "ESP", "ITA"), data.table::copy(MRIO), data.table::copy(MRIObis), RatioEmploiVA_ajust)

  # Results of the first part of variant
  if (OptRDS == TRUE) {
    MRIOinterbis <- readRDS(paste0(PathTest, "Struct_Wide_VarPostTransition_P1.rds"))
  } else {
    MRIOinterbis <- MRIO3_interm_P1
  }
  Indic_interm_P1 <- IndicVariant_IndusCountry(IndusREF, CountryREF = "FRA", ListCountryREF = c("FRA", "DEU", "GBR", "ESP", "ITA"), data.table::copy(MRIO), data.table::copy(MRIOinterbis), RatioEmploiVA_ajust)

  # Results of the second part of variant
  Indic_interm_P2 <- IndicVariant_IndusCountry(IndusREF, CountryREF = "FRA", ListCountryREF = c("FRA", "DEU", "GBR", "ESP", "ITA"), data.table::copy(MRIOinterbis), data.table::copy(MRIObis), RatioEmploiVA_ajust)

  # Comparison step by step with itinial HRM variant
  BaseHRM <- readRDS(paste0(PathTest, "Base_Indic_HRM_100MoE.rds"))
  MergeVariants <- rbind(Indic_interm[, Variant := "C29_PostTransition"], Indic_interm_P1[, Variant := "C29_PostTransition_P1"], Indic_interm_P2[, Variant := "C29_PostTransition_P2"], BaseHRM[Variant == "C29", ], Indic_interm_HorsUE[, Variant := "C29_HorsUE"])
  Tab_compar <- dcast(MergeVariants[Lig_Country == "FRA", ], TypeRes ~ Variant, value.var = "value")
  gc()

  toc()
  return(Tab_compar)
}







