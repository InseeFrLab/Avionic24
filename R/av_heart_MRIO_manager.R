#' SplitPRBR
#' Split of PR and BR into four parts or only PR or only BR
#' Please note: country names, branch names and product names must not contain underscores.
#'
#' @param dl data long, like normalized database
#'
#' @return dl data long
#' @export
#'
#' @examples
#' \dontrun{ test <- SplitPRBR(DT) }
SplitPRBR <- function(dl) {
  interm <- data.table::copy(dl)

  # Split of PR and BR
  if ("PR" %in% colnames(dl)) {
    interm$Lig_Country <- stringr::str_extract(interm$PR, "[^_]+")
    interm$Lig_Indus <- substring(stringr::str_extract(interm$PR, "_.+$"), 2)
    interm <- interm[, PR := NULL]
  }
  if ("BR" %in% colnames(dl)) {
    interm$Col_Country <- stringr::str_extract(interm$BR, "[^_]+")
    interm$Col_Indus <- substring(stringr::str_extract(interm$BR, "_.+$"), 2)
    interm <- interm[, BR := NULL]
  }

  return(interm)
}

#######################################################################

#' PastePRBR
#' Paste (Lig_Country and Lig_Indus) into PR and/or (Col_Country and Col_Indus) into BR.
#' Please note: country names, branch names and product names must not contain underscores.
#'
#' @param dl data long, like normalized database
#'
#' @return dl data long
#' @export
#'
#' @examples
#' \dontrun{ test <- PastePRBR(DT)}
PastePRBR <- function(dl) {
  interm <- data.table::copy(dl)
  Coldl <- colnames(interm)
  # Paste of PR and BR
  if (("Lig_Country" %in% Coldl) & ("Lig_Indus" %in% Coldl)) {
    interm$PR <- paste0(interm$Lig_Country, "_", interm$Lig_Indus)
    interm <- interm[, Lig_Country := NULL][, Lig_Indus := NULL]
  }
  if (("Col_Country" %in% Coldl) & ("Col_Indus" %in% Coldl)) {
    interm$BR <- paste0(interm$Col_Country, "_", interm$Col_Indus)
    interm <- interm[, Col_Country := NULL][, Col_Indus := NULL]
  }

  return(interm)
}

#######################################################################
#' GereInfNA
#'  Function (very useful) to convert infinity data into NA and NA data into 0 or 1 for example
#'
#' @param df dataframe
#' @param impute value to inpute
#'
#' @return df data frame
#' @export
#'
#' @examples
#' \dontrun{ if (TypeSPA == "VA") {
#'   f_dt <- vectDF(diag(MRIO[["VAOverOuput"]]))
#'   f_dt <- GereInfNA(f_dt)
#' }}
GereInfNA <- function(df, impute = 0) {
  if (ncol(df) == 1) { # if vector, need to apply lists functions
    df <- lapply(df, function(x) {
      x[is.infinite(x)] <- impute
      return(x)
    })
    df <- lapply(df, function(x) {
      x[is.na(x)] <- impute
      return(x)
    })
    df <- vectDF(df)
  } else { # if matrix with several colums
    is.na(df) <- sapply(df, is.infinite)
    df[is.na(df)] <- impute
  }
  return(df)
}

#######################################################################
#' vectDF
#'  Function to convert a vector into dataframe
#'
#' @param vect vector
#'
#' @return df data frame
#' @export
#'
#' @examples
#' \dontrun{ MyDF <- vectDF(DF[, 1])}
vectDF <- function(vect) {
  out <- as.data.frame(vect, drop = FALSE)
  return(out)
}

#######################################################################
#' GetRownamesFromFirstCol
#' Get Rownames From First Column
#'
#' @param df dataframe
#'
#' @return df data frame
#' @export
GetRownamesFromFirstCol <- function(df) {
  df <- as.data.frame(df)
  rownames(df) <- as.character(df[, 1])
  dfout <- df[, -1, drop = FALSE]
  return(dfout)
}

#######################################################################
#' AddRownamesToFirstCol
#' Add Rownames To First Column
#'
#' @param df dataframe
#'
#' @return df data frame
#' @export
AddRownamesToFirstCol <- function(df) {
  df <- as.data.frame(df)
  df2 <- cbind(rownames(df), df)
  df2 <- as.data.frame(df2)
  colnames(df2)[1] <- "joint"
  return(df2)
}


#######################################################################
#' ReqSum
#' Query function to calculate a sum from a list of dimensions to be summed
#' ListDimASommer is the normalized list of dimensions on wich you wants to sum up.
#' OptStruct keeps initial format of data bur compute structure on the given list of dimensions (ListDimASommer)
#'
#' @param dl data_long format database
#' @param ListDimASommer list of dimensions
#' @param OptStruct option for structure results
#'
#' @return dl data long
#' @export
#'
#' @examples
#' \dontrun{ Cadre <- Base_depart[["DF"]]
#' Cadre_Tot <- ReqSum(Cadre, "Col_Indus") # Sum components of final demand}
ReqSum <- function(dl, ListDimASommer, OptStruct = FALSE) {
  dl <- setDT(dl)
  interm <- data.table::copy(dl)

  ListDepart <- list("year", "Lig_Indus", "Lig_Country", "Col_Indus", "Col_Country")

  if (OptStruct == FALSE) {
    ListHorsDimASommer <- ListDepart[!(ListDepart %in% ListDimASommer)]
    ListHorsDimASommer <- as.character(ListHorsDimASommer)
    setkeyv(interm, ListHorsDimASommer)
    interm_sum <- interm[, sum(value), by = ListHorsDimASommer]
    setnames(interm_sum, "V1", "value")
    intermOut <- interm_sum
  } else {
    ListHorsDimASommer <- ListDepart[!(ListDepart %in% ListDimASommer)]
    ListHorsDimASommer <- as.character(ListHorsDimASommer)
    setkeyv(interm, ListHorsDimASommer)
    interm_sum <- interm[, sum(value), by = ListHorsDimASommer]
    interm_sum <- interm_sum[interm, on = ListHorsDimASommer]
    interm_sum[, "value"] <- interm_sum[, "value"] / interm_sum[, "V1"]
    interm_sum <- GereInfNA(interm_sum)
    intermOut <- interm_sum[, V1 := NULL]
  }
  return(intermOut)
}


#' ListsReferential
#' Function to return lists of countries or operations or both
#' You can add lists depending of new MRIO for exemple, or depending of releases
#'
#' @param TypeList Choose the list type
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{ if (TypeList == "BRFD_FIG_WIOD_LRWIOD_ICIO") {
#'   List_out <- append(ListsReferential("BR_FIG_WIOD_LRWIOD_ICIO"), ListsReferential("FD_FIG_WIOD_LRWIOD_ICIO"))
#' }
#' return(List_out)}
ListsReferential <- function(TypeList) {
  if (TypeList == "GEO_FIG_WIOD_LRWIOD_ICIO") {
    List_out <- c("ABW", "AFG", "AGO", "AIA", "ALA", "ALB", "AND", "ARE", "ARG", "ARM", "ASM", "ATA", "ATF", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLM", "BLR", "BLZ", "BMU", "BOL", "BRA", "BRB", "BRN", "BTN", "BVT", "BWA", "CAF", "CAN", "CCK", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COK", "COL", "COM", "CPV", "CRI", "CUB", "CXR", "CYM", "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", "EST", "ETH", "FIN", "FJI", "FLK", "FRA", "FRO", "FSM", "GAB", "GBR", "GEO", "GGY", "GHA", "GIB", "GIN", "GLP", "GMB", "GNB", "GNQ", "GRC", "GRD", "GRL", "GTM", "GUF", "GUM", "GUY", "HKG", "HMD", "HND", "HRV", "HTI", "HUN", "IDN", "IMN", "IND", "IOT", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JEY", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", "LSO", "LTU", "LUX", "LVA", "MAC", "MAF", "MAR", "MCO", "MDA", "MDG", "MDV", "MEX", "MHL", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MNP", "MOZ", "MRT", "MSR", "MTQ", "MUS", "MWI", "MYS", "MYT", "NAM", "NCL", "NER", "NFK", "NGA", "NIC", "NIU", "NLD", "NOR", "NPL", "NRU", "NZL", "OMN", "PAK", "PAN", "PCN", "PER", "PHL", "PLW", "PNG", "POL", "PRI", "PRK", "PRT", "PRY", "PSE", "PYF", "QAT", "REU", "ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SGS", "SHN", "SJM", "SLB", "SLE", "SLV", "SMR", "SOM", "SPM", "SRB", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TCA", "TCD", "TGO", "THA", "TJK", "TKL", "TKM", "TLS", "TON", "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "UMI", "URY", "USA", "UZB", "VAT", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", "WLF", "WSM", "YEM", "ZAF", "ZMB", "ZWE", "ROW", "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "ARG", "BRA", "BRN", "BGR", "KHM", "CHN", "HRV", "CYP", "IND", "IDN", "HKG", "KAZ", "LAO", "MYS", "MLT", "MAR", "MMR", "PER", "PHL", "ROU", "RUS", "SAU", "SGP", "ZAF", "TWN", "THA", "TUN", "VNM", "ROW", "MEX", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")
  }
  if (TypeList == "GEO_FIG_WIOD_LRWIOD_ICIO_UE27") {
    List_out <- c("ABW", "AFG", "AGO", "AIA", "ALA", "ALB", "AND", "ARE", "ARG", "ARM", "ASM", "ATA", "ATF", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLM", "BLR", "BLZ", "BMU", "BOL", "BRA", "BRB", "BRN", "BTN", "BVT", "BWA", "CAF", "CAN", "CCK", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COK", "COL", "COM", "CPV", "CRI", "CUB", "CXR", "CYM", "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", "EST", "ETH", "FIN", "FJI", "FLK", "FRA", "FRO", "FSM", "GAB", "GBR", "GEO", "GGY", "GHA", "GIB", "GIN", "GLP", "GMB", "GNB", "GNQ", "GRC", "GRD", "GRL", "GTM", "GUF", "GUM", "GUY", "HKG", "HMD", "HND", "HRV", "HTI", "HUN", "IDN", "IMN", "IND", "IOT", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JEY", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", "LSO", "LTU", "LUX", "LVA", "MAC", "MAF", "MAR", "MCO", "MDA", "MDG", "MDV", "MEX", "MHL", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MNP", "MOZ", "MRT", "MSR", "MTQ", "MUS", "MWI", "MYS", "MYT", "NAM", "NCL", "NER", "NFK", "NGA", "NIC", "NIU", "NLD", "NOR", "NPL", "NRU", "NZL", "OMN", "PAK", "PAN", "PCN", "PER", "PHL", "PLW", "PNG", "POL", "PRI", "PRK", "PRT", "PRY", "PSE", "PYF", "QAT", "REU", "ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SGS", "SHN", "SJM", "SLB", "SLE", "SLV", "SMR", "SOM", "SPM", "SRB", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TCA", "TCD", "TGO", "THA", "TJK", "TKL", "TKM", "TLS", "TON", "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "UMI", "URY", "USA", "UZB", "VAT", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", "WLF", "WSM", "YEM", "ZAF", "ZMB", "ZWE", "ROW", "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "ARG", "BRA", "BRN", "BGR", "KHM", "CHN", "HRV", "CYP", "IND", "IDN", "HKG", "KAZ", "LAO", "MYS", "MLT", "MAR", "MMR", "PER", "PHL", "ROU", "RUS", "SAU", "SGP", "ZAF", "TWN", "THA", "TUN", "VNM", "ROW", "MEX", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM", "UE27")
  }
  if (TypeList == "BR_FIG_WIOD_LRWIOD_ICIO") {
    List_out <- c("A01", "A02", "A03", "AtB", "AZ", "B", "C", "C1", "C10-C12", "C10T12", "C13-C15", "C13T15", "C16", "C17", "C18", "C19", "C2", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C3", "C30", "C31_32", "C31_C32", "C33", "C4", "C5", "D", "D15t16", "D17t19", "D21t22", "D23", "D24", "D25", "D26", "D27t28", "D29", "D30t33", "D34t35", "D35", "DE", "Dnec", "E", "E36", "E37-E39", "E37T39", "F", "FZ", "G", "G45", "G46", "G47", "GZ", "H", "H49", "H50", "H51", "H52", "H53", "HZ", "I", "I60t63", "I64", "IZ", "J", "J58", "J59_60", "J59_J60", "J61", "J62_63", "J62_J63", "JZ", "K", "K64", "K65", "K66", "KZ", "L", "L68", "LtQ", "LZ", "M69_70", "M69_M70", "M71", "M72", "M73", "M74_75", "M74_M75", "MN", "N", "N77", "N78", "N79", "N80T82", "O84", "OQ", "P85", "Q", "Q86", "Q87_88", "R_S", "R90T92", "R93", "RU", "S94", "S95", "S96", "T", "U", "01T02", "03", "05T06", "07T08", "09", "10T12", "13T15", "16", "17T18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31T33", "35", "36T39", "41T43", "45T47", "49", "50", "51", "52", "53", "55T56", "58T60", "61", "62T63", "64T66", "68", "69T75", "77T82", "84", "85", "86T88", "90T93", "94T96", "97T98", "BIENS", "SERV")
  }
  if (TypeList == "FD_FIG_WIOD_LRWIOD_ICIO") {
    List_out <- c("CONS_h", "CONS_g", "CONS_np", "GFCF", "INVEN", "P3_S13", "P3_S14", "P3_S15", "P51G", "P5M", "xCONS_h", "xCONS_g", "xGFCF", "xINV", "HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "P33")
  }
  if (TypeList == "BRFD_FIG_WIOD_LRWIOD_ICIO") {
    List_out <- append(ListsReferential("BR_FIG_WIOD_LRWIOD_ICIO"), ListsReferential("FD_FIG_WIOD_LRWIOD_ICIO"))
  }
  return(List_out)
}


#' CompoMRIO
#' Master function in the ecosystem
#' Allow to build Wide MRIO (dw) from Long MRIO (dl), by splitting components
#' You can add other components if needed
#' Afterward you can use av_extend_MRIO_dw() function to extend with environmental data for example
#'
#'@import Rcpp
#'@import RcppArmadillo
#'@import tictoc
#'@import xlsx
#'@import dplyr
#'@import data.table
#'@import stringr
#'@import readxl
#'@import ggplot2
#'
#' @param MRIO_dl long format MRIO
#' @param typeCompo option type of composition
#' @param date year
#' @param OptTab tabular output option
#' @param OptUE27 option with UE27 modality
#'
#' @return dw data wide (list of components)
#' @export
#'
#' @examples
#' \dontrun{ DT <- readRDS(paste0(PathTest, "BDn_FIG_2010.rds"))
#' Bonus <- CompoMRIO(DT, typeCompo = "OptFullOptionsBonus", date = 2010, OptTab = FALSE)}
CompoMRIO <- function(MRIO_dl, typeCompo, date = 9999, OptTab = FALSE, OptUE27 = FALSE) # options: =CI:DF;PROD;VA;A;B;CI_PR;CI_BR;DF_TOT;L;invB ;OptFullOptions ;OptFullOptionsBonus : les deux dernières options sortent des listes avec l'ensemble des sorties individuelles (le version 'Bonus' est exhaustive mais plus longue car inversions matricielles
{
  interm <- data.table::copy(MRIO_dl)

  if (date != 9999) {
    interm <- interm[year == date, ]
  }

  List_GEO <- ListsReferential("GEO_FIG_WIOD_LRWIOD_ICIO")
  if (OptUE27 == TRUE) {
    List_GEO <- ListsReferential("GEO_FIG_WIOD_LRWIOD_ICIO_UE27")
  }
  List_BR <- ListsReferential("BR_FIG_WIOD_LRWIOD_ICIO")
  List_DF <- ListsReferential("FD_FIG_WIOD_LRWIOD_ICIO")
  List_BRDF <- ListsReferential("BRFD_FIG_WIOD_LRWIOD_ICIO")

  if (typeCompo == "CI") {
    MRIO_dw <- interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BR, ]
    if (OptTab == TRUE) {
      MRIO_dw <- dcast(MRIO_dw, year + Lig_Country + Lig_Indus ~ Col_Country + Col_Indus, value.var = "value")
    } # Mise en format tab
  }
  if (typeCompo == "DF") {
    MRIO_dw <- interm[Lig_Indus %in% List_BR & Col_Indus %in% List_DF, ]
    if (OptTab == TRUE) {
      MRIO_dw <- dcast(MRIO_dw, year + Lig_Country + Lig_Indus ~ Col_Country + Col_Indus, value.var = "value")
    } # Mise en format tab
  }
  if (typeCompo == "PROD") { # On-the-fly recalculation from MRIO to avoid inconsistencies
    interm <- interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BRDF, ]
    MRIO_dw <- interm[, .(sum(value)), by = c("year", "Lig_Indus", "Lig_Country")]
    setnames(MRIO_dw, "V1", "value")
    setorder(MRIO_dw, Lig_Country, Lig_Indus)
  }
  if (typeCompo == "A") { # Calculation of technical coefficients
    Part_CI <- CompoMRIO(interm, "CI", date = date, OptUE27 = OptUE27)
    Part_Prod <- CompoMRIO(interm, "PROD", date = date, OptUE27 = OptUE27)
    setnames(Part_Prod, "Lig_Indus", "Col_Indus")
    setnames(Part_Prod, "Lig_Country", "Col_Country")
    A_merge <- Part_Prod[Part_CI, on = .(Col_Indus, Col_Country, year)]
    A_merge$value <- A_merge$i.value / A_merge$value
    A_merge <- GereInfNA(A_merge[, i.value := NULL])
    MRIO_dw <- A_merge
    if (OptTab == TRUE) {
      MRIO_dw <- dcast(MRIO_dw, year + Lig_Country + Lig_Indus ~ Col_Country + Col_Indus, value.var = "value")
    } # Wide format
  }
  if (typeCompo == "B") { # Calculation of economic coefficients of use
    Part_CI <- CompoMRIO(interm, "CI", date = date, OptUE27 = OptUE27)
    Part_Prod <- CompoMRIO(interm, "PROD", date = date, OptUE27 = OptUE27)
    B_merge <- Part_Prod[Part_CI, on = .(Lig_Indus, Lig_Country, year)]
    B_merge$value <- B_merge$i.value / B_merge$value
    B_merge <- GereInfNA(B_merge[, i.value := NULL])
    MRIO_dw <- B_merge
    if (OptTab == TRUE) {
      MRIO_dw <- dcast(MRIO_dw, year + Lig_Country + Lig_Indus ~ Col_Country + Col_Indus, value.var = "value")
    } # Wide format
  }
  if (typeCompo == "CI_PR") { # On-the-fly recalculation from MRIO to avoid inconsistencies
    interm <- CompoMRIO(interm, "CI", date = date, OptUE27 = OptUE27)
    MRIO_dw <- interm[, .(sum(value)), by = c("year", "Lig_Indus", "Lig_Country")]
    setnames(MRIO_dw, "V1", "value")
    setorder(MRIO_dw, Lig_Country, Lig_Indus)
  }
  if (typeCompo == "CI_BR") { # On-the-fly recalculation from MRIO to avoid inconsistencies
    interm <- CompoMRIO(interm, "CI", date = date, OptUE27 = OptUE27)
    MRIO_dw <- interm[, .(sum(value)), by = c("year", "Col_Indus", "Col_Country")]
    setnames(MRIO_dw, "V1", "value")
    setorder(MRIO_dw, Col_Country, Col_Indus)
  }
  if (typeCompo == "DF_TOT") { # On-the-fly recalculation from MRIO to avoid inconsistencies
    interm <- CompoMRIO(interm, "DF", date = date, OptUE27 = OptUE27)
    MRIO_dw <- interm[, .(sum(value)), by = c("year", "Lig_Indus", "Lig_Country")]
    setnames(MRIO_dw, "V1", "value")
    setorder(MRIO_dw, Lig_Country, Lig_Indus)
  }
  if (typeCompo == "VA") {
    # First we extract the IC crosstable
    Part_SommeCI <- CompoMRIO(interm, "CI_BR", date = date, OptUE27 = OptUE27)
    Part_SommeCI$value <- -Part_SommeCI$value
    # The output is then calculated
    Part_Prod <- CompoMRIO(interm, "PROD", date = date, OptUE27 = OptUE27)
    # We calculate the balance between production and the sum of Branch ICs (concept of VA at basic prices).
    setnames(Part_Prod, "Lig_Indus", "Col_Indus") # Recode to convert PR to BR
    setnames(Part_Prod, "Lig_Country", "Col_Country") # Recode to convert PR to BR
    interm2 <- rbind(Part_Prod, Part_SommeCI)
    MRIO_dw <- interm2[, .(sum(value)), by = c("year", "Col_Indus", "Col_Country")]
    setnames(MRIO_dw, "V1", "value")
    setorder(MRIO_dw, Col_Country, Col_Indus)
  }
  if (typeCompo == "L") { # Calculating the Leontief inverse
    A <- CompoMRIO(interm, "A", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    nb_ligcol <- nrow(A)
    MatrixA <- as.matrix(A[, 4:ncol(A)])
    interm2 <- inversion_rcpp3(diag(nb_ligcol) - MatrixA)
    colnames(interm2) <- colnames(A)[4:ncol(A)]
    rownames(interm2) <- colnames(A)[4:ncol(A)]
    MRIO_dw <- as.data.frame(interm2)
  }
  if (typeCompo == "invB") { # Calculating the Ghosh inverse
    B <- CompoMRIO(interm, "B", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    nb_ligcol <- nrow(B)
    MatrixB <- as.matrix(B[, 4:ncol(B)])
    interm2 <- inversion_rcpp3(diag(nb_ligcol) - MatrixB)
    colnames(interm2) <- colnames(B)[4:ncol(B)]
    rownames(interm2) <- colnames(B)[4:ncol(B)]
    MRIO_dw <- as.data.frame(interm2)
  }
  if (typeCompo == "Imports") { # Calculating imports
    CI <- CompoMRIO(interm, "CI", date = date, OptTab = F, OptUE27 = OptUE27)
    DF <- CompoMRIO(interm, "DF", date = date, OptTab = F, OptUE27 = OptUE27)
    CIDF <- rbind(CI, DF)
    CIDF_imports <- CIDF[Lig_Country != Col_Country, sum(value, na.rm = T), by = c("year", "Col_Country", "Lig_Indus")]
    setnames(CIDF_imports, c("V1", "Col_Country"), c("value", "Lig_Country"))
    MRIO_dw <- as.data.frame(CIDF_imports[order(Lig_Country, Lig_Indus)])
    MRIO_dw <- setDT(MRIO_dw)
  }
  if (typeCompo == "Exports") { # Calculating exports
    CI <- CompoMRIO(interm, "CI", date = date, OptTab = F, OptUE27 = OptUE27)
    DF <- CompoMRIO(interm, "DF", date = date, OptTab = F, OptUE27 = OptUE27)
    CIDF <- rbind(CI, DF)
    CIDF_exports <- CIDF[Lig_Country != Col_Country, sum(value, na.rm = T), by = c("year", "Lig_Country", "Lig_Indus")]
    setnames(CIDF_exports, "V1", "value")
    MRIO_dw <- as.data.frame(CIDF_exports[order(Lig_Country, Lig_Indus)])
    MRIO_dw <- setDT(MRIO_dw)
  }
  if (typeCompo == "StressVA") { # Calculation of VA/PROD stressor
    PROD <- CompoMRIO(interm, "PROD", date = date, OptTab = F, OptUE27 = OptUE27)
    VA <- CompoMRIO(interm, "VA", date = date, OptTab = F, OptUE27 = OptUE27)
    f_VA <- VA
    f_VA$value <- f_VA$value / PROD$value
    f_VA <- GereInfNA(f_VA)
    f_VA <- diag(unlist(f_VA$value))
    MRIO_dw <- as.matrix(f_VA)
  }
  if (typeCompo == "StressEmi") { # Calculation of Emi/PROD stressor
    PROD <- CompoMRIO(interm, "PROD", date = date, OptTab = F, OptUE27 = OptUE27)
    VA <- CompoMRIO(interm, "VA", date = date, OptTab = F, OptUE27 = OptUE27)
    f_Emi <- PROD
    f_Emi$value <- RecupVectEmiFigaro(2017, AjoutPRBR(f_Emi))
    f_Emi <- AjoutPRBR(f_Emi)
    f_Emi[, PR := NULL]
    f_Emi$value <- f_Emi$value / PROD$value
    f_Emi <- GereInfNA(f_Emi)
    f_Emi <- diag(unlist(f_Emi$value))
    MRIO_dw <- as.matrix(f_Emi)
  }
  if (typeCompo == "OptFullOptions") {
    interm1 <- CompoMRIO(MRIO_dl, "PROD", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm2 <- CompoMRIO(MRIO_dl, "CI", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm3 <- CompoMRIO(MRIO_dl, "DF", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm4 <- CompoMRIO(MRIO_dl, "VA", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm5 <- CompoMRIO(MRIO_dl, "CI_PR", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm6 <- CompoMRIO(MRIO_dl, "CI_BR", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm7 <- CompoMRIO(MRIO_dl, "DF_TOT", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm8 <- CompoMRIO(MRIO_dl, "A", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm2tab <- CompoMRIO(MRIO_dl, "CI", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm3tab <- CompoMRIO(MRIO_dl, "DF", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm8tab <- CompoMRIO(MRIO_dl, "A", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm13 <- CompoMRIO(MRIO_dl, "Imports", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm14 <- CompoMRIO(MRIO_dl, "Exports", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    MRIO_dw <- list(PROD = interm1, CI = interm2, DF = interm3, VA = interm4, CI_PR = interm5, CI_BR = interm6, DF_TOT = interm7, A = interm8, CI_tab = interm2tab, DF_tab = interm3tab, A_tab = interm8tab, Imports = interm13, Exports = interm14)
  }
  if (typeCompo == "OptFullOptionsBonus") {
    interm1 <- CompoMRIO(MRIO_dl, "PROD", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm2 <- CompoMRIO(MRIO_dl, "CI", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm3 <- CompoMRIO(MRIO_dl, "DF", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm4 <- CompoMRIO(MRIO_dl, "VA", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm5 <- CompoMRIO(MRIO_dl, "CI_PR", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm6 <- CompoMRIO(MRIO_dl, "CI_BR", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm7 <- CompoMRIO(MRIO_dl, "DF_TOT", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm8 <- CompoMRIO(MRIO_dl, "A", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm2tab <- CompoMRIO(MRIO_dl, "CI", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm3tab <- CompoMRIO(MRIO_dl, "DF", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm8tab <- CompoMRIO(MRIO_dl, "A", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm9 <- CompoMRIO(MRIO_dl, "B", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm10 <- CompoMRIO(MRIO_dl, "B", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm11 <- CompoMRIO(MRIO_dl, "L", date = date, OptUE27 = OptUE27) # Results directly in table form
    interm12 <- CompoMRIO(MRIO_dl, "invB", date = date, OptUE27 = OptUE27) # Results directly in table form
    interm13 <- CompoMRIO(MRIO_dl, "Imports", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm14 <- CompoMRIO(MRIO_dl, "Exports", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    MRIO_dw <- list(PROD = interm1, CI = interm2, DF = interm3, VA = interm4, CI_PR = interm5, CI_BR = interm6, DF_TOT = interm7, A = interm8, CI_tab = interm2tab, DF_tab = interm3tab, A_tab = interm8tab, B = interm9, B_tab = interm10, L = interm11, InvBGhosh = interm12, Imports = interm13, Exports = interm14)
  }
  if (typeCompo == "OptFullOptionsBonusStressFIG") {
    interm1 <- CompoMRIO(MRIO_dl, "PROD", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm2 <- CompoMRIO(MRIO_dl, "CI", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm3 <- CompoMRIO(MRIO_dl, "DF", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm4 <- CompoMRIO(MRIO_dl, "VA", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm5 <- CompoMRIO(MRIO_dl, "CI_PR", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm6 <- CompoMRIO(MRIO_dl, "CI_BR", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm7 <- CompoMRIO(MRIO_dl, "DF_TOT", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm8 <- CompoMRIO(MRIO_dl, "A", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm2tab <- CompoMRIO(MRIO_dl, "CI", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm3tab <- CompoMRIO(MRIO_dl, "DF", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm8tab <- CompoMRIO(MRIO_dl, "A", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm9 <- CompoMRIO(MRIO_dl, "B", date = date, OptTab = FALSE, OptUE27 = OptUE27)
    interm10 <- CompoMRIO(MRIO_dl, "B", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm11 <- CompoMRIO(MRIO_dl, "L", date = date, OptUE27 = OptUE27) # Results directly in table form
    interm12 <- CompoMRIO(MRIO_dl, "invB", date = date, OptUE27 = OptUE27) # Results directly in table form
    interm13 <- CompoMRIO(MRIO_dl, "Imports", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm14 <- CompoMRIO(MRIO_dl, "Exports", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm15 <- CompoMRIO(MRIO_dl, "StressVA", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    interm16 <- CompoMRIO(MRIO_dl, "StressEmi", date = date, OptTab = TRUE, OptUE27 = OptUE27)
    MRIO_dw <- list(PROD = interm1, CI = interm2, DF = interm3, VA = interm4, CI_PR = interm5, CI_BR = interm6, DF_TOT = interm7, A = interm8, CI_tab = interm2tab, DF_tab = interm3tab, A_tab = interm8tab, B = interm9, B_tab = interm10, L = interm11, InvBGhosh = interm12, Imports = interm13, Exports = interm14, StressVA_FIG = interm15, StressEmi_FIG = interm16)
  }
  return(MRIO_dw)
}


#' AjoutPRBR
#' Add PR and BR to help transposing tables to wide format (eg. matrix like A or L)
#'
#' @param dl long data
#'
#' @return dl data long
#' @export
#'
#' @examples
#' \dontrun{ f_dt <- AjoutPRBR(interm)
#' y_dt <- AjoutPRBR(y_dt)}
AjoutPRBR <- function(dl) {
  interm <- data.table::copy(dl)
  interm$PR <- paste0(interm$Lig_Country, "_", interm$Lig_Indus)
  interm$BR <- paste0(interm$Col_Country, "_", interm$Col_Indus)
  return(interm)
}


#' dw_to_dl
#' Convert dw to dl
#'
#' @param dw wide data
#'
#' @return dl data long
#' @export
dw_to_dl <- function(dw) {
  interm <- data.table::copy(dw)
  part1 <- interm[["CI"]]
  part2 <- interm[["DF"]]
  part3 <- interm[["PROD"]][, Col_Country := "PROD"][, Col_Indus := "TOTAL"]
  out <- rbind(part1, part2, part3)
  return(out)
}


#' SommeDFenP3_S14
#' Function which sums up all final demand and stores it in P3_14
#' Caution: potentially biases detailed interpretation of MRIO components
#' On the other hand, remains compatible with all functions and avoids the weird things of P5M (stock variatins that make HRM jump on C22 in particular)
#' Only works with FIGARO at this time, needs more lists if you wants to expand
#'
#' @param base_dl long database
#'
#' @return dl data long
#' @export
#'
#' @examples
#' \dontrun{ if (OptSommeDFenP3_S14 == TRUE) {
#'   Base_init <- SommeDFenP3_S14(data.table::copy(dl))
#' }}
SommeDFenP3_S14 <- function(base_dl) {
  AlterBaseFIG <- data.table::copy(base_dl)
  DemFin <- AlterBaseFIG[Col_Indus %in% c("P3_S13", "P3_S14", "P3_S15", "P51G", "P5M"), ]
  DemFinAgg <- DemFin[, Col_Indus := "P3_S14"]
  DemFinAgg <- DemFinAgg[, sum(value), by = c("year", "Lig_Indus", "Lig_Country", "Col_Country", "Col_Indus")]
  setnames(DemFinAgg, "V1", "value")
  AlterBaseFIGhorsDemFin <- AlterBaseFIG[!Col_Indus %in% c("P3_S13", "P3_S14", "P3_S15", "P51G", "P5M"), ]
  AlterBaseFIG <- rbind(AlterBaseFIGhorsDemFin, DemFinAgg)
  print("Final demand summed up")
  return(AlterBaseFIG)
}

#' SoldExtPays
#' Function to calculate external balance for all countries of a MRIO
#'
#' @param MRIOinterm MRIO object
#'
#' @return df data frame
#' @export
#'
#' @examples
#' \dontrun{ SoldeExt <- SoldExtPays(MRIO)}
SoldExtPays <- function(MRIOinterm) {
  Exports <- MRIOinterm[["Exports"]]
  Imports <- MRIOinterm[["Imports"]]
  SoldExt <- Exports # init
  SoldExt$value <- Exports$value - Imports$value
  SoldExt <- SoldExt[, c("year", "Lig_Country", "Lig_Indus", "value")]
  SoldExt <- setDT(SoldExt)
  SoldExt <- SoldExt[, sum(value, na.rm = T), by = c("year", "Lig_Country")]
  setnames(SoldExt, "V1", "value")
  return(SoldExt)
}


#' Info_MRIO
#' Info_MRIO function: provides information on a product * country (equivalent to branch * country)
#' MRIO must be derived from compoMRIO
#' nb_top : number of crosses to display (ex: nb of countries of origin)
#' Output in print format and save possible.
#'
#' @param MRIO MRIO object
#' @param Indus_select Industry selection
#' @param Country_select Country selection
#' @param nb_top Number of outputs
#' @param verbose verbose option
#'
#' @return text
#' @export
#'
#' @examples
#' \dontrun{ Info_MRIO(MRIO, "A01", "FRA", nb_top = 5)
#' test <- Info_MRIO(MRIO, "C19", "FRA", nb_top = 10) / print(test)}
Info_MRIO <- function(MRIO, Indus_select, Country_select, nb_top = 5, verbose = T) {
  annee_w <- as.numeric(MRIO[["PROD"]][1, "year"])
  prod_dt <- MRIO[["PROD"]]
  prod_w <- round(as.numeric(prod_dt[Lig_Indus == Indus_select & Lig_Country == Country_select, "value"]), 0)
  prod_w_pct <- 100 * prod_w / as.numeric(prod_dt[Lig_Country == Country_select, sum(value)])

  CI_dt <- MRIO[["CI"]]
  CI_w <- round(CI_dt[Lig_Indus == Indus_select & Lig_Country == Country_select, sum(value)], 0)
  CI_DOM_w <- round(CI_dt[Lig_Indus == Indus_select & Lig_Country == Country_select & Col_Country == Country_select, sum(value)], 0)
  CI_EXP_w <- round(CI_dt[Lig_Indus == Indus_select & Lig_Country == Country_select & Col_Country != Country_select, sum(value)], 0)
  CI_w_pct <- 100 * CI_w / CI_dt[Lig_Country == Country_select, sum(value)]
  CI_EXP_topN_w <- arrange(top_n(CI_dt[Lig_Indus == Indus_select & Lig_Country == Country_select & Col_Country != Country_select, ], nb_top, value), desc(value))


  CI_BR_w <- round(CI_dt[Col_Indus == Indus_select & Col_Country == Country_select, sum(value)], 0)
  CI_BR_DOM_w <- round(CI_dt[Col_Indus == Indus_select & Col_Country == Country_select & Lig_Country == Country_select, sum(value)], 0)
  CI_BR_IMP_w <- round(CI_dt[Col_Indus == Indus_select & Col_Country == Country_select & Lig_Country != Country_select, sum(value)], 0)
  CI_BR_w_pct <- 100 * CI_BR_w / CI_dt[Col_Country == Country_select, sum(value)]
  CI_BR_topN_w <- arrange(top_n(CI_dt[Col_Indus == Indus_select & Col_Country == Country_select & Lig_Country == Country_select, ], nb_top, value), desc(value))
  CI_BR_IMP_topN_w <- arrange(top_n(CI_dt[Col_Indus == Indus_select & Col_Country == Country_select & Lig_Country != Country_select, ], nb_top, value), desc(value))

  VA_BR_w <- round(MRIO[["VA"]][Col_Indus == Indus_select & Col_Country == Country_select, sum(value)], 0)
  Txva_BR_w <- 100 * VA_BR_w / prod_w
  Txva_moy_BR_w <- 100 * MRIO[["VA"]][Col_Country == Country_select, sum(value)] / prod_dt[Lig_Country == Country_select, sum(value)]
  Txva_moy_inter_BR_w <- 100 * MRIO[["VA"]][Col_Indus == Indus_select, sum(value)] / prod_dt[Lig_Indus == Indus_select, sum(value)]


  DF_dt <- MRIO[["DF"]]
  DF_TOT_w <- round(DF_dt[Lig_Indus == Indus_select & Lig_Country == Country_select, sum(value)], 0)
  DF_DETAIL_w <- DF_dt[Lig_Indus == Indus_select & Lig_Country == Country_select, sum(value), by = c("Col_Indus")]
  DF_TOT_DOM_w <- round(DF_dt[Lig_Indus == Indus_select & Lig_Country == Country_select & Col_Country == Country_select, sum(value)], 0)
  DF_TOT_EXP_w <- round(DF_dt[Lig_Indus == Indus_select & Lig_Country == Country_select & Col_Country != Country_select, sum(value)], 0)
  DF_TOT_w_pct <- 100 * DF_TOT_w / DF_dt[Lig_Country == Country_select, sum(value)]
  DF_TOT_EXP_topN_w <- arrange(top_n(DF_dt[Lig_Indus == Indus_select & Lig_Country == Country_select & Col_Country != Country_select, ], nb_top, value), desc(value))

  DF_BR_TOT_w <- round(DF_dt[Lig_Indus == Indus_select & Col_Country == Country_select, sum(value)], 0)
  # DF_BR_DETAIL_w<-DF_dt[Lig_Indus==Indus_select & Lig_Country==Country_select & Col_Country==Country_select,sum(value),by=c("Col_Indus")]
  DF_BR_TOT_DOM_w <- round(DF_dt[Lig_Indus == Indus_select & Lig_Country == Country_select & Col_Country == Country_select, sum(value)], 0)
  DF_BR_TOT_IMP_w <- round(DF_dt[Lig_Indus == Indus_select & Lig_Country != Country_select & Col_Country == Country_select, sum(value)], 0)
  DF_BR_TOT_w_pct <- round(100 * DF_BR_TOT_DOM_w / DF_BR_TOT_w, 2)
  DF_BR_TOT_IMP_topN_w <- arrange(top_n(DF_dt[Lig_Indus == Indus_select & Lig_Country != Country_select & Col_Country == Country_select, ], nb_top, value), desc(value))


  if (verbose == T) {
    L0 <- "######################################################################"
    L1 <- paste("####  Product/ industry info", Indus_select, "from country", Country_select, "in", annee_w)
    L2 <- "######################################################################"
    L3 <- "### online reading of the resource-use balance (RUE) :"
    L4 <- paste("Output:= ", prod_w, " (rep", rndN(prod_w_pct, rnd = 3), "% of the output of", Country_select, ") = CI:=", CI_w, " (rep", rndN(CI_w_pct, rnd = 3), "% de la CI) + DF:=", DF_TOT_w, " (rep", rndN(DF_TOT_w_pct, rnd = 3), "% de la DF)")
    L5 <- paste("CI (", CI_w, ") are made up of ", CI_DOM_w, "domestic CI and", CI_EXP_w, "exported CI")
    L6 <- paste("CI's main export destinations by product", Indus_select, "are :", PasteN(CI_EXP_topN_w, c("Col_Country", "Col_Indus")))
    L7 <- paste("Final product demand (line)", Country_select, "equal  ", DF_TOT_w, "M hence", rndN(DF_TOT_w_pct, rnd = 2), "% of the final demand of", Country_select, "of which", DF_TOT_DOM_w, "of domestic final demand and", DF_TOT_EXP_w, "of final demand exported")
    L8 <- paste("The main export destinations for final product demand", Indus_select, "are :", PasteN(DF_TOT_EXP_topN_w, c("Col_Country", "Col_Indus")))

    L9 <- "######################################################################"
    L10 <- paste("### Column reading (branch, CI level) of the branch's accounting balance", Indus_select, "de", Country_select, ":")
    L11 <- paste("Prod (industry=product) := ", prod_w, " (rep", rndN(prod_w_pct, rnd = 3), "% of the output of", Country_select, ")")
    L12 <- paste("Industries series (", CI_BR_w, ") are made up of ", CI_BR_DOM_w, "domestic CI and", CI_BR_IMP_w, "CI importées. Elles représentent", round(CI_BR_w_pct, 2), "% of the CI of", Country_select)
    L13 <- paste("The main domestic CIs consumed by the branch", Indus_select, "are :", PasteN(CI_BR_topN_w, "Lig_Indus"))
    L14 <- paste("The main sources of CI imported by the industry", Indus_select, "are :", PasteN(CI_BR_IMP_topN_w, c("Lig_Country", "Lig_Indus")))
    L15 <- paste("The branch's value added (VA)", Country_select, "is equal to", VA_BR_w, "i.e. a VA rate (=VA/PROD at base price) of", round(Txva_BR_w, 2), "to be compared with the average VA rate for this industry in all the countries of", round(Txva_moy_inter_BR_w, 2), "and should also be compared with the country's average VA rate (all industries combined) of", round(Txva_moy_BR_w, 2))

    L16 <- "######################################################################"
    L17 <- paste("### Column reading (at final demand level) in product line", Indus_select, "de", Country_select, ":")
    L18 <- paste("Final product demand", Indus_select, "of", Country_select, "(", DF_BR_TOT_w, ") are made up of ", DF_BR_TOT_DOM_w, "domestif DF and", DF_BR_TOT_IMP_w, "imported DF. domestic FD (not the 'made-in' but just the direct share for this product) therefore represents", DF_BR_TOT_w_pct, "% of the DF of", Country_select, "in this product")
    L19 <- paste("The main origins of DF imported as a product", Indus_select, "are :", PasteN(DF_BR_TOT_IMP_topN_w, c("Lig_Country", "Lig_Indus", "Col_Indus")))

    L999 <- "#####################################################################"

    # cat(L0,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,L17,L18,L19,L999, sep = '\n')
    out <- capture.output(cat(L0, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, L14, L15, L16, L17, L18, L19, L999, sep = "\n"))
  }
  return(out)
}


#' rndN
#' Rounding function, default 0 decimal places
#'
#' @param dataa data to round
#' @param rnd round decimal option
#'
#' @return rounded value
#' @export
rndN <- function(dataa, rnd = 0) {
  return(round(as.numeric(dataa), rnd))
}


#' PasteN
#' Function to concatenate the amounts (in value) of several columns of a DT (in list form)
#'
#' @param dt datatable
#' @param column_ref column number
#'
#' @return text
#' @export
#'
#' @examples
#' \dontrun{ P2 <- PasteN(DF_TOT_EXP_topN_w, c("Col_Country", "Col_Indus"))}
PasteN <- function(dt, column_ref) {
  nrow_w <- nrow(dt)
  nbcol_w <- length(column_ref)
  text_w <- " "
  lib_col <- " "

  if (nbcol_w > 1) { # If more than one column
    for (k in 1:nrow_w) {
      lib_col <- " "
      for (j in 1:nbcol_w) {
        lib_col <- paste(lib_col, dt[k, get(column_ref[[j]])], sep = "_")
      }
      text_w <- paste(text_w, lib_col, ":", rndN(dt[k, "value"]))
    }
  } else {
    for (k in 1:nrow_w) {
      text_w <- paste(text_w, dt[k, get(column_ref)], ":", rndN(dt[k, "value"]))
    }
  }

  return(text_w)
}


#' av_MRIO_comparison
#' Function to compare 2 MRIOs
#' MRIOs can be initially in long format (Optdl) or in wide format (split comparison of all components of the MRIO)
#'
#' @param MRIO1 MRIO object
#' @param MRIO2 MRIO object
#' @param Optdl long data option
#' @param OptVerbose verbose option
#'
#' @return An ordered comparison (if dl) or a list of ordered comparisons (if wide format)
#' @export
#'
#' @examples
#' \dontrun{ M_test <- av_MRIO_comparison(Bonus, BonusBis, Optdl = F, OptVerbose = T)}
av_MRIO_comparison <- function(MRIO1, MRIO2, Optdl = TRUE, OptVerbose = FALSE) {
  if (Optdl == TRUE) {
    if (OptVerbose == T) {
      print(paste0("MRIO1 := ", deparse(substitute(MRIO1))))
      print(paste0("MRIO2 := ", deparse(substitute(MRIO2))))
    }
    MRIO1 <- setDT(MRIO1)
    MRIO2 <- setDT(MRIO2)
    Interm_merge <- rbind(MRIO1[, TypMRIO := deparse(substitute(MRIO1))], MRIO2[, TypMRIO := deparse(substitute(MRIO2))])
    Interm_tab <- dcast(Interm_merge, Lig_Country + Col_Country + Lig_Indus + Col_Indus ~ TypMRIO, value.var = "value")
    Interm_tab$Diff <- Interm_tab$MRIO2 - Interm_tab$MRIO1
    Interm_tab <- Interm_tab[order(-abs(Diff))]
    out <- Interm_tab
    if (OptVerbose == T) {
      print(head(Interm_tab))
    }
  }
  if (Optdl == FALSE) {
    Diff_CI <- av_MRIO_comparison(MRIO1[["CI"]], MRIO2[["CI"]], Optdl = TRUE, OptVerbose = FALSE)
    Diff_DF <- av_MRIO_comparison(MRIO1[["DF"]], MRIO2[["DF"]], Optdl = TRUE, OptVerbose = FALSE)

    Interm_merge <- rbind(MRIO1[["PROD"]][, TypMRIO := deparse(substitute(MRIO1))], MRIO2[["PROD"]][, TypMRIO := deparse(substitute(MRIO2))])
    Interm_tab <- dcast(Interm_merge, Lig_Country + Lig_Indus ~ TypMRIO, value.var = "value")
    Interm_tab$Diff <- Interm_tab$MRIO2 - Interm_tab$MRIO1
    Diff_PROD <- Interm_tab[order(-abs(Diff))]

    Interm_merge <- rbind(MRIO1[["VA"]][, TypMRIO := deparse(substitute(MRIO1))], MRIO2[["VA"]][, TypMRIO := deparse(substitute(MRIO2))])
    Interm_tab <- dcast(Interm_merge, Col_Country + Col_Indus ~ TypMRIO, value.var = "value")
    Interm_tab$Diff <- Interm_tab$MRIO2 - Interm_tab$MRIO1
    Diff_VA <- Interm_tab[order(-abs(Diff))]

    Diff_A <- av_MRIO_comparison(MRIO1[["A"]], MRIO2[["A"]], Optdl = TRUE, OptVerbose = FALSE)

    Interm_merge <- rbind(setDT(MRIO1[["Imports"]])[, TypMRIO := deparse(substitute(MRIO1))], setDT(MRIO2[["Imports"]])[, TypMRIO := deparse(substitute(MRIO2))])
    Interm_tab <- dcast(Interm_merge, Lig_Country + Lig_Indus ~ TypMRIO, value.var = "value")
    Interm_tab$Diff <- Interm_tab$MRIO2 - Interm_tab$MRIO1
    Diff_Imports <- Interm_tab[order(-abs(Diff))]

    Interm_merge <- rbind(setDT(MRIO1[["Exports"]])[, TypMRIO := deparse(substitute(MRIO1))], setDT(MRIO2[["Exports"]])[, TypMRIO := deparse(substitute(MRIO2))])
    Interm_tab <- dcast(Interm_merge, Lig_Country + Lig_Indus ~ TypMRIO, value.var = "value")
    Interm_tab$Diff <- Interm_tab$MRIO2 - Interm_tab$MRIO1
    Diff_Exports <- Interm_tab[order(-abs(Diff))]

    out <- list(Diff_PROD = Diff_PROD, Diff_CI = Diff_CI, Diff_DF = Diff_DF, Diff_VA = Diff_VA, Diff_A = Diff_A, Diff_Imports = Diff_Imports, Diff_Exports = Diff_Exports)

    if (OptVerbose == T) {
      print("Diff_PROD : Output")
      print(head(Diff_PROD))
      print("Diff_CI : Intermediate consumption")
      print(head(Diff_CI))
      print("Diff_DF : Final Demand")
      print(head(Diff_DF))
      print("Diff_VA : Value Added")
      print(head(Diff_VA))
      print("Diff_A")
      print(head(Diff_A))
      print("Diff_Imports")
      print(head(Diff_Imports))
      print("Diff_Exports")
      print(head(Diff_Exports))
    }
  }
  return(out)
}
