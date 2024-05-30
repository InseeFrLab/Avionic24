# library(data.table)
# library(stringr)
# library(readxl)
# library(ggplot2)


#' av_create_FIGAROixi_2022
#'
#' Figaro 2022 release, CSV flat format : link : https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/database#CSV%20flat%20format%20(FIGARO%202022%20edition)
#'
#' @param Path_FIG22ixi path of FIGARO ixi CSV flat format files
#' @param Path_out path to save normalized data
#' @param OptAnnual option to keep one files by year instead of one file with all years. Can provide a degraded solution if a single database requires too much memory.
#'
#' @return Nothing. Only save data into normalized format.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Path_FIG22ixi <- paste0(PathTemp, "ixi/")
#' Path_out <- PathTest
#' av_create_FIGAROixi_2022(Path_FIG22ixi, Path_out, OptAnnual = TRUE)
#' av_create_FIGAROixi_2022(Path_FIG22ixi, Path_out, OptAnnual = FALSE)
#' testAll <- readRDS(paste0(Path_out, "/BDn_FIG.rds"))
#' test2015 <- readRDS(paste0(Path_out, "/BDn_FIG_", 2015, ".rds"))}
av_create_FIGAROixi_2022 <- function(Path_FIG22ixi, Path_out, OptAnnual = FALSE) {
  # Load concordance tables
  TabPassGEO <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_GEO", col_names = TRUE)
  TabPassIND <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_PR", col_names = TRUE)

  ### iteration part and bind on FIgaro years
  for (k in 2010:2020) {
    mrio_fig22_20XX <- fread(paste0(Path_FIG22ixi, paste0("matrix_eu-ic-io_ind-by-ind_", k, ".csv")))

    # Adding production by calculation
    mrio_fig22_20XX$Prod <- rowSums(mrio_fig22_20XX[, 2:3175])
    setnames(mrio_fig22_20XX, "rowLabels", "PR")

    # convert to long format
    mrio_fig22_20XX_long <- melt(mrio_fig22_20XX,
      id.vars = c("PR"),
      measure.vars = 2:3176
    )
    setnames(mrio_fig22_20XX_long, "variable", "BR")

    # Split of _
    mrio_fig22_20XX_long$annee <- as.numeric(k)
    mrio_fig22_20XX_long$Contry_Lig <- stringr::str_extract(mrio_fig22_20XX_long$PR, "[^_]+")
    mrio_fig22_20XX_long$PR_Lig <- substring(stringr::str_extract(mrio_fig22_20XX_long$PR, "_.+$"), 2)
    mrio_fig22_20XX_long$Contry_Col <- stringr::str_extract(mrio_fig22_20XX_long$BR, "[^_]+")
    mrio_fig22_20XX_long$PR_Col <- substring(stringr::str_extract(mrio_fig22_20XX_long$BR, "_.+$"), 2)
    mrio_fig22_20XX_long[is.na(mrio_fig22_20XX_long), ] <- "TOTAL"
    mrio_fig22_20XX_long <- mrio_fig22_20XX_long[, 3:8]

    if (OptAnnual == TRUE) {
      BD_FIG <- mrio_fig22_20XX_long
    } else {
      if (k == 2010) {
        BD_FIG <- mrio_fig22_20XX_long
      } else {
        BD_FIG <- rbind(BD_FIG, mrio_fig22_20XX_long)
        gc()
      }
    }

    if (OptAnnual == TRUE) {
      # Recoding
      TabPassGEO_FIG <- TabPassGEO[, c("FIGARO22_CODE_2", "ISO3_CODE_Official")]
      TabPassGEO_FIG <- setDT(TabPassGEO_FIG)
      Interm2_FIG <- na.omit(BD_FIG[TabPassGEO_FIG, on = .(Contry_Lig = FIGARO22_CODE_2)])
      Interm2_FIG <- Interm2_FIG[, Contry_Lig := NULL]
      setnames(Interm2_FIG, "ISO3_CODE_Official", "Contry_Lig")
      Interm2_FIG <- na.omit(Interm2_FIG[TabPassGEO_FIG, on = .(Contry_Col = FIGARO22_CODE_2)])
      Interm2_FIG <- Interm2_FIG[, Contry_Col := NULL]
      setnames(Interm2_FIG, "ISO3_CODE_Official", "Contry_Col")
      Interm_FIG <- Interm2_FIG

      # Change variables names
      setnames(Interm_FIG, "Contry_Lig", "Lig_Country")
      setnames(Interm_FIG, "Contry_Col", "Col_Country")
      setnames(Interm_FIG, "PR_Lig", "Lig_Indus")
      setnames(Interm_FIG, "PR_Col", "Col_Indus")
      setnames(Interm_FIG, "annee", "year")

      # Save FIGARO in RDS
      saveRDS(Interm_FIG, paste0(Path_out, "/BDn_FIG_", k, ".rds"))
      print(paste0("FIGARO database ", k, " saved"))
    }
  }

  ### Normalize

  # Recoding
  TabPassGEO_FIG <- TabPassGEO[, c("FIGARO22_CODE_2", "ISO3_CODE_Official")]
  TabPassGEO_FIG <- setDT(TabPassGEO_FIG)
  Interm2_FIG <- na.omit(BD_FIG[TabPassGEO_FIG, on = .(Contry_Lig = FIGARO22_CODE_2)])
  Interm2_FIG <- Interm2_FIG[, Contry_Lig := NULL]
  setnames(Interm2_FIG, "ISO3_CODE_Official", "Contry_Lig")
  Interm2_FIG <- na.omit(Interm2_FIG[TabPassGEO_FIG, on = .(Contry_Col = FIGARO22_CODE_2)])
  Interm2_FIG <- Interm2_FIG[, Contry_Col := NULL]
  setnames(Interm2_FIG, "ISO3_CODE_Official", "Contry_Col")
  Interm_FIG <- Interm2_FIG

  # Change variables names
  setnames(Interm_FIG, c("Contry_Lig", "Contry_Col", "PR_Lig", "PR_Col", "annee"), c("Lig_Country", "Col_Country", "Lig_Indus", "Col_Indus", "year"))

  # Save FIGARO in RDS
  if (OptAnnual == FALSE) {
    saveRDS(Interm_FIG, paste0(Path_out, "/BDn_FIG.rds"))
    print("Full FIGARO database saved")
  }
}


#' av_create_LRWIOD_2022
#' LR-WIOD 2022 release : link : https://www.rug.nl/ggdc/valuechain/long-run-wiod?lang=en
#' License and funding : Long-run WIOD is licensed under a Creative Commons Attribution 4.0 International License . The construction of the Long-run WIOD was supported by the Dutch Science Foundation (NWO) [grant number 453-14-012].
#'
#' @param Path_WIODLR path of Long-run WIOD flat CSV format files
#' @param ConvertToEuro Option to convert Dollars to Euros
#' @param Path_out path to save normalized data
#'
#' @return Nothing. Only save data into normalized format.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{ Path_WIODLR <- PathTemp
#' Path_out <- PathTest
#' av_create_LRWIOD_2022(Path_WIODLR, Path_out, ConvertToEuro = TRUE)
#' testEuroDolls <- readRDS(paste0(Path_out, "/BDn_LR_WIOD.rds"))
#' Euros <- testEuroDolls
#' av_create_LRWIOD_2022(Path_WIODLR, Path_out, ConvertToEuro = FALSE)
#' testEuroDolls <- readRDS(paste0(Path_out, "/BDn_LR_WIOD.rds"))
#' dolls <- testEuroDolls}
av_create_LRWIOD_2022 <- function(Path_WIODLR, Path_out, ConvertToEuro = TRUE) {
  # Load concordance tables
  TabPassGEO <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_GEO", col_names = TRUE)
  TabPassIND <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_PR", col_names = TRUE)


  ### Loading of full database
  mrio_lrwiod <- fread(paste0(Path_WIODLR, "lr_wiod_wiot_final_filled.csv"))

  # Convert to Euro if option activated
  if (ConvertToEuro == TRUE) {
    # Attention on est en dollars, on convertit en euros
    ConvertCurrency <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "PariteEuroDollar", col_names = TRUE)
    ConvertCurrency <- setDT(ConvertCurrency[, 1:2])
    setnames(ConvertCurrency, "Year", "year")
    mrio_lrwiod_currency <- ConvertCurrency[mrio_lrwiod, on = .(year)]
    mrio_lrwiod_currency$value <- mrio_lrwiod_currency$value / mrio_lrwiod_currency$UnEuroEgalXdollars
    mrio_lrwiod_currency <- mrio_lrwiod_currency[, UnEuroEgalXdollars := NULL]

    BD_LR_WIOD <- mrio_lrwiod_currency
  } else {
    BD_LR_WIOD <- mrio_lrwiod
  }

  # Normalization
  Interm_LRWIOD <- BD_LR_WIOD
  TabPassGEO_LRWIOD <- TabPassGEO[, c("WIOD16_CODE", "ISO3_CODE_Official")]
  TabPassGEO_LRWIOD <- setDT(TabPassGEO_LRWIOD)
  Interm2_LRWIOD <- na.omit(Interm_LRWIOD[TabPassGEO_LRWIOD, on = .(row_country = WIOD16_CODE)])
  Interm2_LRWIOD <- Interm2_LRWIOD[, row_country := NULL]
  setnames(Interm2_LRWIOD, "ISO3_CODE_Official", "LIG_Country")
  Interm2_LRWIOD <- na.omit(Interm2_LRWIOD[TabPassGEO_LRWIOD, on = .(col_country = WIOD16_CODE)])
  Interm2_LRWIOD <- Interm2_LRWIOD[, col_country := NULL]
  setnames(Interm2_LRWIOD, "ISO3_CODE_Official", "COL_Country")
  Interm_LRWIOD <- Interm2_LRWIOD

  # Change variables names
  setnames(Interm_LRWIOD, c("LIG_Country", "COL_Country", "row_isic3", "col_isic3"), c("Lig_Country", "Col_Country", "Lig_Indus", "Col_Indus"))
  Interm_LRWIOD[, c("row_id", "col_id") := NULL]

  # Save LR-WIOD in RDS
  saveRDS(Interm_LRWIOD, paste0(Path_out, "/BDn_LR_WIOD.rds"))
  print(paste0("Full database LRWIOD saved"))
}


#' av_create_WIOD_2016
#' WIOD 2016 release, RData format : link : https://www.rug.nl/ggdc/valuechain/wiod/wiod-2016-release?lang=en
#' Timmer, M. P., Dietzenbacher, E., Los, B., Stehrer, R. and de Vries, G. J. (2015), "An Illustrated User Guide to the World Input–Output Database: the Case of Global Automotive Production" , Review of International Economics., 23: 575–605
#'
#' @param Path_WIOD path of WIOD RData format files
#' @param Path_out path to save normalized data
#' @param OptAnnual option to keep one files by year instead of one file with all years. Can provide a degraded solution if a single database requires too much memory.
#' @param ConvertToEuro Option to convert Dollars to Euros
#'
#' @return Nothing. Only save data into normalized format.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{ Path_WIOD <- PathTemp
#' Path_out <- PathTest
#' av_create_WIOD_2016(Path_WIOD, Path_out, OptAnnual = TRUE, ConvertToEuro = TRUE)
#' test2015Euro <- readRDS(paste0(Path_out, "/BDn_WIOD_", 2013, ".rds"))
#' av_create_WIOD_2016(Path_WIOD, Path_out, OptAnnual = TRUE, ConvertToEuro = FALSE)
#' test2015Dolls <- readRDS(paste0(Path_out, "/BDn_WIOD_", 2013, ".rds"))
#' av_create_WIOD_2016(Path_WIOD, Path_out, OptAnnual = FALSE, ConvertToEuro = TRUE)
#' testEuro <- readRDS(paste0(Path_out, "/BDn_WIOD.rds"))
#' av_create_WIOD_2016(Path_WIOD, Path_out, OptAnnual = FALSE, ConvertToEuro = FALSE)
#' testDolls <- readRDS(paste0(Path_out, "/BDn_WIOD.rds"))}
av_create_WIOD_2016 <- function(Path_WIOD, Path_out, OptAnnual = FALSE, ConvertToEuro = TRUE) {
  # Load concordance tables
  TabPassGEO <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_GEO", col_names = TRUE)
  TabPassIND <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_PR", col_names = TRUE)

  ### iteration part and bind on other years
  for (k in 2000:2014) {
    load(paste0(Path_WIOD, paste0("WIOT", k, "_October16_ROW.RData")))
    base_wiod <- setDT(wiot)

    BD_WIODy <- melt(base_wiod,
      id.vars = c("IndustryCode", "IndustryDescription", "Country", "RNr", "Year"),
      measure.vars = 6:2690
    )
    setnames(BD_WIODy, "variable", "BR")

    # Split of BR
    BD_WIODy$COL_code <- substring(BD_WIODy$BR, 1, 3)
    BD_WIODy$COL_Indus_num <- substring(BD_WIODy$BR, 4)
    tab_corresp <- BD_WIODy[1:56, c(1, 4)]
    tab_corresp <- rbindlist(list(tab_corresp, data.table("Industrycode" = "CONS_h", "RNr" = 57), data.table("Industrycode" = "CONS_g", "RNr" = 58), data.table("Industrycode" = "CONS_np", "RNr" = 59), data.table("Industrycode" = "GFCF", "RNr" = 60), data.table("Industrycode" = "INVEN", "RNr" = 61)))
    setnames(tab_corresp, "RNr", "COL_Indus_num") # pour préparer la jointure
    tab_corresp <- tab_corresp[, COL_Indus_num := as.character(COL_Indus_num)] # Change format
    BD_WIODy <- BD_WIODy[tab_corresp, on = .(COL_Indus_num)]
    setnames(BD_WIODy, "i.IndustryCode", "COL_Indus")
    BD_WIODy[, BR := NULL]
    setnames(BD_WIODy, c("IndustryCode", "Country", "COL_code", "RNr", "IndustryDescription"), c("LIG_Indus", "LIG_Country", "COL_Country", "LIG_Indus_num", "LIG_IndustryDescription"))
    Interm_WIOD <- BD_WIODy

    if (OptAnnual == TRUE) {
      BD_WIOD <- BD_WIODy
    } else {
      if (k == 2000) {
        BD_WIOD <- BD_WIODy
      } else {
        BD_WIOD <- rbind(BD_WIOD, BD_WIODy)
        gc()
      }
    }

    if (OptAnnual == TRUE) {
      # Recoding
      Interm_WIOD <- BD_WIODy
      TabPassGEO_WIOD <- TabPassGEO[, c("WIOD16_CODE", "ISO3_CODE_Official")]
      TabPassGEO_WIOD <- setDT(TabPassGEO_WIOD)
      Interm2_WIOD <- na.omit(Interm_WIOD[TabPassGEO_WIOD, on = .(LIG_Country = WIOD16_CODE)])
      Interm2_WIOD <- Interm2_WIOD[, LIG_Country := NULL]
      setnames(Interm2_WIOD, "ISO3_CODE_Official", "LIG_Country")
      Interm2_WIOD <- na.omit(Interm2_WIOD[TabPassGEO_WIOD, on = .(COL_Country = WIOD16_CODE)])
      Interm2_WIOD <- Interm2_WIOD[, COL_Country := NULL]
      setnames(Interm2_WIOD, "ISO3_CODE_Official", "COL_Country")
      Interm_WIOD <- Interm2_WIOD

      # Convert to Euro ?
      if (ConvertToEuro == TRUE) {
        # WIOD is in dollars, we convert to euros
        ConvertCurrency <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "PariteEuroDollar", col_names = TRUE)
        ConvertCurrency <- setDT(ConvertCurrency[, 1:2])
        BD_WIOD_currency <- ConvertCurrency[Interm_WIOD, on = .(Year)]
        BD_WIOD_currency$value <- BD_WIOD_currency$value / BD_WIOD_currency$UnEuroEgalXdollars
        BD_WIOD_currency <- BD_WIOD_currency[, UnEuroEgalXdollars := NULL]
        Interm_WIOD <- BD_WIOD_currency
      }

      # Change variables names
      setnames(Interm_WIOD, c("LIG_Country", "COL_Country", "LIG_Indus", "COL_Indus", "Year"), c("Lig_Country", "Col_Country", "Lig_Indus", "Col_Indus", "year"))
      Interm_WIOD[, c("LIG_IndustryDescription", "LIG_Indus_num", "COL_Indus_num") := NULL]

      # Save WIOD in RDS
      saveRDS(Interm_WIOD, paste0(Path_out, "/BDn_WIOD_", k, ".rds"))
      print(paste0("WIOD database ", k, " saved"))
    }
  }

  ### Normalize

  # Recoding
  Interm_WIOD <- BD_WIOD
  TabPassGEO_WIOD <- TabPassGEO[, c("WIOD16_CODE", "ISO3_CODE_Official")]
  TabPassGEO_WIOD <- setDT(TabPassGEO_WIOD)
  Interm2_WIOD <- na.omit(Interm_WIOD[TabPassGEO_WIOD, on = .(LIG_Country = WIOD16_CODE)])
  Interm2_WIOD <- Interm2_WIOD[, LIG_Country := NULL]
  setnames(Interm2_WIOD, "ISO3_CODE_Official", "LIG_Country")
  Interm2_WIOD <- na.omit(Interm2_WIOD[TabPassGEO_WIOD, on = .(COL_Country = WIOD16_CODE)])
  Interm2_WIOD <- Interm2_WIOD[, COL_Country := NULL]
  setnames(Interm2_WIOD, "ISO3_CODE_Official", "COL_Country")
  Interm_WIOD <- Interm2_WIOD

  # Convert to Euro ?
  if (ConvertToEuro == TRUE) {
    # WIOD is in dollars, we convert to euros
    ConvertCurrency <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "PariteEuroDollar", col_names = TRUE)
    ConvertCurrency <- setDT(ConvertCurrency[, 1:2])
    BD_WIOD_currency <- ConvertCurrency[Interm_WIOD, on = .(Year)]
    BD_WIOD_currency$value <- BD_WIOD_currency$value / BD_WIOD_currency$UnEuroEgalXdollars
    BD_WIOD_currency <- BD_WIOD_currency[, UnEuroEgalXdollars := NULL]
    Interm_WIOD <- BD_WIOD_currency
  }

  # Change variables names
  setnames(Interm_WIOD, c("LIG_Country", "COL_Country", "LIG_Indus", "COL_Indus", "Year"), c("Lig_Country", "Col_Country", "Lig_Indus", "Col_Indus", "year"))
  Interm_WIOD[, c("LIG_IndustryDescription", "LIG_Indus_num", "COL_Indus_num") := NULL]

  # Save WIOD in RDS
  if (OptAnnual == FALSE) {
    saveRDS(Interm_WIOD, paste0(Path_out, "/BDn_WIOD.rds"))
    print("Full WIOD database saved")
  }
}

#' av_create_ICIO_2021
#' ICIO 2021 release, CSV flat format : link : https://www.oecd.org/industry/ind/inter-country-input-output-tables.htm
#' OECD (2021), OECD Inter-Country Input-Output Database, http://oe.cd/icio
#'
#' @param Path_ICIO path of ICIO flat CSV format files
#' @param Path_out path to save normalized data
#' @param OptAnnual option to keep one files by year instead of one file with all years. Can provide a degraded solution if a single database requires too much memory.
#' @param ConvertToEuro Option to convert Dollars to Euros
#'
#' @return Nothing. Only save data into normalized format.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{ Path_ICIO <- PathTemp
#' Path_out <- PathTest
#' av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = TRUE, ConvertToEuro = TRUE)
#' test1998Euro <- readRDS(paste0(Path_out, "/BDn_ICIO_", 1998, ".rds"))
#' av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = TRUE, ConvertToEuro = FALSE)
#' test1998Dolls <- readRDS(paste0(Path_out, "/BDn_ICIO_", 1998, ".rds"))
#' av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = FALSE, ConvertToEuro = TRUE)
#' testEuro <- readRDS(paste0(Path_out, "/BDn_ICIO.rds"))
#' av_create_ICIO_2021(Path_ICIO, Path_out, OptAnnual = FALSE, ConvertToEuro = FALSE)
#' testDolls <- readRDS(paste0(Path_out, "/BDn_ICIO.rds"))
#' head(testEuro)
#' head(testDolls)}
av_create_ICIO_2021 <- function(Path_ICIO, Path_out, OptAnnual = FALSE, ConvertToEuro = TRUE) {
  # Load concordance tables
  TabPassGEO <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_GEO", col_names = TRUE)
  TabPassIND <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "TabPass_PR", col_names = TRUE)

  ### Initialization

  ### Loop
  for (k in 1995:2018) {
    dataICIO <- read.csv(paste0(Path_ICIO, "ICIO2021_", k, ".csv"))
    base_ICIO <- setDT(dataICIO)
    BD_ICIO <- melt(base_ICIO,
      id.vars = c("X"),
      measure.vars = 2:3599
    )
    setnames(BD_ICIO, c("X", "variable"), c("PR", "BR"))
    BD_ICIO[, year := k]

    # Split of PR and BR
    BD_ICIO <- SplitPRBR(BD_ICIO)
    BD_ICIO[is.na(BD_ICIO), ] <- "TOTAL"

    # Geo names are already ISO3, but onlmy CHN and MEX need agregation (like CHN=CN1+CN2)
    BD_ICIO[Lig_Country == "CN1", Lig_Country := "CHN"]
    BD_ICIO[Lig_Country == "CN2", Lig_Country := "CHN"]
    BD_ICIO[Col_Country == "CN1", Col_Country := "CHN"]
    BD_ICIO[Col_Country == "CN2", Col_Country := "CHN"]

    BD_ICIO[Lig_Country == "MX1", Lig_Country := "MEX"]
    BD_ICIO[Lig_Country == "MX2", Lig_Country := "MEX"]
    BD_ICIO[Col_Country == "MX1", Col_Country := "MEX"]
    BD_ICIO[Col_Country == "MX2", Col_Country := "MEX"]

    BD_ICIO <- BD_ICIO[, sum(value), by = c("Lig_Country", "Lig_Indus", "Col_Country", "Col_Indus", "year")]
    setnames(BD_ICIO, "V1", "value")

    if (OptAnnual == FALSE) {
      if (k == 1995) {
        Base_ICIO <- BD_ICIO
      } else {
        Base_ICIO <- rbind(Base_ICIO, BD_ICIO)
        gc()
      }
    }

    if (OptAnnual == TRUE) {
      Base_ICIO <- BD_ICIO
      if (ConvertToEuro == TRUE) {
        # Convert into Euros
        ConvertCurrency <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "PariteEuroDollar", col_names = TRUE)
        ConvertCurrency <- setDT(ConvertCurrency[, 1:2])
        setnames(ConvertCurrency, "Year", "year")
        Base_ICIO_currency <- ConvertCurrency[Base_ICIO, on = .(year)]
        Base_ICIO_currency$value <- Base_ICIO_currency$value / Base_ICIO_currency$UnEuroEgalXdollars
        Base_ICIO_currency <- Base_ICIO_currency[, UnEuroEgalXdollars := NULL]
        Base_ICIO <- Base_ICIO_currency
      }
      saveRDS(Base_ICIO, paste0(Path_out, "/BDn_ICIO_", k, ".rds"))
      print(k)
    }
  }

  if (ConvertToEuro == TRUE) {
    # Convert into Euros
    ConvertCurrency <- readxl::read_excel("StructDocs et Tab_Pass_Reindus.xlsx", sheet = "PariteEuroDollar", col_names = TRUE)
    ConvertCurrency <- setDT(ConvertCurrency[, 1:2])
    setnames(ConvertCurrency, "Year", "year")
    Base_ICIO_currency <- ConvertCurrency[Base_ICIO, on = .(year)]
    Base_ICIO_currency$value <- Base_ICIO_currency$value / Base_ICIO_currency$UnEuroEgalXdollars
    Base_ICIO_currency <- Base_ICIO_currency[, UnEuroEgalXdollars := NULL]
    Base_ICIO <- Base_ICIO_currency
  }

  # Save ICIO in RDS
  if (OptAnnual == FALSE) {
    saveRDS(Base_ICIO, paste0(Path_out, "/BDn_ICIO.rds"))
    print("Full database saved")
  }
}

#' av_dl_MicroMRIO
# WARNING : dl must be normalized (not raw data) for countries homogeneity reasons.
# Warning : lists of goods and services only works with FIGARO, WIOD, LR-WIOD, ICIO rel21. Please change the lists to add other MRIO classifications.
#'
#' @param dl long data
#' @param OptXLSout to export the mini-MRIO in Excel format
#'
#' @return dl micro
#' @export
#'
#' @examples
#' \dontrun{ DT_micro <- av_dl_MicroMRIO(DT, OptXLSout = FALSE)
#' DT_microXls <- av_dl_MicroMRIO(DT, OptXLSout = TRUE)}
av_dl_MicroMRIO <- function(dl, OptXLSout = FALSE) {
  ListUE <- list("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
  ListUEUSACHN <- list("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE", "CHN", "USA") # Pour definir le ROW en complementaire
  ListBIENS <- list("3", "9", "16", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "35", "01T02", "05T06", "07T08", "10T12", "13T15", "17T18", "31T33", "36T39", "A01", "A02", "A03", "AtB", "B", "C", "C10-C12", "C10T12", "C13-C15", "C13T15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31_32", "C31_C32", "C33", "D15t16", "D17t19", "D21t22", "D23", "D24", "D25", "D26", "D27t28", "D29", "D30t33", "D34t35", "D35", "Dnec", "E", "E36", "E37-E39", "E37T39")
  ListSERV <- list("49", "50", "51", "52", "53", "61", "68", "84", "85", "41T43", "45T47", "55T56", "58T60", "62T63", "64T66", "69T75", "77T82", "86T88", "90T93", "94T96", "97T98", "D21t22", "F", "G", "G45", "G46", "G47", "H", "H49", "H50", "H51", "H52", "H53", "I", "I60t63", "I64", "J", "J58", "J59_60", "J59_J60", "J61", "J62_63", "J62_J63", "K", "K64", "K65", "K66", "L", "L68", "LtQ", "M69_70", "M69_M70", "M71", "M72", "M73", "M74_75", "M74_M75", "N", "N77", "N78", "N79", "N80T82", "O84", "P85", "Q", "Q86", "Q87_88", "R_S", "R90T92", "R93", "S94", "S95", "S96", "T", "U")

  Interm <- data.table::copy(dl)

  # Agregation of UE27 countries
  Interm <- Interm[!(Lig_Country %in% ListUEUSACHN), Lig_Country := "ROW"]
  Interm <- Interm[!(Col_Country %in% ListUEUSACHN), Col_Country := "ROW"]
  Interm <- Interm[Lig_Country %in% ListUE, Lig_Country := "UE27"]
  Interm <- Interm[Col_Country %in% ListUE, Col_Country := "UE27"]
  Interm <- Interm[Lig_Indus %in% ListBIENS, Lig_Indus := "BIENS"]
  Interm <- Interm[Lig_Indus %in% ListSERV, Lig_Indus := "SERV"]
  Interm <- Interm[Col_Indus %in% ListBIENS, Col_Indus := "BIENS"]
  Interm <- Interm[Col_Indus %in% ListSERV, Col_Indus := "SERV"]
  Interm <- Interm[, sum(value), by = c("Lig_Country", "Lig_Indus", "Col_Country", "Col_Indus", "year")]
  setnames(Interm, "V1", "value")

  if (OptXLSout == TRUE) {
    yearREF <- Interm[1, "year"]$year
    MRIO_TestMicro <- CompoMRIO(Interm, "OptFullOptionsBonus", date = yearREF, OptUE27 = TRUE)
    write.xlsx(MRIO_TestMicro[["PROD"]], "Micro_MRIO.xlsx", sheetName = "PROD", col.names = TRUE, row.names = TRUE)
    write.xlsx(MRIO_TestMicro[["CI_tab"]], "Micro_MRIO.xlsx", sheetName = "TEI", col.names = TRUE, row.names = TRUE, append = TRUE)
    write.xlsx(MRIO_TestMicro[["DF_tab"]], "Micro_MRIO.xlsx", sheetName = "DF", col.names = TRUE, row.names = TRUE, append = TRUE)
    write.xlsx(MRIO_TestMicro[["DF_TOT"]], "Micro_MRIO.xlsx", sheetName = "DF_TOT", col.names = TRUE, row.names = TRUE, append = TRUE)
    write.xlsx(MRIO_TestMicro[["VA"]], "Micro_MRIO.xlsx", sheetName = "VA", col.names = TRUE, row.names = TRUE, append = TRUE)
    write.xlsx(MRIO_TestMicro[["A_tab"]], "Micro_MRIO.xlsx", sheetName = "A", col.names = TRUE, row.names = TRUE, append = TRUE)
    write.xlsx(MRIO_TestMicro[["L"]], "Micro_MRIO.xlsx", sheetName = "L", col.names = TRUE, row.names = TRUE, append = TRUE)
    write.xlsx(MRIO_TestMicro[["Imports"]], "Micro_MRIO.xlsx", sheetName = "Imports", col.names = TRUE, row.names = TRUE, append = TRUE)
    write.xlsx(MRIO_TestMicro[["Exports"]], "Micro_MRIO.xlsx", sheetName = "Exports", col.names = TRUE, row.names = TRUE, append = TRUE)
    print("Micro_MRIO.xlsx saved with components")
  }

  return(Interm)
}

#' av_dl_UE27
#' Build a new MRIO with all EU countries aggragated into "UE27" item.
#' Useful to calculate EU made-in for instance.
#' WARNING : dl must be normalized (not raw data) for countries homogeneity reasons.
#'
#' @param dl long data
#' @param OptSaveRDS text
#'
#' @return dl with EU27 agreggate instead of each country
#' @export
#'
#' @examples
#' \dontrun{ DT_UE27 <- av_dl_UE27(DT)}
av_dl_UE27 <- function(dl, OptSaveRDS = "NO") {
  ListUE <- list("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")

  Interm <- data.table::copy(dl)

  # Agregation of UE27 countries
  Interm <- Interm[Lig_Country %in% ListUE, Lig_Country := "UE27"]
  Interm <- Interm[Col_Country %in% ListUE, Col_Country := "UE27"]
  Interm <- Interm[, sum(value), by = c("Lig_Country", "Lig_Indus", "Col_Country", "Col_Indus", "year")] # Agregation
  setnames(Interm, "V1", "value")

  if (OptSaveRDS != "NO") {
    saveRDS(Interm, paste0(OptSaveRDS, ".rds"))
    print(paste0(OptSaveRDS, "saved"))
  }

  return(Interm)
}
