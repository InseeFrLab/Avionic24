#' @import Rcpp RcppArmadillo
#' @importFrom stats na.omit sd
#' @importFrom utils capture.output head read.csv
#' @useDynLib Avionic24

NULL

utils::globalVariables(c(
".", "..Ordre_Col", "..TypAgreg", "BASE_FIG2019", "BR", "C19", "C27", "C28", "C29", "COL_Country",
"COL_Indus_num", "COU", "Col_Country", "Col_Indus", "Compo", "ContenuDuPays",
"Contry_Col", "Contry_Lig", "CountryREF", "D21t22", "D35", "DF_seclect", "DT", "Diff",
"EstimeEmploi", "FIGARO22_CODE_2", "Figaro64ind", "ICIO21OECD45_CODE", "IND",
"INDUSTRY_AGG", "IndicH", "LIG_Country", "LR_WIOD_Rev1", "Lig_Country", "Lig_Indus",
"LtQ", "MRIO", "MRIOCountry", "Manuf", "ManufFabC3C5", "NEW_numerateur", "OBS_VALUE", "PAR",
"PR", "PathTemp", "PathTest", "Pathvalue", "RatioMadeIn", "RecupVectEmiFigaro",
"ServMarch", "ServNonMarch", "Services", "TIME", "TypMRIO", "TypeManuf", "TypeRatio",
"TypeRes", "UnEuroEgalXdollars", "V1", "Variant", "WIOD16_CODE", "WIOD56rel2016_CODE",
"Year", "a_ij", "a_ij_deb", "a_jk", "a_jk_fin", "a_kl", "a_lm", "a_mn", "a_no", "a_op", "ay", "base",
"col_country", "deb", "denominateur", "diffRanking", "diffValue", "dispers",
"dispersRelative", "f_dt", "f_i", "f_i_deb", "fa", "fin", "force_faa", "force_faaa",
"force_faaaa", "force_faaaaa", "force_faaaaaa", "force_faaaaaaay", "force_faaaaaay",
"force_faaaaay", "force_faaaay", "force_faaay", "force_faay", "force_fay", "full",
"fullAdj", "geo", "i.Pathvalue", "i.V1", "i.ranking", "i.value", "induse", "industry",
"na_item", "nace_r2", "name_i", "name_j", "name_k", "name_l", "name_m", "name_n", "name_o",
"name_p", "numerateur", "obs_value", "path", "position", "prod_na", "ranking",
"ratioNoguera", "ref_area", "row_country", "separate", "stk_flow", "type_correc", "value",
"valuePct", "valueSquared", "valueSquaredAdj", "value_delta", "variable", "vect", "wiot",
"y_i", "y_j", "y_k", "y_k_fin", "y_l", "y_m", "y_n", "y_o", "y_p", "yearREF", "ï..VAR"
))
