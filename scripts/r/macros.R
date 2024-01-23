################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] macros.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 2 June 2023
##
################################################################################

## state abbreviations
stabbr <- crosswalkr::stcrosswalk |> pull(stabbr)

## state names
stname <- crosswalkr::stcrosswalk |> pull(stname)

## parameter labels
add_param_labels <- function(x) {
    case_when(
      x == "b_fe" ~ "Female",
      x == "b_lo" ~ "Low income",
      x == "a_ra[1]" ~ "Amer.Ind.",
      x == "a_ra[2]" ~ "Asian/PI",
      x == "a_ra[3]" ~ "Black",
      x == "a_ra[4]" ~ "Hispanic",
      x == "a_ra[5]" ~ "Multiracial",
      x == "a_ra[6]" ~ "White",
      x == "a" ~ "Constant",
      x == "a_rg[1]" ~ "Northeast",
      x == "a_rg[2]" ~ "Midwest",
      x == "a_rg[3]" ~ "South",
      x == "a_rg[4]" ~ "West",
      x == "a_st[1]" ~ "AL",
      x == "a_st[2]" ~ "AK",
      x == "a_st[3]" ~ "AZ",
      x == "a_st[4]" ~ "AR",
      x == "a_st[5]" ~ "CA",
      x == "a_st[6]" ~ "CO",
      x == "a_st[7]" ~ "CT",
      x == "a_st[8]" ~ "DE",
      x == "a_st[9]" ~ "FL",
      x == "a_st[10]" ~ "GA",
      x == "a_st[11]" ~ "HI",
      x == "a_st[12]" ~ "ID",
      x == "a_st[13]" ~ "IL",
      x == "a_st[14]" ~ "IN",
      x == "a_st[15]" ~ "IA",
      x == "a_st[16]" ~ "KS",
      x == "a_st[17]" ~ "KY",
      x == "a_st[18]" ~ "LA",
      x == "a_st[19]" ~ "ME",
      x == "a_st[20]" ~ "MD",
      x == "a_st[21]" ~ "MA",
      x == "a_st[22]" ~ "MI",
      x == "a_st[23]" ~ "MN",
      x == "a_st[24]" ~ "MS",
      x == "a_st[25]" ~ "MO",
      x == "a_st[26]" ~ "MT",
      x == "a_st[27]" ~ "NE",
      x == "a_st[28]" ~ "NV",
      x == "a_st[29]" ~ "NH",
      x == "a_st[30]" ~ "NJ",
      x == "a_st[31]" ~ "NM",
      x == "a_st[32]" ~ "NY",
      x == "a_st[33]" ~ "NC",
      x == "a_st[34]" ~ "ND",
      x == "a_st[35]" ~ "OH",
      x == "a_st[36]" ~ "OK",
      x == "a_st[37]" ~ "OR",
      x == "a_st[38]" ~ "PA",
      x == "a_st[39]" ~ "RI",
      x == "a_st[40]" ~ "SC",
      x == "a_st[41]" ~ "SD",
      x == "a_st[42]" ~ "TN",
      x == "a_st[43]" ~ "TX",
      x == "a_st[44]" ~ "UT",
      x == "a_st[45]" ~ "VT",
      x == "a_st[46]" ~ "VA",
      x == "a_st[47]" ~ "WA",
      x == "a_st[48]" ~ "WV",
      x == "a_st[49]" ~ "WI",
      x == "a_st[50]" ~ "WY",
      x == "a_st_lo[1]" ~ "AL_lo",
      x == "a_st_lo[2]" ~ "AK_lo",
      x == "a_st_lo[3]" ~ "AZ_lo",
      x == "a_st_lo[4]" ~ "AR_lo",
      x == "a_st_lo[5]" ~ "CA_lo",
      x == "a_st_lo[6]" ~ "CO_lo",
      x == "a_st_lo[7]" ~ "CT_lo",
      x == "a_st_lo[8]" ~ "DE_lo",
      x == "a_st_lo[9]" ~ "FL_lo",
      x == "a_st_lo[10]" ~ "GA_lo",
      x == "a_st_lo[11]" ~ "HI_lo",
      x == "a_st_lo[12]" ~ "ID_lo",
      x == "a_st_lo[13]" ~ "IL_lo",
      x == "a_st_lo[14]" ~ "IN_lo",
      x == "a_st_lo[15]" ~ "IA_lo",
      x == "a_st_lo[16]" ~ "KS_lo",
      x == "a_st_lo[17]" ~ "KY_lo",
      x == "a_st_lo[18]" ~ "LA_lo",
      x == "a_st_lo[19]" ~ "ME_lo",
      x == "a_st_lo[20]" ~ "MD_lo",
      x == "a_st_lo[21]" ~ "MA_lo",
      x == "a_st_lo[22]" ~ "MI_lo",
      x == "a_st_lo[23]" ~ "MN_lo",
      x == "a_st_lo[24]" ~ "MS_lo",
      x == "a_st_lo[25]" ~ "MO_lo",
      x == "a_st_lo[26]" ~ "MT_lo",
      x == "a_st_lo[27]" ~ "NE_lo",
      x == "a_st_lo[28]" ~ "NV_lo",
      x == "a_st_lo[29]" ~ "NH_lo",
      x == "a_st_lo[30]" ~ "NJ_lo",
      x == "a_st_lo[31]" ~ "NM_lo",
      x == "a_st_lo[32]" ~ "NY_lo",
      x == "a_st_lo[33]" ~ "NC_lo",
      x == "a_st_lo[34]" ~ "ND_lo",
      x == "a_st_lo[35]" ~ "OH_lo",
      x == "a_st_lo[36]" ~ "OK_lo",
      x == "a_st_lo[37]" ~ "OR_lo",
      x == "a_st_lo[38]" ~ "PA_lo",
      x == "a_st_lo[39]" ~ "RI_lo",
      x == "a_st_lo[40]" ~ "SC_lo",
      x == "a_st_lo[41]" ~ "SD_lo",
      x == "a_st_lo[42]" ~ "TN_lo",
      x == "a_st_lo[43]" ~ "TX_lo",
      x == "a_st_lo[44]" ~ "UT_lo",
      x == "a_st_lo[45]" ~ "VT_lo",
      x == "a_st_lo[46]" ~ "VA_lo",
      x == "a_st_lo[47]" ~ "WA_lo",
      x == "a_st_lo[48]" ~ "WV_lo",
      x == "a_st_lo[49]" ~ "WI_lo",
      x == "a_st_lo[50]" ~ "WY_lo",
      x == "g[1]" ~ "Bachelor's (%)",
      x == "g[2]" ~ "Two-year (%)",
      x == "g[3]" ~ "Four-year tuition",
      x == "g[4]" ~ "Unemployment (%)",
      x == "g[5]" ~ "Two-year distance"
    )
}

fl_params_to_factor <- function(x, table = FALSE) {
  x <- factor(x,
              levels = c("Low income",
                         "Female",
                         "Amer.Ind.",
                         "Asian/PI",
                         "Black",
                         "Hispanic",
                         "Multiracial",
                         "White")
              )
  if (table) {
    x <- factor(x,
                levels = c("Low income",
                           "Female",
                           "Amer.Ind.",
                           "Asian/PI",
                           "Black",
                           "Hispanic",
                           "Multiracial",
                           "White"),
                labels = c("Low income",
                           "Female",
                           "American Indian",
                           "Asian / Pacific Islander",
                           "Black",
                           "Hispanic",
                           "Multiracial",
                           "White")
                )
  }
  return(x)
}

st_params_to_factor <- function(x) {
  factor(x,
         levels = c("Constant",
                    "Northeast",
                    "Midwest",
                    "South",
                    "West",
                    crosswalkr::stcrosswalk |> pull(stabbr),
                    crosswalkr::stcrosswalk |>
                      pull(stabbr) |>
                      (\(x) {paste0(x, "_lo")})()
                    )
         )
}

sl_params_to_factor <- function(x, table = FALSE) {
  x <- factor(x,
              levels = c("Bachelor's (%)",
                         "Two-year (%)",
                         "Four-year tuition",
                         "Unemployment (%)",
                         "Two-year distance")
              )
  if (table) {
    x <- gsub("%", "\\\\%", x)
  }
  return(x)
}

## add square brackets
add_sqbr <- function(x) { gsub("(.+)", "[\\1]", x) }

## add parentheses
add_paren <- function(x) { gsub("(.+)", "(\\1)", x) }

## add italics
add_italics <- function(x) { gsub("(.+)", "{\\\\itshape \\1}", x) }

## add bold
add_bold <- function(x) { gsub("(.*)", "{\\\\bfseries \\1}", x) }

## add hspace
add_hspace <- function(x, em = 1) {
  gsub("(.*)", paste0("\\\\hspace{", em, "em}\\1"), x)
}


## =============================================================================
## END SCRIPT
################################################################################
