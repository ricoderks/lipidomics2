#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @noRd
app_server <- function(input, output, session) {
  # increase upload limit
  options(shiny.maxRequestSize = 30 * 1024^2)

  # for communication between modules
  r <- shiny::reactiveValues(
    name = NULL,
    omics = NULL,
    defaults = list(
      lipidclass_ion = c("ADGGA - [M-H]-", "AHexBRS - [M+HCOO]-", "AHexCAS - [M+HCOO]-", "AHexCS - [M+HCOO]-", "AHexSIS - [M+HCOO]-", "ASM - [M+H]+", "BASulfate - [M-H]-",
                         "BileAcid - [M-H]-", "BMP - [M+NH4]+", "CAR - [M+H]+", "CE - [M+NH4]+", "Cer_ADS - [M+HCOO]-", "Cer_AP - [M+HCOO]-",
                         "Cer_AS - [M+HCOO]-", "Cer_BS - [M+HCOO]-", "Cer_HS - [M+H]+", "Cer_NDS - [M+HCOO]-",
                         "Cer_NP - [M+HCOO]-", "Cer_NS - [M+HCOO]-", "CerP - [M+H]+",
                         "CL - [M+NH4]+", "CoQ - [M+H]+", "DCAE - [M+NH4]+", "DG - [M+NH4]+", "DGGA - [M-H]-", "EtherDG - [M+NH4]+",
                         "EtherLPC - [M+HCOO]-", "EtherLPE - [M-H]-", "EtherMGDG - [M+NH4]+", "EtherPC - [M+HCOO]-",
                         "EtherPE - [M-H]-", "EtherPG - [M-H]-", "EtherPI - [M-H]-", "EtherTG - [M+NH4]+", "FA - [M-H]-", "FAHFA - [M-H]-",
                         "HBMP - [M-H]-", "Hex2Cer - [M+HCOO]-", "HexCer_EOS - [M-H]-", "HexCer_HS - [M+HCOO]-", "HexCer_NS - [M+HCOO]-",
                         "LPA - [M-H]-", "LPC - [M+HCOO]-", "LPE - [M-H]-", "LPI - [M-H]-",
                         "LPS - [M-H]-", "MG - [M+NH4]+", "MGDG - [M+HCOO]-", "MLCL - [M-H]-", "NAE - [M+H]+", "NAGly - [M+H]+", "NAGlySer - [M-H]-",
                         "NAOrn - [M+H]+", "OxFA - [M-H]-", "OxPC - [M+HCOO]-", "OxPE - [M-H]-", "OxPG - [M-H]-", "OxPI - [M-H]-", "OxTG - [M+NH4]+",
                         "PA - [M-H]-", "PC - [M+HCOO]-", "PE - [M-H]-", "PE_Cer - [M-H]-", "PEtOH - [M-H]-",
                         "PG - [M-H]-", "PI - [M-H]-",  "PI_Cer - [M+H]+", "PMeOH - [M-H]-",
                         "PS - [M-H]-", "SHexCer - [M-H]-", "SL - [M-H]-", "SM - [M+H]+", "Sph - [M+H]+",
                         "SQDG - [M-H]-", "SSulfate - [M-H]-", "ST - [M+H-H2O]+", "ST - [M+H]+", "TG - [M+NH4]+", "TG_EST - [M+NH4]+", "VAE - [M+H]+"),
      metclass_ion = NULL,
      rsd_cutoff = 0.3,
      dot_cutoff = 50,
      revdot_cutoff = 50,
      blanksample_ratio = 5,
      blanksample_threshold = 0.8
    ),
    settings = list(
      rsd_cutoff = NULL,
      dot_cutoff = NULL,
      revdot_cutoff = NULL,
      blanksample_ratio = NULL,
      blanksample_threshold = NULL,
      feature_class = NULL,
      selected_feature_class = NULL
    ),
    files = list(
      meta_file = NULL,
      data_file_pos = NULL,
      data_file_neg = NULL,
      rda_file = NULL
    ),
    columns = list(
      sampleid = NULL,
      sampletype = NULL,
      acqorder = NULL
    ),
    index = list(
      blanks = NULL,
      pools = NULL,
      samples = NULL,
      selected_blanks = NULL,
      selected_pools = NULL,
      selected_samples = NULL,
      keep_rsd = NULL,
      keep_blankratio = NULL,
      keep_id = NULL
    ),
    tables = list(
      meta_data = NULL,
      raw_data_pos = NULL,
      raw_data_neg = NULL,
      raw_data = NULL,
      clean_data = NULL,
      clean_data_wide = NULL,
      blank_data = NULL,
      qc_data = NULL,
      analysis_data = NULL
    )
  )

  mod_file_server(id = "file",
                  r = r)

  mod_settings_server(id = "settings",
                      r = r)

  #--------------------------------------------------------------------- help ----
  mod_help_server(id = "help")

  mod_about_server(id = "about")

}
