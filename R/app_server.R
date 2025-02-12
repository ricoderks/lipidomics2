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
      lipid_classes = list(
        "Fatty acyls" = list(
          "FA" = list(id = "FA",
                      pattern = "^(Ox)?FA$",
                      name = "Fatty acids and conjugates"),
          "FAM" = list(id = "FAM",
                       pattern = "^(NAGly|NAGlySer|NAOrn|NAE)",
                       name = "Fatty amides"),
          "FE" = list(id = "FE",
                      pattern = "^(CAR|FAHFA)",
                      name = "Fatty esters")
        ),
        "Glycerolipids" = list(
          "EGL" = list(id = "EGL",
                       pattern = "^(Ether|Ox)[MDT]G$",
                       name = "Ether/Oxidized glycerolipids"),
          "GL" = list(id = "GL",
                      pattern = "^[MDT]G$",
                      name = "Glycerolipids"),
          "GLDG" = list(id = "GLDL",
                        pattern = "^(Ether|EtherS)?[DMS][GQ]DG$",
                        name = "Glycosyldiradylglycerols"),
          "OGL" = list(id = "OGL",
                       pattern = "^([AL]?DG(GA|CC|TS/A)|TG_EST)$",
                       name = "Other glycerolipids")
        ),
        "Glycerophospholipids" = list(
          "PA" = list(id = "PA",
                      pattern = "^L?PA$",
                      name = "Glycerophosphates (PA)"),
          "PC" = list(id = "PC",
                      pattern = "^(Ether)?L?PC$",
                      name = "Glycerophosphocholines (PC)"),
          "PE" = list(id = "PE",
                      pattern = "^(LNA)?(Ether)?L?PE(\\(P\\))?$",
                      name = "Glycerophosphoethanolamines (PE)"),
          "PG" = list(id = "PG",
                      pattern = "^(H?BMP|(Ether)?L?PG)$",
                      name = "Glycerophosphoglycerols (PG)"),
          "CL" = list(id = "CL",
                      pattern = "^([DM]L)?CL$",
                      name = "Cardiolipins (CL)"),
          "AcPIM" = list(id = "AcPIM",
                         pattern = "^Ac[2-4]PIM[12]$",
                         name = "Glycerophosphoinositolglycans"),
          "PI" = list(id = "PI",
                      pattern = "^(Ether)?L?PI$",
                      name = "Glycerophosphoinositols (PI)"),
          "PS" = list(id = "PS",
                      pattern = "^(LNA)?(Ether)?L?PS$",
                      name = "Glycerophosphoserines (PS)"),
          "OPL" = list(id = "OPL",
                       pattern = "^OxP[ACEGIS]$",
                       name = "Oxidized glycerophospholipids"),
          "OGPL" = list(id = "OGPL",
                        pattern = "^P(Et|Me)OH$",
                        name = "Other glycerophospholipids")
        ),
        "Prenol lipids" = list(
          "PRL" = list(id = "PRL",
                       pattern = "^(VAE|CoQ|VitaminE)$",
                       name = "Prenol lipids")
        ),
        "Sphingolipids" = list(
          "AcGL" = list(id = "AcGL",
                        pattern = "^(GM3|SHexCer(\\+O)?)$",
                        name = "Acidic glycosphingolipids"),
          "Cer" = list(id = "Cer",
                       pattern = "^Cer[P_]",
                       name = "Ceramides"),
          "PSL" = list(id = "PSL",
                       pattern = "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)",
                       name = "Phosphosphingolipids"),
          "NPSL" = list(id = "NPSL",
                        pattern = "^A?Hex[23]?Cer",
                        name = "Neutral glycosphingolipids"),
          "SB" = list(id = "SB",
                      pattern = "^((Phyto|DH)?Sph|SL(\\+O)?)$",
                      name = "Sphingoid bases")
        ),
        "Sterol lipids" = list(
          "BA" = list(id = "BA",
                      pattern = "^(BASulfate|BileAcid|DCAE)$",
                      name = "Bile acids and conjugates"),
          "SC" = list(id = "SC",
                      pattern = "^VitaminD$",
                      name = "Secosteroids"),
          "STC" = list(id = "STC",
                       pattern = "^SSulfate$",
                       name = "Steroid conjugates"),
          "ST" = list(id = "ST",
                      pattern = "^((BR|CA|SI|ST)?[CS]E|Cholesterol|SHex)$",
                      name = "Sterols"),
          "OST" = list(id = "OST",
                       pattern = "^AHex(CAS|CS|SIS|BRS|STS)$",
                       name = "Other sterol lipids")
        )
      ),
      patterns = list(
        PL = "^((Ether)?(Ox)?(L)?(LNA)?(MM)?P[ACEGISM]|HBMP|BMP)",
        GL = "^(Ox|Ether|SQ|EtherS|L|A)?[DMT]G",
        Cer = "^Cer[P_]",
        HexCer = "^A?Hex[23]?Cer",
        FA = "^((Ox)?FA|FAHFA|NAGly|NAGlySer|NAOrn|NAE|CAR)",
        PSL = "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)",
        SB = "^(PhytoSph|SL|SL\\+O|DHSph|Sph)",
        SA = "^(GM3|SHexCer|SHexCer\\+O)",
        CL = "^([DM]L)?CL",
        ACPIM = "^Ac[2-4]PIM[12]",
        STL = "^((BA|S)Sulfate|BileAcid|AHex[BCS][AIRTS][S]?|(BRS|CAS|C|SIS|STS|DCA|TDCA)E|SHex|Cholesterol|VitaminD|ST) ",
        PRL = "^(VAE|CoQ|VitaminE)"
      )
    ),
    settings = list(
      rsd_cutoff = 0.3,
      dot_cutoff = 50,
      revdot_cutoff = 50,
      blanksample_ratio = 5,
      blanksample_threshold = 0.8,
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
      filename = NULL,
      sampleid = NULL,
      sampletype = NULL,
      acqorder = NULL,
      batch = NULL,
      groups = NULL
    ),
    text_patterns = list(
      blanks = NULL,
      pools = NULL,
      samples = NULL
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

  mod_qc_server(id = "qc",
                r = r)

  mod_identification_server(id = "id",
                            r = r)

  mod_issues_server(id = "issues",
                    r = r)

  mod_export_server(id = "export",
                    r = r)

  #--------------------------------------------------------------------- help ----
  mod_help_server(id = "help")

  mod_about_server(id = "about")

}
