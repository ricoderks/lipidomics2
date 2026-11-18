cols_common <- c("Alignment ID", "Average Rt(min)", "Average Mz", "Metabolite name",
                 "Adduct type", "Ontology", "Reverse dot product", "Total score")

test_that("MS-DIAL export format is detected", {
  expect_equal(detect_msdial_format(c(cols_common, "Dot product", "Fragment presence %")),
               "v4")
  expect_equal(detect_msdial_format(c(cols_common, "Simple dot product", "Matched peaks percentage")),
               "v5")
  expect_equal(detect_msdial_format(c(cols_common, "Simple dot product", "Matched peaks percentage",
                                      "Measured terms", "Evidence source")),
               "v5_evidence")
  # spaces around the column names are ignored
  expect_equal(detect_msdial_format(c(cols_common, " Evidence source ")),
               "v5_evidence")
})

test_that("MS-DIAL format label always returns a label", {
  expect_match(msdial_format_label("v5_evidence"), "5.5.261105")
  expect_type(msdial_format_label(NULL), "character")
})

make_wide <- function(names, evidence) {
  n <- length(names)
  data.frame(
    my_id = paste0("pos_", seq_len(n)),
    AlignmentID = seq_len(n),
    AverageRT = seq_len(n),
    AverageMZ = 700 + seq_len(n),
    ion = factor(rep("[M+H]+", n)),
    FeatureName = names,
    Class = rep("PC", n),
    polarity = factor(rep("pos", n)),
    EvidenceSource = evidence,
    Sample_1 = seq_len(n)
  )
}

test_that("select_identified uses the name prefix for older MS-DIAL versions", {
  data <- make_wide(
    names = c("PC 34:1", "low score: PC 34:2", "no MS2: PC 36:1", "Unknown",
              "RIKEN ID-1", "w/o MS2: PC 36:2", "PC 36:4"),
    evidence = NA_character_
  )

  res <- select_identified(data = data, omics = "lip")

  expect_equal(sort(res$FeatureName), c("PC 34:1", "PC 36:4"))
})

test_that("select_identified uses the evidence source for MS-DIAL >= 5.5.261105", {
  data <- make_wide(
    names = c("PC 34:1", "PC 34:2", "PC 36:1", "PC 36:2", "Unknown", "RIKEN ID-1", "PC 38:4"),
    evidence = c("RuleBased", "UnmatchedSpectrum", "WeakSpectrumMatch", "PrecursorOnly",
                 NA, "RuleBased", NA)
  )

  res <- select_identified(data = data, omics = "lip")

  expect_equal(res$FeatureName, "PC 34:1")
  expect_true(all(res$EvidenceSource == "RuleBased"))
})

test_that("select_identified calculates the short lipid name for MS-DIAL >= 5.5.261105", {
  data <- make_wide(
    names = c("PC 16:0_18:1", "LPC 16:1", "PC 18:1_16:0;O"),
    evidence = c("RuleBased", "RuleBased", "WeakSpectrumMatch")
  )

  res <- select_identified(data = data, omics = "lip")

  expect_equal(res$LongLipidName, c("LPC 16:1", "PC 16:0_18:1"))
  expect_equal(res$ShortLipidName, c("LPC 16:1", "PC 34:1"))
  expect_equal(res$orgShortLipidName, res$ShortLipidName)
})

test_that("select_identified splits 'short|long' for older MS-DIAL versions", {
  data <- make_wide(
    names = c("PC 34:1|PC 16:0_18:1", "LPC 16:1"),
    evidence = NA_character_
  )

  res <- select_identified(data = data, omics = "lip")

  expect_equal(res$ShortLipidName, c("LPC 16:1", "PC 34:1"))
  expect_equal(res$LongLipidName, c("LPC 16:1", "PC 16:0_18:1"))
})

test_that("short lipid name is calculated from the long lipid name", {
  names <- c(
    # sum of chains
    "PC 16:0_18:1" = "PC 34:1",
    "TG 8:0_8:0_10:0" = "TG 26:0",
    "CL 32:2_34:3" = "CL 66:5",
    "PE-Cer 16:1;O2/18:1" = "PE-Cer 34:2;O2",
    # ether lipids
    "PE P-16:0_18:1" = "PE P-34:1",
    "TG O-16:0_16:0_18:2" = "TG O-50:2",
    # oxygens
    "PE 18:1_16:0;O2" = "PE 34:1;O2",
    "HexCer 18:1;O2/16:0;O" = "HexCer 34:1;O3",
    "Cer 18:1;O2/26:0(2OH)" = "Cer 44:1;O3",
    "Cer 12:0;O2/26:1(3OH)" = "Cer 38:1;O3",
    # ester linked fatty acid after a chain
    "TG 16:0_18:1_16:1;O(FA 16:1)" = "TG 66:4;O2",
    "Cer 26:3;O3(FA 18:0)" = "Cer 44:4;O4",
    "Cer 15:0;O2/16:2(3OH)(FA 16:0)" = "Cer 47:3;O4",
    "SM 24:2;O2(FA 12:0)" = "SM 36:3;O3",
    "NAGly 8:0;O(FA 20:0)" = "NAGly 28:1;O2",
    # leading fatty acid / O-acyl only adds the chain
    "LPE-N (FA 18:0)22:6" = "LPE-N 40:6",
    "AHexCer (O-12:0)12:2;O2/13:1;O" = "AHexCer 37:3;O3",
    "ADGGA (O-22:6)16:0_22:4" = "ADGGA 60:10",
    # FAHFA
    "AAHFA 6:0/15:2;O" = "AAHFA 21:3;O2",
    # isotope label
    "DG 15:0_18:1(d7)" = "DG 33:1(d7)",
    # not changed
    "LPC 16:1" = "LPC 16:1",
    "PC 26:1" = "PC 26:1",
    "CoQ10" = "CoQ10",
    "ST 27:1;O;S" = "ST 27:1;O;S",
    "SE 28:1/18:2" = "SE 28:1/18:2",
    "BA 24:1;O4;G/11:0" = "BA 24:1;O4;G/11:0"
  )

  expect_equal(long_to_short_name(x = names(names)), unname(names))
  expect_equal(long_to_short_name(x = NA_character_), NA_character_)
  expect_equal(long_to_short_name(x = character(0)), character(0))
})
