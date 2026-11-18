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
