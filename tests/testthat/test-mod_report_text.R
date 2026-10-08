test_that("report chapters are added, updated and removed", {
  r <- shiny::reactiveValues(analysis = list(report_chapters = NULL))

  shiny::testServer(
    mod_report_text_server,
    args = list(r = r),
    {
      session$setInputs(addChapter = 1)
      session$setInputs(chapter_1_title = "Introduction",
                        chapter_1_text = "Some **markdown** text.")
      expect_length(r$analysis$report_chapters, 1)
      expect_equal(r$analysis$report_chapters[[1]]$title, "Introduction")
      expect_equal(r$analysis$report_chapters[[1]]$text, "Some **markdown** text.")

      session$setInputs(addChapter = 2)
      session$setInputs(chapter_2_title = "Conclusion")
      expect_length(r$analysis$report_chapters, 2)
      expect_equal(r$analysis$report_chapters[[2]]$title, "Conclusion")
      expect_equal(r$analysis$report_chapters[[2]]$text, "")

      session$setInputs(chapter_1_remove = 1)
      expect_length(r$analysis$report_chapters, 1)
      expect_equal(r$analysis$report_chapters[[1]]$title, "Conclusion")
    }
  )
})

test_that("module ui works", {
  ui <- mod_report_text_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_report_text_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
