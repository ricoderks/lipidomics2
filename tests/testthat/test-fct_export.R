test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("sanitize_markdown_html keeps markdown formatting", {
  html <- sanitize_markdown_html("Some **bold** text.\n\n## Sub\n\n- a\n- b\n\n| a | b |\n|---|---|\n| 1 | 2 |\n\n> quote")
  expect_match(html, "<strong>bold</strong>", fixed = TRUE)
  expect_match(html, "<h2>Sub</h2>", fixed = TRUE)
  expect_match(html, "<li>a</li>", fixed = TRUE)
  expect_match(html, "<table>", fixed = TRUE)
  expect_match(html, "<blockquote>", fixed = TRUE)
})

test_that("sanitize_markdown_html escapes raw html", {
  html <- sanitize_markdown_html("<script>alert(1)</script>\n\n<img src=x onerror=alert(1)>\n\n```{=html}\n<script>alert(2)</script>\n```")
  expect_false(grepl("<script", html, fixed = TRUE))
  expect_false(grepl("<img", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
})

test_that("sanitize_markdown_html only allows safe urls", {
  html <- sanitize_markdown_html("[a](javascript:alert(1)) [b](JaVaScRiPt:alert(1)) [c](data:text/html,x) [d](javascript&#58;alert(1)) ![e](javascript:alert(1))")
  expect_false(grepl("javascript:", html, ignore.case = TRUE))
  expect_false(grepl("data:", html, fixed = TRUE))
  expect_match(html, '<img src="#"', fixed = TRUE)

  html <- sanitize_markdown_html("[a](https://www.lumc.nl) [b](mailto:a@b.nl) [c](#section)")
  expect_match(html, 'href="https://www.lumc.nl"', fixed = TRUE)
  expect_match(html, 'href="mailto:a@b.nl"', fixed = TRUE)
  expect_match(html, 'href="#section"', fixed = TRUE)
})

test_that("escape_markdown_text escapes all punctuation", {
  expect_equal(escape_markdown_text("Intro {onclick=alert(1)}"),
               "Intro &#123;onclick&#61;alert&#40;1&#41;&#125;")
  expect_equal(escape_markdown_text("<b>x</b>"),
               "&#60;b&#62;x&#60;&#47;b&#62;")
  expect_equal(escape_markdown_text("a\nb"), "a b")
  expect_equal(escape_markdown_text("Résumé"), "Résumé")
  expect_equal(escape_markdown_text(""), "")
})

test_that("sanitize_report_title removes unsafe characters", {
  expect_equal(sanitize_report_title("My report <script>\"{`"), "My report script")
  expect_equal(sanitize_report_title("Lipidomics (n=12): mice, 2026"),
               "Lipidomics (n12): mice, 2026")
  expect_equal(sanitize_report_title(NULL), "")
})

test_that("prepare_report_chapters drops empty chapters", {
  chapters <- list(
    list(title = "Background", text = "Some <b>text</b>"),
    list(title = " ", text = "  "),
    list(title = "", text = "No title")
  )
  res <- prepare_report_chapters(chapters)
  expect_length(res, 2)
  expect_equal(res[[1]]$title, "Background")
  expect_equal(res[[1]]$html, "<p>Some &lt;b&gt;text&lt;/b&gt;</p>\n")
  expect_equal(res[[2]]$title, "")
  expect_equal(prepare_report_chapters(list(list(title = "Results (part 1)", text = "")))[[1]]$title,
               "Results &#40;part 1&#41;")
  expect_length(prepare_report_chapters(NULL), 0)
})
