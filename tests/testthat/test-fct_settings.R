test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("qc_rlsc uses the span parameter", {
  set.seed(1)
  n <- 30
  tab <- data.frame(a = 100 + 1:n + stats::rnorm(n, sd = 5))
  colv <- rep(c(1, 2, 2), length.out = n)

  res_default <- qc_rlsc(tab = tab, colv = colv, or = 1:n)
  res_span <- qc_rlsc(tab = tab, colv = colv, or = 1:n, span = 0.5)

  expect_equal(res_default,
               qc_rlsc(tab = tab, colv = colv, or = 1:n, span = 0.75))
  expect_false(isTRUE(all.equal(res_default, res_span)))
})

test_that("do_trend_correction keeps the original areas when rerun", {
  set.seed(1)
  samples <- sprintf("s%02d", 1:30)
  pools <- samples[seq(1, 30, by = 3)]
  data <- expand.grid(sample_name = samples,
                      my_id = c("id1", "id2"),
                      stringsAsFactors = FALSE)
  data$order <- match(data$sample_name, samples)
  data$batch <- 1
  data$area <- 100 + data$order + stats::rnorm(nrow(data), sd = 5)

  columns <- list(acqorder = "order", batch = "batch")
  index <- list(selected_pools = pools,
                selected_samples = setdiff(samples, pools))

  res1 <- do_trend_correction(data = data, method = "loess",
                              columns = columns, index = index, span = 0.75)
  res2 <- do_trend_correction(data = res1, method = "loess",
                              columns = columns, index = index, span = 0.5)
  res3 <- do_trend_correction(data = res2, method = "loess",
                              columns = columns, index = index, span = 0.75)

  data <- data[order(data$sample_name, data$my_id), ]
  res1 <- res1[order(res1$sample_name, res1$my_id), ]
  res3 <- res3[order(res3$sample_name, res3$my_id), ]

  expect_equal(res3$areaOriginal, data$area)
  expect_equal(res3$area, res1$area)
})

test_that("do_loess corrects each batch with its own QC samples", {
  set.seed(1)
  samples <- sprintf("s%02d", 1:40)
  pools <- samples[c(seq(1, 20, by = 2), seq(22, 40, by = 2))]
  data <- expand.grid(sample_name = samples,
                      my_id = c("id1", "id2"),
                      stringsAsFactors = FALSE)
  data$order <- match(data$sample_name, samples)
  data$batch <- ifelse(data$order <= 20, 1, 2)
  data$area <- 100 + data$order + stats::rnorm(nrow(data), sd = 5)

  columns <- list(acqorder = "order", batch = "batch")
  make_index <- function(s) {
    list(selected_pools = intersect(pools, s),
         selected_samples = setdiff(s, pools))
  }

  res_all <- do_loess(data = data, columns = columns,
                      index = make_index(samples))
  res_batch <- rbind(
    do_loess(data = data[data$batch == 1, ], columns = columns,
             index = make_index(samples[1:20])),
    do_loess(data = data[data$batch == 2, ], columns = columns,
             index = make_index(samples[21:40]))
  )

  res_all <- res_all[order(res_all$sample_name, res_all$my_id), ]
  res_batch <- res_batch[order(res_batch$sample_name, res_batch$my_id), ]

  expect_equal(res_all$area, res_batch$area)
})

test_that("qc_rlsc returns NA instead of non-finite values", {
  set.seed(1)
  n <- 16
  colv <- rep(c(1, 2, 2), length.out = n)
  tab <- data.frame(
    ok = 100 + 1:n + stats::rnorm(n, sd = 5),
    zero_qc = ifelse(colv == 1, 0, 50)
  )
  tab$zero_qc[1] <- 10

  res <- qc_rlsc(tab = tab, colv = colv, or = 1:n, span = 1)

  expect_false(any(is.nan(as.matrix(res)) | is.infinite(as.matrix(res))))
  expect_true(all(is.finite(res$ok)))
})
