test_that("highest_averages_method", {
  votes <- c(yellow = 47000,
             white = 16000,
             red = 15900,
             green = 12000,
             blue = 6000,
             pink = 3100)
  n_seats <- 10L

  check <- function(fun_divisor, expected,
                    fun_quota = "none") {
    out <- highest_averages_method(votes, n_seats,
                                   fun_divisor = fun_divisor,
                                   fun_quota = fun_quota)
    expect_equal(unname(out), expected)
  }

  check("dhondt", c(5, 2, 2, 1, 0, 0))
  check("sainte_lague", c(4, 2, 2, 1, 1, 0))
  check("modified_sainte_lague", c(5, 2, 2, 1, 0, 0))
  check("huntington_hill", c(5, 2, 2, 1, 0, 0),
        fun_quota = "hare")
  check("adams", c(3, 2, 2, 1, 1, 1))
})
