#' @export
highest_averages_method <- function(votes, n_seats,
                                    fun_divisor = c("dhondt", "jefferson", "greatest_divisors",

                                                    "webster", "sainte_lague",
                                                    "modified_webster", "modified_sainte_lague",

                                                    "huntington_hill",

                                                    "adams",
                                                    "danish",
                                                    "dean",
                                                    "equal_proportions",
                                                    "imperiali"),
                                    fun_quota = c("none", "hare", "droop", "imperiali"),
                                    ties.method = c("first", "last", "random")) {

  rank <- mat_quotient(votes = votes,
                       n_seats = n_seats,
                       fun_divisor = fun_divisor,
                       fun_quota = fun_quota,
                       rank = TRUE,
                       ties.method = ties.method)
  rowSums(rank <= n_seats)
}

#' @export
fun_divisor <- function(fun = c("dhondt", "jefferson", "greatest_divisors",

                                "webster", "sainte_lague",
                                "modified_webster", "modified_sainte_lague",

                                "huntington_hill",

                                "adams",
                                "danish",
                                "dean",
                                "equal_proportions",
                                "imperiali")) {
  if (is.character(fun)) {
    fun <- arg_match(fun,
                     c("dhondt", "jefferson", "greatest_divisors",

                       "webster", "sainte_lague",
                       "modified_webster", "modified_sainte_lague",

                       "huntington_hill",

                       "adams",
                       "danish",
                       "dean",
                       "equal_proportions",
                       "imperiali"))
    fun <- switch(
      fun,

      dhondt = function(x) x + 1,
      jefferson = function(x) x + 1,
      greatest_divisors = function(x) x + 1,

      webster = function(x) x + 0.5,
      sainte_lague = function(x) x + 0.5,

      modified_webster = function(x) ifelse(x == 0L, 0.7, x + 0.5),
      modified_sainte_lague = function(x) ifelse(x == 0L, 0.7, x + 0.5),

      huntington_hill = function(x) sqrt(x * (x + 1)),

      adams = function(x) x,
      danish = function(x) x + 1 / 3,
      dean = function(x) x * (x + 1) / (x + 0.5),
      equal_proportions = function(x) sqrt(x * (x - 1)),
      imperiali = function(x) x + 2
    )
  } else {
    fun <- as_function(fun)
  }

  fun
}

#' @export
fun_quota <- function(fun = c("none", "hare", "droop", "imperiali")) {
  if (is.character(fun)) {
    fun <- arg_match(fun, c("none", "hare", "droop", "imperiali"))
    fun <- switch(
      fun,
      none = function(votes, n_seats) 0,
      hare = function(votes, n_seats) sum(votes) / n_seats,
      droop = function(votes, n_seats) floor(sum(votes) / (n_seats + 1L)) + 1,
      imperiali = function(votes, n_seats) sum(votes) / (n_seats + 2L)
    )
  } else {
    fun <- as_function(fun)
  }

  fun
}

#' @export
mat_quotient <- function(votes, n_seats,
                         fun_divisor = c("dhondt", "jefferson", "greatest_divisors",

                                         "webster", "sainte_lague",
                                         "modified_webster", "modified_sainte_lague",

                                         "huntington_hill",

                                         "adams",
                                         "danish",
                                         "dean",
                                         "equal_proportions",
                                         "imperiali"),
                         fun_quota = c("none", "hare", "droop", "imperiali"),
                         rank = FALSE,
                         ties.method = c("first", "last", "random")) {
  votes <- vec_cast(votes, double())
  n_seats <- vec_cast(n_seats, integer())
  fun_divisor <- fun_divisor(fun_divisor)
  fun_quota <- fun_quota(fun_quota)

  stopifnot(
    all(votes >= 0),
    is_scalar_integer(n_seats),
    n_seats > 0,
    is.function(fun_divisor),
    is.null(fun_quota) || is.function(fun_quota)
  )

  nms <- names(votes)
  votes <- ifelse(votes >= fun_quota(votes, n_seats),
                  votes,
                  0)

  votes <- matrix(rep(votes, n_seats),
                  ncol = n_seats)
  divisor <- fun_divisor(seq_len(n_seats) - 1L)

  out <- sweep(votes, 2, divisor, divide)
  dimnames(out)[[1L]] <- nms

  if (rank) {
    ties.method <- arg_match(ties.method, c("first", "last", "random"))

    out <- matrix(rank(-out, ties.method = ties.method),
                  ncol = n_seats,
                  dimnames = dimnames(out))
  }

  out
}
