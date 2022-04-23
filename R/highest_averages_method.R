#' @export
highest_averages_method <- function(votes, n_seats,
                                    divisor = c("dhondt", "jefferson", "greatest_divisors",

                                                "webster", "sainte_lague",
                                                "modified_webster", "modified_sainte_lague",

                                                "adams",
                                                "danish",
                                                "dean",
                                                "equal_proportions",
                                                "huntington_hill",
                                                "imperiali"),
                                    quota = c("none", "hare", "droop", "imperiali")) {
  votes <- vec_cast(votes, double())
  n_seats <- vec_cast(n_seats, integer())
  divisor <- divisor(divisor)
  quota <- quota(quota)

  stopifnot(
    all(votes >= 0),
    is_scalar_integer(n_seats),
    n_seats >= 0,
    is.function(divisor),
    is.null(quota) || is.function(quota)
  )

  nms <- names(votes)
  votes <- ifelse(votes >= quota(votes, n_seats),
                  votes,
                  0)

  if (n_seats == 0L) {
    out <- rep(0, length(votes))
  } else {
    votes <- matrix(rep(votes, n_seats),
                    ncol = n_seats)
    divisor <- divisor(seq_len(n_seats) - 1L)

    out <- sweep(votes, 2, divisor, divide)
    out <- matrix(rank(-out, ties.method = "first"),
                  ncol = n_seats)
    out <- rowSums(out <= n_seats)
  }

  names(out) <- nms
  out
}

#' @export
divisor <- function(divisor = c("dhondt", "jefferson", "greatest_divisors",

                                "webster", "sainte_lague",
                                "modified_webster", "modified_sainte_lague",

                                "adams",
                                "danish",
                                "dean",
                                "equal_proportions",
                                "huntington_hill",
                                "imperiali")) {
  if (is.character(divisor)) {
    divisor <- arg_match(divisor,
                         c("dhondt", "jefferson", "greatest_divisors",

                           "webster", "sainte_lague",
                           "modified_webster", "modified_sainte_lague",

                           "adams",
                           "danish",
                           "dean",
                           "equal_proportions",
                           "huntington_hill",
                           "imperiali"))
    divisor <- switch(
      divisor,

      dhondt = function(x) x + 1,
      jefferson = function(x) x + 1,
      greatest_divisors = function(x) x + 1,

      webster = function(x) x + 0.5,
      sainte_lague = function(x) x + 0.5,

      modified_webster = function(x) ifelse(x == 0L, 0.7, x + 0.5),
      modified_sainte_lague = function(x) ifelse(x == 0L, 0.7, x + 0.5),

      adams = identity,
      danish = function(x) x + 1 / 3,
      dean = function(x) x * (x + 1) / (x + 0.5),
      equal_proportions = function(x) sqrt(x * (x - 1)),
      huntington_hill = function(x) sqrt(x * (x + 1)),
      imperiali = function(x) x + 2
    )
  } else {
    divisor <- as_function(divisor)
  }

  divisor
}

#' @export
quota <- function(quota = c("none", "hare", "droop", "imperiali")) {
  if (is.character(quota)) {
    quota <- arg_match(quota, c("none", "hare", "droop", "imperiali"))
    quota <- switch(
      quota,
      none = function(votes, n_seats) 0,
      hare = function(votes, n_seats) sum(votes) / n_seats,
      droop = function(votes, n_seats) floor(sum(votes) / (n_seats + 1L)) + 1,
      imperiali = function(votes, n_seats) sum(votes) / (n_seats + 2L)
    )
  } else {
    quota <- as_function(quota)
  }

  quota
}
