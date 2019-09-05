#' A function to generate a random Markov blanket polytree
#'
#' This function randomly generate a Markov blanket polytree for a given target and Markov blanket variables.
#' @param target The target varaible.
#' @param mb The potential Markov blanket of the target.
#' @return The output of this function is a Markov blanket polytree
#' @keywords matrix2dag()
#' @export
rand_mbp = function(target, mb, format = c("matrix", "dag")) {

  # sampling parents (1), children (2) and spouses (3) for the target
  repeat {# repeat the process until spouses occur with children together

    roles = sample(3, length(mb), replace = TRUE)
    if ((!3 %in% roles) || ((2 %in% roles) && (3 %in% roles))) break

  }

  pas = mb[which(roles == 1)]
  ch = mb[which(roles == 2)]
  sps = mb[which(roles == 3)]

  str = matrix(0, length(mb) + 1, length(mb) + 1, dimnames = rep(list(c(mb, target)), 2))
  str[pas, target] = 1
  str[target, ch] = 1
  if (length(sps) > 0) {# if exist spouses, sampling one child for each spouse

    for (s in sps) {

      str[s, sample(ch, 1)] = 1

    }

  }

  if (format == "dag") str = matrix2dag(str)

  return(str)

}


