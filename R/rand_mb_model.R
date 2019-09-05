#' A function to generate a random Markov blanket model (structure)
#'
#' This function randomly generate a Markov blanket model for a given target and Markov blanket variables.
#' @param target The target varaible.
#' @param mb The potential Markov blanket of the target.
#' @return The output of this function is a Markov blanket model.
#' @export
rand_mb_model = function(target, mb) {

  # sampling parents (1), children (2) and spouses (3) for the target
  repeat {# repeat the process until both children and spouses occur

    roles = ceiling(runif(length(mb))/(1/3))
    if ((!3 %in% roles) || ((2 %in% roles) && (3 %in% roles))) break

  }

  pas = mb[which(roles == 1)]
  ch = mb[which(roles == 2)]
  sps = mb[which(roles == 3)]
  varOrdering = c(pas, sample(c(target, sps)), ch)

  str = matrix(0, length(varOrdering), length(varOrdering), dimnames = rep(list(varOrdering), 2))
  str[pas, target] = 1
  str[target, ch] = 1

  # sampling common children for each spouse
  if (length(sps) > 0) {

    for (s in sps) {

      nCommonCh = sample(length(ch), 1)
      commonCh = resample(ch, nCommonCh)
      str[s, commonCh] = 1

    }

  }

  # the above steps guarantee mbIndices are in the Markov blanket of the target
  # the following steps add more arcs in

  # sampling parents for each var, starting from the 2nd var
  # ignore the target since its parents has been sampled
  # if x is a child of the target, when sampling additional parents ignore its current parents set
  k = 2
  for (x in varOrdering[-1]) {

    # potential parents should include current parents
    potentialPas = which(str[1:(k - 1), x] == 0)
    nPas = sample(0:length(potentialPas), 1)
    if (nPas > 0) {

      pas = resample(potentialPas, nPas)
      str[pas, k] = 1

    }

    k = k + 1

  } # end for each x

  return(str)

}


