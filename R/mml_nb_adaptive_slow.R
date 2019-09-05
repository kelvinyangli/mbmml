#' MML Naive Bayes using adaptive code approach (slower version)
#'
#' This function calculates the mml score of a NB model using the adaptive code approach. It is much simpler than using
#' MML87 that involves complex fisher calculation. The output is the mml score of a NB without adding the extra bit
#' for each parameter as what the multi-state mml adaptive code does, because we don't know the complete message length
#' from MML87 for NB. But maybe this is ok, because we are not interested in the mml estimation of parameters.
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param arities A vector of variable arities.
#' @param paIndex The target variable (or parent node) index.
#' @param chIndices A vector of indices for the Xs (or child nodes).
#' @export
mml_nb_adaptive_slow = function(data, arities, paIndex, chIndices) {

  msgLen = 0
  paCnt = rep(1, arities[paIndex]) # parent value count

  # a list to store children count, the reason a list is used is because different
  # child nodes could have different arities
  if (length(chIndices) > 0) {

    chCnt = list()
    for (j in 1:length(chIndices)) {

      chCnt[[j]] = rep(1, prod(arities[c(paIndex, chIndices[j])]))

    }

  }

  for (rowID in 1:nrow(data)) {

    paValue = data[rowID, paIndex]
    p = paCnt / sum(paCnt) # marginal distribution p(paNode)

    if (length(chIndices) > 0) {

      chValues = data[rowID, chIndices]
      probXCondY = chCnt # initialize prob(x_j | y)
      probX = 0 # marginal probability p(x_1, x_2, ...)
      for (i in 1:arities[paIndex]) {# for each value of the parent node

        probXJointY = p[i] # initialize p(x_j, y) with p(y)
        for (j in 1:length(chIndices)) {# for each child node

          indices = ((i - 1) * prod(arities[chIndices[j]]) + 1):(prod(arities[chIndices[j]]) * i)
          probXCondY[[j]][indices] = chCnt[[j]][indices] / sum(chCnt[[j]][indices]) # update p(x_j | y)
          index = node_value_to_index(arities[c(chIndices[j], paIndex)], c(chValues[j], i))
          probXJointY = probXJointY * probXCondY[[j]][index]
          if (i == paValue) {

            chCnt[[j]][index] = chCnt[[j]][index] + 1 # update count x|y by 1
            #cat(unlist(chCnt), "\n")

          }

        }# end for each child node

        # if y's ith value matches its current value, then cach the joint probability p(x, y)
        if (i == paValue) cachedValue = probXJointY
        probX = probX + probXJointY # keep marginalizing p(x, y) to get p(x_1, x_2, ...)

      }# end for each value of the parent node

      probYCondX = cachedValue / probX
      #cat(probYCondX, "\n")
      msgLen = msgLen - log(probYCondX) # update message length
      #cat(msgLen, "\n")

    } else {

      msgLen = msgLen - log(p[paValue])
      #cat(p[paValue], "\n")

    }

    paCnt[paValue] = paCnt[paValue] + 1 # update count y by 1
    #cat(paCnt, "\n")

  } # end for each row

  return(msgLen)

}







