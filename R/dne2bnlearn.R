#' Convert netica's dne file to bnlearn dag format
#'
#' This is a function to convert the dag saved in netica's dne format (particularly
#' for CaMML's output) to bnlearn's dag format. The function has dependency
#' on readr::read_file to read dne file.
#' @param dneFile a DAG saved in dne format.
#' @export

dne2bnlearn = function(dneFile) {
  bnInfo = strsplit(dneFile, "node")[[1]][-1]
  allNodes = vector(length = length(bnInfo))
  parentsList = list()

  for (i in 1:length(bnInfo)) {

    info = strsplit(bnInfo[i], "\n\t")[[1]]
    allNodes[i] = trimws(gsub("[[:punct:]]", "", info[1]))
    parents = trimws(gsub("[[:punct:]]", "", strsplit(info[6], "=")[[1]][2]))

    if (nchar(parents) > 0) {# if there is at least one parent

      parentsList[[i]] = strsplit(parents, " ")[[1]]

    } else {# if there is no parent

      parentsList[[i]] = vector()

    } # end else

  } # end for i

  names(parentsList) = allNodes
  ls = list(allNodes, parentsList)
  names(ls) = c("node", "parent")

  dag = empty.graph(ls$node)

  for (i in 1:length(ls$parent)) {# add directed arc from its parents to each node i

    if (length(ls$parent[[i]]) > 0) {# only add arc if there is at least 1 parent for node i

      for (j in 1:length(ls$parent[[i]])) {# for each parent j

        dag = set.arc(dag, ls$parent[[i]][j], ls$node[i])

      }# end for j

    }# end if

  }# end for i

  return(dag)
}
