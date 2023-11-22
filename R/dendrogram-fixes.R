# Non-recursive function to ensure the height of all dendrogram
# branches is not lower than the heights of its children.
#
dendfixh <- function(node) {
  if (is.leaf(node)) {
    return(node)
  }
  position <- 0
  stack <- NULL
  while (TRUE) {
    # Descend into all non-leaf child nodes.
    while (position < length(node)) {
      position <- position + 1
      child <- node[[position]]
      if (!is.leaf(child)) {
        stack <- list(position = position, node = node, stack = stack)
        node <- child
        position <- 0
      }
    }

    # All children of current node have been processed.
    # Adjust height of current node iff needed.
    h <- max(vapply(node, function(o) attr(o, "height"), 1))
    if (attr(node, "height") < h) {
      attr(node, "height") <- h
    }

    # Terminate if current node was the root node.
    if (length(stack) == 0) {
      return(node)
    }

    # Come up one level in the tree.
    position <- stack$position
    # Update parent's reference to this node.
    if (!identical(stack$node[[position]], node)) {
      # Update reference iff this node (or a child) was updated.  This
      # reduces memory churn in the usual case when no correction is necessary.
      stack$node[[position]] <- node
    }
    node <- stack$node
    stack <- stack$stack
  }
}

# Non-recursive function for str(dendrogram)
nr.str.dendrogram <-
  function(object, max.level = NA, digits.d = 3L, give.attr = FALSE,
           wid = getOption("width"), nest.lev = 0L, indent.str = "",
           last.str = getOption("str.dendrogram.last"), stem = "--", ...) {
    ## TO DO: when object is part of a larger structure which is str()ed
    ##    with default max.level= NA, it should not be str()ed to all levels,
    ##   but only to e.g. level 2
    ## Implement via smarter default for 'max.level' (?)

    pasteLis <- function(lis, dropNam, sep = " = ") {
      ## drop uninteresting "attributes" here
      lis <- lis[!(names(lis) %in% dropNam)]
      fl <- sapply(lis, format, digits = digits.d)
      paste(paste(names(fl), fl, sep = sep), collapse = ", ")
    }

    todo <- NULL # Nodes to process after this one
    repeat {
      ## when  indent.str  ends in a blank, i.e. "last" (see below)
      istr <- sub(" $", last.str, indent.str)
      cat(istr, stem, sep = "")

      at <- attributes(object)
      memb <- at[["members"]]
      hgt <- at[["height"]]
      if (!is.leaf(object)) {
        le <- length(object)
        if (give.attr) {
          if (nzchar(at <- pasteLis(at, c("class", "height", "members")))) {
            at <- paste(",", at)
          }
        }
        cat("[dendrogram w/ ", le, " branches and ", memb, " members at h = ",
          format(hgt, digits = digits.d), if (give.attr) at,
          "]", if (!is.na(max.level) && nest.lev == max.level) " ..", "\n",
          sep = ""
        )
        if (is.na(max.level) || nest.lev < max.level) {
          # Push children onto todo list in reverse order.
          # Assumes at least one child.
          nest.lev <- nest.lev + 1L
          todo <- list(object = object[[le]], nest.lev = nest.lev, indent.str = paste(indent.str, "  "), todo = todo)
          indent.str <- paste(indent.str, " |")
          while ((le <- le - 1L) > 0L) {
            todo <- list(object = object[[le]], nest.lev = nest.lev, indent.str = indent.str, todo = todo)
          }
        }
      } else { ## leaf
        cat(
          "leaf",
          if (is.character(at$label)) {
            paste("", at$label, "", sep = '"')
          } else {
            format(object, digits = digits.d)
          }, ""
        )
        any.at <- hgt != 0
        if (any.at) cat("(h=", format(hgt, digits = digits.d))
        if (memb != 1) { # MM: when can this happen?
          cat(if (any.at) {
            ", "
          } else {
            any.at <- TRUE
            "("
          }, "memb= ", memb, sep = "")
        }
        at <- pasteLis(at, c("class", "height", "members", "leaf", "label"))
        if (any.at || nzchar(at)) cat(if (!any.at) "(", at, ")")
        cat("\n")
      }
      # Advance to next node, if any.
      if (is.null(todo)) {
        break
      } else {
        object <- todo$object
        nest.lev <- todo$nest.lev
        indent.str <- todo$indent.str
        todo <- todo$todo
      }
    }
    invisible()
  }

# Non-recursively count the number of leaves in a dendrogram.
nleaves <- function(node) {
  if (is.leaf(node)) {
    return(1L)
  }
  todo <- NULL # Non-leaf nodes to traverse after this one.
  count <- 0L
  repeat {
    # For each child: count iff a leaf, add to todo list otherwise.
    while (length(node)) {
      child <- node[[1L]]
      node <- node[-1L]
      if (is.leaf(child)) {
        count <- count + 1L
      } else {
        todo <- list(node = child, todo = todo)
      }
    }
    # Advance to next node, terminating when no nodes left to count.
    if (is.null(todo)) {
      break
    } else {
      node <- todo$node
      todo <- todo$todo
    }
  }
  return(count)
}

## Reversing the above (as much as possible)
## is only possible for dendrograms with *binary* splits
nr.as.hclust.dendrogram <- function(x, ...) {
  stopifnot(is.list(x), length(x) == 2L)
  n <- nleaves(x)
  stopifnot(n == attr(x, "members"))

  # Ord and labels for each leaf node (in preorder).
  ord <- integer(n)
  labsu <- character(n)

  # Height and (parent,index) for each internal node (in preorder).
  n.h <- n - 1L
  height <- numeric(n.h)
  myIdx <- matrix(NA_integer_, 2L, n.h)

  # Record merges initially in preorder traversal
  # We will resort into merge order at end.
  merge <- matrix(NA_integer_, 2L, n.h)

  # Starting at root, traverse dendrogram recording
  # information above about leaves and nodes encountered
  position <- 0L # position within current node
  stack <- NULL # parents of current node plus saved state
  leafCount <- 0L # number of leaves seen
  nodeCount <- 0L # number of nodes seen
  repeat {
    # Pre-order traversal of the current node.
    # Will descend into non-leaf children pushing parents onto stack.
    while (length(x)) {
      # Record height and index list on first visit to each internal node.
      if (position == 0L) {
        nodeCount <- nodeCount + 1L
        myNodeIndex <- nodeCount
        if (nodeCount != 1L) {
          myIdx[, nodeCount] <- c(stack$position, stack$myNodeIndex)
        }
        height[nodeCount] <- attr(x, "height")
      }
      position <- position + 1L
      child <- x[[1L]]
      x <- x[-1L]
      if (is.leaf(child)) {
        # Record information about leaf nodes.
        leafCount <- leafCount + 1L
        labsu[leafCount] <- attr(child, "label")
        ord[leafCount] <- as.integer(child)
        merge[position, myNodeIndex] <- -ord[leafCount]
      } else {
        stopifnot(length(child) == 2L)
        # Descend into non-leaf nodes, saving state on stack.
        stack <- list(node = x, position = position, myNodeIndex = myNodeIndex, stack = stack)
        x <- child
        position <- 0L
      }
    }
    # All children of current node have been traversed.

    # Terminate if current node was the root node.
    if (is.null(stack)) {
      break
    }

    # Otherwise, pop parent node and state.
    position <- stack$position # Restore position in parent node.
    x <- stack$node
    myNodeIndex <- stack$myNodeIndex
    stack <- stack$stack
  }

  iOrd <- sort.list(ord)
  if (!identical(ord[iOrd], seq_len(n))) {
    stop(gettextf(
      "dendrogram entries must be 1,2,..,%d (in any order), to be coercible to \"hclust\"",
      n
    ), domain = NA)
  }

  ## ties: break ties "compatibly" with above preorder traversal -- relies on stable sort here:
  ii <- sort.list(height, decreasing = TRUE)[n.h:1L]
  stopifnot(ii[n.h] == 1L)

  # Record internal merges
  k <- seq_len(n.h - 1L)
  merge[t(myIdx[, ii[k]])] <- +k

  if (getOption("as.hclust.dendr", FALSE)) {
    for (k in seq_len(n.h)) {
      cat(sprintf("ii[k=%2d]=%2d ", k, ii[k]))
      cat("-> s=merge[[,ii[k]]]=")
      str(merge[, ii[k]])
    }
  }

  structure(
    list(
      merge = t(merge[, ii]), # Resort into merge order
      height = height[ii], # Resort into merge order
      order = ord,
      labels = labsu[iOrd],
      call = match.call(),
      method = NA_character_,
      dist.method = NA_character_
    ),
    class = "hclust"
  )
}
