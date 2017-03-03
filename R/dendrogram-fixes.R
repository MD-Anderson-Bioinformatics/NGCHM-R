# Non-recursive function to ensure the height of all dendrogram
# branches is not lower than the heights of its children.
#
dendfixh <- function (node) 
{
    if (is.leaf( node )) {
        return (node);
    }
    position <- 0;
    stack <- NULL;
    while (TRUE) {
        # Descend into all non-leaf child nodes.
	while (position < length(node)) {
	    position <- position + 1;
	    child <- node[[position]];
	    if (!is.leaf(child)) {
		stack <- list (position=position, node=node, stack=stack);
		node <- child;
		position <- 0;
	    }
	}

        # All children of current node have been processed.
        # Adjust height of current node iff needed.
	h <- max(vapply(node, function(o) attr(o, "height"), 1))
	if (attr(node, "height") < h) {
	    attr(node, "height") <- h
	}

        # Terminate if current node was the root node.
	if (length (stack) == 0) {
	    return (node);
	}

        # Come up one level in the tree.
	position <- stack$position;
	# Update parent's reference to this node.
	if (!identical (stack$node[[position]], node)) {
            # Update reference iff this node (or a child) was updated.  This
            # reduces memory churn in the usual case when no correction is necessary.
	    stack$node[[position]] <- node;
        }
	node <- stack$node;
	stack <- stack$stack;
    }
}

# Non-recursive function for str(dendrogram)
nr.str.dendrogram <- function (object, max.level = NA, digits.d = 3L, give.attr = FALSE, 
    wid = getOption("width"), nest.lev = 0, indent.str = "", 
    last.str = getOption("str.dendrogram.last"), stem = "--", 
    ...) 
{
    todo <- list( list( object=object, nest.lev=nest.lev, indent.str=indent.str));
    while (length(todo) > 0) {
        node <- todo[[1]];
        todo <- todo[-1];
	pasteLis <- function(lis, dropNam, sep = " = ") {
	    lis <- lis[!(names(lis) %in% dropNam)]
	    fl <- sapply(lis, format, digits = digits.d)
	    paste(paste(names(fl), fl, sep = sep), collapse = ", ")
	}
	istr <- sub(" $", last.str, node$indent.str);
	cat(istr, stem, sep = "")
	at <- attributes(node$object)
	memb <- at[["members"]]
	hgt <- at[["height"]]
	if (is.leaf(node$object)) {
	    cat("leaf",
                if (is.character(at$label)) paste("", at$label, "", sep = "\"") else format(node$object, digits = digits.d), "")
	    any.at <- hgt != 0
	    if (any.at) 
		cat("(h=", format(hgt, digits = digits.d))
	    if (memb != 1) 
		cat(if (any.at) ", " else { any.at <- TRUE; "(" }, "memb= ", memb, sep = "")
	    at <- pasteLis(at, c("class", "height", "members", "leaf", "label"))
	    if (any.at || nzchar(at)) 
		cat(if (!any.at) "(", at, ")")
	    cat("\n")
	}
	else {
	    le <- length(node$object)
	    if (give.attr) {
		if (nzchar(at <- pasteLis(at, c("class", "height", "members"))))
		    at <- paste(",", at)
	    }
	    cat("[dendrogram w/ ", le, " branches and ", memb, " members at h = ", 
		format(hgt, digits = digits.d),
                if (give.attr) at,
                "]",
                if (!is.na(max.level) && node$nest.lev == max.level) " ..",
                "\n",
                sep = "")
	    if (is.na(max.level) || node$nest.lev < max.level) {
		todo <- append (lapply (1:le, function(i) {
		    list(object=node$object[[i]],
			 nest.lev = node$nest.lev + 1,
			 indent.str = paste(node$indent.str, if (i < le) " |" else "  "))
		}), todo);
	    }
	}
    }
    invisible()
}

# Non-recursively count the number of leaves in a dendrogram.
nr.count.dendrogram <- function (node) {
    if (is.leaf(node)) { return (1L); }
    position <- 0L;
    stack <- NULL;
    count <- 0L;
    while (TRUE) {
        # Descend into all non-leaf child nodes, counting leaves.
	while (position < length(node)) {
	    position <- position + 1L;
	    child <- node[[position]];
	    if (is.leaf(child)) {
                count <- count + 1L;
            } else {
		stack <- list (position=position, node=node, stack=stack);
		node <- child;
		position <- 0L;
	    }
	}

        # Terminate if current node was the root node.
	if (length (stack) == 0L) {
	    return (count);
	}

        # Come up one level in the tree.
	position <- stack$position;
	node <- stack$node;
	stack <- stack$stack;
    }
}

# Non-recursive implementation of as.hclust.dendrogram
nr.as.hclust.dendrogram <- function(x, ...)
{
    node <- x;
    stopifnot(is.list(node), length(node) == 2)
    n <- nr.count.dendrogram(node);
    stopifnot(n == attr(node, "members"));

    # Ord and labels for each leaf (in preorder).
    ord <- integer(n);
    labsu <- character(n);

    # Height and inds for each node (in preorder).
    n.h <- n - 1L;
    height <- numeric(n.h);
    inds <- vector("list",n.h);

    # Starting at node, traverse dendrogram recording
    # information above about leaves and nodes encountered
    # and simplifying the dendrogram into a nested list.
    position <- 0L;  # position within children of current node
    stack <- NULL;   # parents of current node plus saved state
    leafCount <- 0L; # number of leaves seen
    nodeCount <- 0L; # number of nodes seen
    ind <- integer(); # list of indices from root to current node
    while (TRUE) {
        # Process every child of the current node.
	while (position < length(node)) {
            # Record height and index list on first visit to each node.
	    if (position == 0L) {
		nodeCount <- nodeCount + 1L;
		height[nodeCount] <- attr(node, "height");
		inds[[nodeCount]] <- ind;
	    }
	    position <- position + 1L;
	    child <- node[[position]];
	    if (is.leaf(child)) {
                # Record information about leaf child nodes.
                leafCount <- leafCount + 1L;
                ord[leafCount] <- as.integer(child);
                labsu[leafCount] <- attr(child,'label');
                node[[position]] <- - as.vector(child); # Dropping attributes
            } else {
                # Descend into non-leaf child nodes, saving state.
		stack <- list (position=position, node=node, stack=stack);
		node <- child;
                ind <- c(ind, position);
		position <- 0L;
	    }
	}
        # All children of current node have been processed.

        # Simplify node.
	attributes(node) <- NULL;

        # Terminate if current node was the root node.
	if (length (stack) == 0L) {
	    break;
	}

        # Come up one level in the tree.
	position <- stack$position;
        stack$node[[position]] <- node;  # Update parent's reference to this node
	node <- stack$node;
	stack <- stack$stack;
        ind <- ind[-length(ind)];
    }

    xS <- node;

    iOrd <- sort.list(ord)
    if(!identical(ord[iOrd], seq_len(n)))
	stop(gettextf(
	    "dendrogram entries must be 1,2,..,%d (in any order), to be coercible to \"hclust\"",
	    n), domain=NA)

    ## ties: break ties "compatibly" with inds -- relies on stable sort here:
    ii <- sort.list(height, decreasing=TRUE)[n.h:1L]
    verbose <- getOption("as.hclust.dendr", FALSE)
    merge <- matrix(NA_integer_, 2L, n.h)
    for(k in seq_len(n.h)) {
        if(verbose) cat(sprintf("ii[k=%2d]=%2d ", k, ii[k]))
	s <- if(k < n.h) {
		 if(length(in.k <- inds[[ ii[k] ]]))
		     xS[[in.k]]
	     } else xS
	if(verbose) { cat("-> s=xS[[in.k]]="); str(s) }
	stopifnot(length(s) == 2L, all( vapply(s, is.integer, NA) ))# checking..
	merge[,k] <- unlist(s)
	if(k < n.h)
	    xS[[in.k]] <- + k
    }

    structure(list(merge = t(merge),
		   height = height[ii],
		   order = ord,
		   labels = labsu[iOrd],
		   call = match.call(),
		   method = NA_character_,
		   dist.method = NA_character_),
	      class = "hclust")
}
