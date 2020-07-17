as.matrix.ngchmLayer <- function (x, ...) {
    as.matrix (x@data, typeProto=1)
};

as.matrix.ngchmBar <- function (x, ...) {
    typeProto <- if (x@type == "discrete") "a" else 1;
    as.matrix (x@data, typeProto=typeProto, ...)
};

as.matrix.shaid <- function (x, ..., typeProto=1, debug=FALSE) {
    stopifnot (is(x,"shaid"), x@type=='dataset');
    repo <- NGCHM:::ngchmFindRepo (x, FALSE);
    if (length(repo) == 0) {
        stop("Unable to locate data matrix on a connected shaidy server");
    }
    if (repo$accessMethod == "file") {
	localrepo <- repo;
	scb <- NULL;
    } else {
        scb <- shaidyCopyBlob (repo, x, NGCHM:::ngchm.env$tmpShaidy);
        if (length(scb) == 0) stop("Unable to locate data matrix on a connected shaidy server");
	localrepo <- NGCHM:::ngchm.env$tmpShaidy;
    }
    mat <- tsvio::tsvGetData (localrepo$blob.path (x, 'matrix.tsv'), localrepo$blob.path (x, 'index.tsv'), NULL, NULL, typeProto);
    if (debug) list (x=x, repo=repo, scb=scb, localrepo=localrepo, mat=mat) else mat
};

#' Load an NG-CHM from an NG-CHM server.
#'
#' @param mapid An NG-CHM ShaidyID that identifies the NG-CHM to download.
#' @param debug If TRUE, return a list containing additional information .
#'
#' @return An object of class ngchm.
#' @export
#'
#' @seealso [chmInstall()]
#' @seealso [ngchmPushSourceServer()]
#'
chmLoadShaidyCHM <- function (mapid, debug=FALSE) {
    shaid <- new ('shaid', type='chm', value=mapid);
    repo <- ngchmFindRepo (shaid);
    json <- repo$loadProperty (shaid, 'chm');
    chm <- chmFromJSON (json);
    if (debug) list(shaid=shaid,repo=repo,json=json,chm=chm) else chm
};

covariateBarsFromJSON <- function (hm, cvr, js) {
    stopifnot (is (hm, "ngchm"));
    stopifnot (length(cvr) == 0 || typeof(cvr) == "list");
    stopifnot (length(js) == 0 || typeof(js) == "list");
    lapply (js, function (jj) {
    	stopifnot (jj$class == "ngchmBar");
	typeProto <- if (jj$type == "discrete") "a" else 1;
	values <- as.matrix(new("shaid", type=jj$data$type, value=jj$data$value), typeProto=typeProto);
	if (class(values) == "matrix") values <- values[,1];
	cv <- chmNewCovariate (fullname=jj$label, values=values, value.properties=cvr[[jj$renderer+1]], type=jj$type);
	chmNewCovariateBar (cv, display=jj$display, thickness=jj$thickness, merge=jj$merge)
    })
};

axisTypesFromJSON <- function (js) {
    stopifnot (length(js) == 0 || typeof(js) == "list");
    lapply (js, function (jj) {
    	stopifnot (jj$class == "ngchmAxisType");
	new (jj$class, where=jj$where, type=jj$type, func=chmGetFunction ("getLabelValue"));
    })
};

chmFromJSON <- function (js) {
	stopifnot (js$class == 'ngchm');
	stopifnot (js$version == 2);
	stopifnot (js$name != '');

	shaidyRepo <- ngchm.env$tmpShaidy;
	hm = chmNew (js$name);
	renderers <- list();
	for (cm in js$renderers) {
	    pts <- list();
	    for (pt in cm$points) {
		pts <- append (pts, new (Class="ngchmValueProp", value=pt$value, color=pt$color, name=pt$name, shape=pt$shape, z=pt$z));
	    }
	    renderers <- append (renderers, new (Class="ngchmColormap", type=cm$type, missing=cm$missing, points=pts));
	}
	cvrenderers <- list();
	for (cm in js$covariate_renderers) {
	    pts <- list();
	    for (pt in cm$points) {
		pts <- append (pts, new (Class="ngchmValueProp", value=pt$value, color=pt$color, name=pt$name, shape=pt$shape, z=pt$z));
	    }
	    cvrenderers <- append (cvrenderers, new (Class="ngchmColormap", type=cm$type, missing=cm$missing, points=pts));
	}
	for (ll in js$layers) {
		data <- new ('shaid', type=ll$data$type, value=ll$data$value);
		cutsColor <- if (exists('cuts_color',ll)) ll$cuts_color else "#4c4c4c";
		summaryMethod <- if (exists('summary_method',ll)) ll$summary_method else "average";
		layer <- new (Class="ngchmLayer", name=ll$name, data=data,
			colors=renderers[[ll$renderer+1]], summarizationMethod=summaryMethod, cuts_color=cutsColor);
		hm <- chmAddLayer (hm, layer);
	}
	for (pp in js$properties) {
		stopifnot (pp$class == "ngchmProperty");
		hm <- chmAddProperty (hm, pp$label, pp$value);
	}
        # rowOrder="optDendrogram", rowDist="charOrFunction", rowAgglom="charOrFunction",
        # colOrder="optDendrogram", colDist="charOrFunction", colAgglom="charOrFunction",
        # rowOrderMethod="character", colOrderMethod="character",
	if (exists('col_data',js)) {
		if (exists('order_method',js$col_data)) hm@colOrderMethod = js$col_data$order_method;
		if (exists('distance_metric',js$col_data)) hm@colDist = js$col_data$distance_metric;
		if (exists('agglomeration_method',js$col_data)) hm@colAgglom = js$col_data$agglomeration_method;
		if (exists('dendrogram',js$col_data)) {
			hm@colOrder = shaid = new ('shaid', type=js$col_data$dendrogram$type, value=js$col_data$dendrogram$value);
			if (exists('labels',js$col_data)) {
			    provid <- shaidyProvenance (shaidyRepo, name="ngchmGetLabels", type=shaid@type, shaid=shaid@value, axis=NULL);
			    labshaid <- new ('shaid', type=js$col_data$labels$type, value=js$col_data$labels$value);
			    shaidyRepo$provenanceDB$insert (provid, labshaid);
			    #cat (file=stderr(), 'Added column dendro provid', provid, labshaid@value, '\n');
			}
		} else if (exists('labels',js$col_data)) {
			hm@colOrder = new ('shaid', type=js$col_data$labels$type, value=js$col_data$labels$value);
		}
		if (exists('covariates',js$col_data)) hm@colCovariateBars <- covariateBarsFromJSON (hm, cvrenderers, js$col_data$covariates);
	}
	if (exists('row_data',js)) {
		if (exists('order_method', js$row_data)) hm@rowOrderMethod = js$row_data$order_method;
		if (exists('distance_metric', js$row_data)) hm@rowDist = js$row_data$distance_metric;
		if (exists('agglomeration_method', js$row_data)) hm@rowAgglom = js$row_data$agglomeration_method;
		if (exists('dendrogram', js$row_data)) {
			hm@rowOrder = shaid = new ('shaid', type=js$row_data$dendrogram$type, value=js$row_data$dendrogram$value);
			if (exists('labels',js$row_data)) {
			    provid <- shaidyProvenance (shaidyRepo, name="ngchmGetLabels", type=shaid@type, shaid=shaid@value, axis=NULL);
			    labshaid <- new ('shaid', type=js$row_data$labels$type, value=js$row_data$labels$value);
			    shaidyRepo$provenanceDB$insert (provid, labshaid);
			    #cat (file=stderr(), 'Added row dendro provid', provid, labshaid@value, '\n');
			}
		} else if (exists('labels', js$row_data)) {
			hm@rowOrder = new ('shaid', type=js$row_data$labels$type, value=js$row_data$labels$value);
		}
		if (exists('covariates',js$row_data)) hm@rowCovariateBars <- covariateBarsFromJSON (hm, cvrenderers, js$row_data$covariates);
	}
	if (exists('axisTypes',js)) hm@axisTypes <- axisTypesFromJSON (js$axisTypes);
	return (hm);
}
