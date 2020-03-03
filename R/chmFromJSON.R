useServerAsRepository <- function (server) {
    sr <- NGCHM:::shaidyLoadRepository(server@protoOpts$accessMethod, server@protoOpts$basePath);
    assign(envir=NGCHM:::ngchm.env, 'shaidyStack', c(list(sr), NGCHM:::ngchm.env$shaidyStack));
}

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
	for (ll in js$layers) {
		data <- new ('shaid', type=ll$data$type, value=ll$data$value);
		layer <- new (Class="ngchmLayer", name=ll$name, data=data,
			colors=renderers[[ll$renderer+1]], summarizationMethod=ll$summary_method, cuts_color=ll$cuts_color);
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
			    cat (file=stderr(), 'Added column dendro provid', provid, labshaid@value, '\n');
			}
		} else if (exists('labels',js$col_data)) {
			hm@colOrder = new ('shaid', type=js$col_data$labels$type, value=js$col_data$labels$value);
		}
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
			    cat (file=stderr(), 'Added row dendro provid', provid, labshaid@value, '\n');
			}
		} else if (exists('labels', js$row_data)) {
			hm@rowOrder = new ('shaid', type=js$row_data$labels$type, value=js$row_data$labels$value);
		}
	}
	return (hm);
}
