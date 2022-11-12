#' Javascript extensions for the Next Generation Clustered Heat Map (NGCHM) Construction Library
#'
#' Currently:
#' \itemize{
#'   \item Axis function View Ideogram is added for the appropriate axis types.
#' }
#'
#' @seealso [chmGetFunction()]
#' @seealso [chmListFunctions()]
#'
#' @name NGCHM-functions
#' @rdname NGCHM-functions
#' @aliases NGCHM-functions
NULL

ngchm.env <- new.env(parent=emptyenv());

.initNGCHM <- function () {
    # Populate library environment.
    ngchm.env$uuid <- paste(sample (c(letters,0:9,toupper(letters)), 50, replace=TRUE),collapse="");
    ngchm.env$scripts <- c();
    ngchm.env$axisFunctions <- NULL;
    ngchm.env$matrixFunctions <- NULL;
    ngchm.env$typeInfo <- NULL;
    ngchm.env$typeMappers <- NULL;
    ngchm.env$serverProtocols <- NULL;
    ngchm.env$toolbox <- NULL;
    ngchm.env$servers <- list();
    ngchm.env$deployServerConfigs <- new.env(parent=emptyenv());
    ngchm.env$parseFns <- new.env(parent=emptyenv());
    ngchm.env$jarCache <- new.env(parent=emptyenv());
    ngchm.env$handledb <- new.env (hash=TRUE, parent=emptyenv());
    ngchm.env$nextId <- 0;
    shaidyInit();
    ngchmShaidyInit ();
}

#' Specify per-user configuration for a specific deploy Server.
#'
#' @param server The server for which the configuration is being set.  Must have deployServer set.
#' @param config The configuration to set
#'
#' @export
chmSetDeployServerConfig <- function (server, config) {
    if (is(server,"character")) server <- chmServer (server);
    assign (server@deployServer, config, ngchm.env$deployServerConfigs);
    NULL
}

#' Get per-user configuration for a specific deploy Server.
#'
#' @param server The server for which the configuration is being set.  Must have deployServer set.
#'
#' @export
chmGetDeployServerConfig <- function (server) {
    if (is(server,"character")) server <- chmServer (server);
    if (exists (server@deployServer, ngchm.env$deployServerConfigs)) {
        get (server@deployServer, ngchm.env$deployServerConfigs)
    } else {
	NULL
    }
}


# Get the list of configuration directories.
#
getConfigDirs <- function () {

    configpath <- Sys.getenv ("NGCHMCONFIGPATH");
    sep <- if (Sys.info()[['sysname']] == "Windows") ";" else ":";
    if (nzchar(configpath) == 0) {
	configpath <- paste ("/etc/ngchm", "/usr/local/ngchm", "/opt/ngchm",
			     file.path (Sys.getenv("HOME"), ".ngchm"),
			     sep=sep);
    }

    ngchm.env$configdirs <- strsplit (configpath, sep)[[1]];
}

# Remove leading and trailing spaces from s.
#
spstrip <- function (s) {
    sub ("^ *", "", sub (" *$", "", s))
}

# Simple Javascript parser for extracting JSDoc components.
# Returns a list of JSDoc components.
# Each JSDoc Component is a list of two fields:
#     tags: a list of JSDoc tag fields
#     src: a string containing Javascript source code.
parseJSDoc <- function (filename, lines) {
    alldocs <- list();

    sources <- c();
    jstags <- list();
    incomment <- FALSE;
    inJSDoc <- FALSE;
    for (line in lines) {
	if (!inJSDoc) sources <- append (sources, line);
	line <- sub ("^  *", "", line);
	while (nchar(line) > 0) {
	    if (incomment) {
	        if (substr(line,1,2) == "*/") {
		    line <- substring (line, 3);
		    incomment <- FALSE;
		    inJSDoc <- FALSE;
		}
		else if (inJSDoc && (substr(line,1,1)=="@")) {
		    line <- substring(line,2);
		    fields <- strsplit (line,' ')[[1]];
		    jstags <- append (jstags, list(fields));
		    line <- "";
		}
		else {
		    line <- sub ("^[*@]*", "", substring(line,2));
		}
	    }
	    else if (substr(line,1,2) == "/*") {
	        line <- substring (line, 3);
		incomment <- TRUE;
		inJSDoc <- FALSE;
		if ((nchar(line) > 0) && (substr(line,1,1) == '*')) {
		    if ((nchar(line) == 1) || (substr(line,2,1) != '*')) {
		        inJSDoc <- TRUE;
			src <- paste(sources[-length(sources)], collapse='\n');
			if (length(jstags)>0)
			    alldocs <- append (alldocs, list(list(tags=jstags,src=src)));
			jstags <- list();
			sources <- c();
		    }
		}
	    }
	    else if (substr(line,1,2) == "//") {
	        line <- "";
	    }
	    else if (substr(line,1,1) == "'") {
	        tmp <- sub ("^'[^']*'", "", line);
		if (line == tmp) stop (sprintf ("In Javascript file %s, unterminated string: %s", filename, line));
		line <- tmp;
	    }
	    else if (substr(line,1,1) == '"') {
	        tmp <- sub ('^"[^"]*"', "", line);
		if (line == tmp) stop (sprintf ("In Javascript file %s, unterminated string %s", filename, line));
		line <- tmp;
	    }
	    else {
	        line <- sub ("^[^'\"/]*", "", substring(line,2));
	    }
	    line <- sub ("^  *", "", line);
	}
    }
    if (incomment) {
        stop (sprintf ("Encountered EOF while parsing comment in Javascript file '%s'", filename));
    }
    if (length(jstags)>0) {
	src <- paste(sources, collapse='\n');
	alldocs <- append (alldocs, list(list(tags=jstags,src=src)));
    }
    return (alldocs);
}

jsTagExists <- function (jsdoc, tag) {
    any(vapply(jsdoc,function(x)x[1]==tag,TRUE))
}

jsGetTag <- function (jsdoc, tag) {
    idx <- which(vapply(jsdoc,function(x)x[1]==tag,TRUE));
    if (length(idx)==0) stop (sprintf ("Required tag '%s' not found in Javascript JSDoc", tag));
    jsdoc[[idx[length(idx)]]]
}

jsGetOptTag <- function (jsdoc, tag) {
    idx <- which(vapply(jsdoc,function(x)x[1]==tag,TRUE));
    if (length(idx)==0) NULL else jsdoc[[idx[length(idx)]]]
}

# Load a Javascript file.
#
loadJavascript <- function(filename) {
    # Load configuration from text file.
    lines <- NULL;
    try (suppressWarnings(lines <- readLines (filename)), silent=TRUE);
    if (length(lines) > 0) {
        jsdocs <- parseJSDoc (filename, lines);
	for (jsdoc in jsdocs) {
	    jsTags <- jsdoc$tags;
	    if (jsTagExists (jsTags, 'axisfunction')) {
		name <- jsGetTag(jsTags,'name')[2];
		desc <- paste(jsGetTag(jsTags,'description')[-1],collapse=' ');
		atype <- jsGetTag(jsTags,'axisfunction')[2];
		menue <- paste(jsGetTag(jsTags,'menuentry')[-1],collapse=' ');
		extras <- jsGetOptTag (jsTags, 'extraparams')[-1];
		requires <- jsGetOptTag (jsTags, 'requires')[-1];
		fn <- chmNewFunction (name, desc, jsdoc$src, extraParams=extras, requires=requires);
		chmRegisterAxisFunction (atype, menue, fn);
	    }
	    else if (jsTagExists (jsTags, 'matrixfunction')) {
		name <- jsGetTag(jsTags,'name')[2];
		desc <- paste(jsGetTag(jsTags,'description')[-1],collapse=' ');
		rtype <- jsGetTag(jsTags,'matrixfunction')[2];
		ctype <- jsGetTag(jsTags,'matrixfunction')[3];
		menue <- paste(jsGetTag(jsTags,'menuentry')[-1],collapse=' ');
		extras <- jsGetOptTag (jsTags, 'extraparams')[-1];
		requires <- jsGetOptTag (jsTags, 'requires')[-1];
		fn <- chmNewFunction (name, desc, jsdoc$src, extraParams=extras, requires=requires);
		chmRegisterMatrixFunction (rtype, ctype, menue, fn);
	    }
	    else if (jsTagExists (jsTags, 'toolboxfunction')) {
		name <- jsGetTag(jsTags,'name')[2];
		desc <- paste(jsGetTag(jsTags,'description')[-1],collapse=' ');
		tooltype <- jsGetTag(jsTags,'toolboxfunction')[2];
		menue <- paste(jsGetTag(jsTags,'menuentry')[-1],collapse=' ');
		extras <- jsGetTag(jsTags,'extraparams')[-1];
		requires <- jsGetOptTag (jsTags, 'requires')[-1];
		fn <- chmNewFunction (name, desc, jsdoc$src, extraParams=extras, requires=requires);
		chmRegisterToolboxFunction (tooltype, menue, fn);
	    }
	    else if (jsTagExists (jsTags, 'globalfunction')) {
		name <- jsGetTag(jsTags,'name')[2];
		desc <- paste(jsGetTag(jsTags,'description')[-1],collapse=' ');
		requires <- jsGetOptTag (jsTags, 'requires')[-1];
		fn <- chmNewFunction (name, desc, jsdoc$src, requires=requires, global=TRUE);
	    }
	    else {
		stop (sprintf ("Unknown type of Javascript function in file '%s'", filename));
	    }
	}
    }
}

# Define a specified type mapper.
defineMapper <- function (filename, fields) {
    if (!"srctype" %in% names(fields)) {
        stop (sprintf ("srctype not specified in typemap in file '%s'", filename));
    }
    if (!"dsttype" %in% names(fields)) {
        stop (sprintf ("dsttype not specified in typemap in file '%s'", filename));
    }
    srctype <- fields$srctype;
    dsttype <- fields$dsttype;
    if ("field" %in% names(fields)) {
        if ("fieldsep" %in% names(fields)) {
	    fieldsep <- fields$fieldsep;
	}
	else {
	    fieldsep <- ",";
	}
	chmRegisterTypeMapper (strsplit(srctype,",")[[1]], dsttype, 'field', separator=fieldsep, num=fields$field);
    }
    else if ("stringop" %in% names(fields)) {
	if (fields$stringop == "substring(0)") {
	    fn <- "";
	}
	else {
	    fn <- chmStringopFunction (fields$stringop)
	}
	chmRegisterTypeMapper (strsplit(srctype,",")[[1]], dsttype, 'expr', expr=fields$stringop);
    }
    else {
        stop (sprintf ("No known converter for %s --> %s specified in typemap in file '%s'", srctype, dsttype, filename));
    }
}

# Load a text configuration file.
#
loadTextConfig <- function(filename) {
    # Load configuration from text file.
    lines <- NULL;
    try (suppressWarnings(lines <- readLines (filename)), silent=TRUE);
    thismap <- list();
    section <- "";
    fieldsep <- "=";
    for (line in lines) {
	if (grepl ("^ *#", line) || grepl("^ *$", line)) {
	    # Comment.
	}
	else if (grepl ("^\\[", line)) {
	    # Section definition.
	    if (section == "typemap") {
	        defineMapper (filename, thismap);
		thismap <- list();
	    }
	    sectionline <- line;
	    section <- sub ("^\\[(.*)].*$", "\\1", tolower(line));
	    section <- gsub ("  *", " ", spstrip (section));
	}
	else if (section == "typedefs") {
	    # name fieldsep definition
	    parts <- sub (" *#.*$", "", line);
	    parts <- strsplit (parts, fieldsep)[[1]];
	    if (length(parts) != 2) stop (sprintf ('Malformed type definition "%s" in %s: should be name%sdescription', paste(parts,sep=fieldsep), filename, fieldsep));
	    chmRegisterType (spstrip(parts[1]), spstrip(parts[2]));
	}
	else if (section == "servers") {
	    # name fieldsep directory
	    parts <- sub (" *#.*$", "", line);
	    parts <- strsplit (parts, fieldsep)[[1]];
	    if (length(parts) != 2) stop (sprintf ('Malformed server definition "%s" in %s: should be name%sdirectory', paste(parts,sep=fieldsep), filename, fieldsep));
	    chmCreateServer (spstrip(parts[1]), spstrip(parts[2]));
	}
	else if (section == "typemap") {
	    # name fieldsep definition
	    parts <- sub (" *#.*$", "", line);
	    parts <- strsplit (parts, fieldsep)[[1]];
	    if (length(parts) != 2) stop (sprintf ('Malformed typemap line "%s" in %s: should be field%svalue', paste(parts,sep=fieldsep), filename, fieldsep));
	    thismap[[spstrip(parts[1])]] <- spstrip(parts[2]);
	}
	else if (section == "") {
	    stop ("section must be set before definitions");
	}
	else {
	    stop (sprintf ("Unknown section definition '%s' in file '%s'", section, filename));
	}
    }
    if (section == "typemap") {
	defineMapper (filename, thismap);
	thismap <- list();
    }
}

#' Get Javascript function name for accessing a specific string field in each element of string vector.
#'
#' This function returns the name of a Javascript function thats accepts a string vector
#' as its parameter, and for each string in the vector splits the string into fields separated by
#' fieldsep, and accesses field idx (zero origin).  The function returns a vector of these fields.
#'
#' The name of the function returned for a specific fieldsep and idx will be
#' constant within an R session, but may differ between R sessions (or if this
#' library is unloaded and reloaded).
#'
#' @param fieldsep The string that separates fields in the strings of the input vector.
#' @param idx The index (zero origin) of the field the function should extract.
#'
#' @export
#'
#' @seealso [chmGetFunction()]
#' @seealso [chmStringopFunction()]
#'
chmFieldAccessFunction <- function (fieldsep, idx) {
    key <- sprintf ("fa%s%d", fieldsep, idx);
    if (!exists (key, ngchm.env$parseFns)) {
        fnname <- sprintf ("chmFA%x", ngchm.env$nextId);
	ngchm.env$nextId = ngchm.env$nextId + 1;
	if (length(grep("'", fieldsep, fixed=TRUE))>0) {
	   stop (sprintf ('fieldsep "%s" cannot contain any single quotes', fieldsep));
	}
	fieldsep = sprintf ('%s', fieldsep);
	fn <- chmNewFunction (fnname,
		sprintf ("Splits each input string at %s, and returns field %d.", fieldsep, idx),
		paste (sprintf ("function %s (ns) {", fnname),
		       sprintf ("    return ns.map(function(s){return s.split('%s')[%d];});", fieldsep, idx),
		       "}", sep="\n"));
	ngchm.env$parseFns[[key]] <- fn;
    }
    ngchm.env$parseFns[[key]]@name
}

#' Get Javascript function name for performing a specific string operation on each element of a string vector.
#'
#' This function returns the name of a Javascript function thats accepts a string vector
#' as its parameter, and for each string in the vector performs the operation stringop on the string.
#' Stringop must be valid Javascript code that can be appended to a string value.
#' The function returns a vector of the resulting strings.
#'
#' The name of the function returned for a specific stringop will be
#' constant within an R session, but may differ between R sessions (or if this
#' library is unloaded and reloaded).
#'
#' @param stringop A javascript code fragment that can be applied to a string to yield another string.
#'
#' @export
#'
#' @seealso [chmGetFunction()]
#' @seealso [chmFieldAccessFunction()]
#'
chmStringopFunction <- function (stringop) {
    key <- sprintf ("sop%s", stringop);
    if (!exists (key, ngchm.env$parseFns)) {
        fnname <- sprintf ("chmSO%x", ngchm.env$nextId);
	ngchm.env$nextId <- ngchm.env$nextId + 1;
	fn <- chmNewFunction (fnname,
		sprintf ("Transforms each input string by applying stringop '%s'.", stringop),
		paste (sprintf ("function %s (ns) {", fnname),
		       sprintf ("    return ns.map(function(s){return s.%s;});", stringop),
		       "}", sep="\n"));
	ngchm.env$parseFns[[key]] <- fn;
    }
    ngchm.env$parseFns[[key]]@name
}

.onLoad <- function (libname, pkgname) {
    .initNGCHM ();
}

#' Initialization of the NGCHM library.
#'
#' When first loaded the NGCHM library reads configuration files in
#' the configuration path specified by the NGCHMCONFIGPATH environment variable.  The
#' configuration path is a colon (:) separated list of directory names.
#' If not set it defaults to /etc/ngchm:/usr/local/ngchm:/opt/ngchm:$HOME/.ngchm.
#'
#' For each configuration directory in the configuration path, the NGCHM package
#' reads the contents of the configuration files in the conf.d subdirectory in order (as
#' determined by the R sort function).  Other subdirectories are not scanned
#' unless instructed to by an entry in a configuration file.
#'
#' Configuration files may be either text files (.txt extension), R scripts (.R extension),
#' or javascript files (.js extension).
#'
#' @section Text files:
#' A text configuration file consists of one or more sections.  Each section begins with a
#' single line containing the section type enclosed in square brackets. Subsequent lines in
#' the section are either blank or contain a definition of the form "name separator value".
#' The default separator is the equals sign (=).
#'
#' The 'servers' section defines available servers.  The name field defines the name by which
#' the server is known to the library.  The value field specifies a directory containing
#' a specification of the server's properties.  The server specification directory must contain
#' a config.txt that contains lines of the form "name separator value".  The config.txt file
#' must define the value of 'serverProtocol' to be the name of a ngchmServerProtocol.  It
#' must also define the values of any mandatory parameters required by ngchmServerProtocol, and
#' may optionally define any optional parameters.
#'
#' @section R scripts:
#' R scripts are sourced.  They can be used to define local NGCHM related functions.
#'
#' @section Javascript scripts:
#' Javascript files define context specific menu entries.
#'
#' @name NGCHM-initialization
#' @rdname NGCHM-initialization
#' @aliases NGCHM-initialization
#'
#' @examples
#'\dontrun{
#'#/usr/local/ngchm/conf.d/00-servers.txt :
#'[servers]
#'my-server = /usr/local/ngchm/my-server
#'}
#'\dontrun{
#'#/usr/local/ngchm/my-server/config.txt :
#'serverProtocol = manual
#'}
NULL

loadConfigDir <- function (dirname) {
    srcfiles <- NULL;
    try (srcfiles <- dir (dirname, full.names=TRUE), silent=TRUE);
    if (length (srcfiles) > 0) {
	for (src in sort(srcfiles)) {
	    if (grepl ("\\.[rR]$", src)) {
		tryCatch (source (src), error=function(e) stop (sprintf ("while processing R source file '%s'\n", src), e));
	    }
	    else if (grepl ("\\.txt$", src)) {
		tryCatch (loadTextConfig (src), error=function(e) stop (sprintf ("while processing text configuration file '%s'\n", src), e));
	    }
	    else if (grepl ("\\.js$", src)) {
		tryCatch (loadJavascript (src), error=function(e) stop (sprintf ("while processing Javascript file '%s'\n", src), e));
	    }
	    else if (grepl ("\\.d$", src)) {
		tryCatch (loadConfigDir (src), error=function(e) stop (sprintf ("while processing configuration directory '%s'\n", src), e));
	    }
	    else {
		warning (sprintf ("Unknown kind of module file '%s', ignored.", src));
	    }
	}
    }
}

.onAttach <- function (libname, pkgname) {
    getConfigDirs ();

    # Check if suggested system applications are available.
    for (program in c("git","java","tar","scp","ssh")) testExternalProgram(program);

    chmNewFunction ("", "Simple reference", "");
    chmNewFunction ("getLabelValue",
        "This returns the label at the specified index as a list of values.  Can be used whenever the label itself is of the correct type.",
	paste ("function getLabelValue (axis, idx) {",
	       "    return [axis.labels.getLabel (idx)];",
	       "};", sep="\n"));

    # Load module definitions.
    for (cfgdir in c (system.file ("base.config", package="NGCHM"), ngchm.env$configdirs)) {
        loadConfigDir (file.path (cfgdir, "conf.d"));
    }

}
