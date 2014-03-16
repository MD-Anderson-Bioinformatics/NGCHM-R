#' Javascript extensions for the Next Generation Clustered Heat Map (NGCHM) Construction Library
#' 
#' Currently:
#' \itemize{
#'   \item Axis function View Ideogram is added for the appropriate axis types.
#' }
#'
#' @seealso chmGetFunction
#' @seealso chmListFunctions
#'
#' @name NGCHM-functions
#' @rdname NGCHM-functions
#' @aliases NGCHM-functions
NULL

.onLoad <- function (libname, pkgname) {
    .initNGCHM ()

    chmCreateServerProtocol ("manual",
			     installMethod = function (server, chm) {
				stop ("NGCHMs cannot be automatically installed on this server. Please obtain installation instructions from the server administrator.");
			     },
			     uninstallMethod = function (server, chm) {
				stop ("NGCHMs cannot be automatically uninstalled from this server. Please obtain instructions from the server administrator.");
			     },
			     makePrivate = function (server, chm) {
				stop ("NGCHMs cannot be automatically be made private on this server. Please obtain instructions from the server administrator.");
			     },
			     makePublic = function (server, chm) {
				stop ("NGCHMs cannot be automatically be made public on this server. Please obtain instructions from the server administrator.");
			     });

     removeCHMfromServer <- function (server, chmname) {
	# Uninstall the installed CHM, if any, at server@serverName:server@deployDir/chmname
	# We create the delete.txt special file, wait for the server to do its thing, then remove any leftover trash.
	if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
	    system (sprintf ("/bin/find %s/%s -type d -exec /bin/chmod g+s '{}' ';'", shQuote(chmDeployDir(server)), shQuote(chmname)));
	    system (sprintf ("/bin/sh -c \"'(cd %s; touch %s/delete.txt; while [ -O %s/delete.txt ] ; do sleep 1; done; sleep 2; /bin/rm -rf %s; while [ -d %s ] ; do sleep 1; done)'\"",
			     shQuote (chmDeployDir(server)),
			     shQuote (chmname),
			     shQuote (chmname),
			     shQuote (chmname),
			     shQuote (chmname),
			     shQuote (chmname)));
	} else {
	    system (sprintf ("ssh %s /bin/find %s/%s -type d -exec /bin/chmod g+s '{}' '\\;'", getServerDest (server), shQuote(chmDeployDir(server)), shQuote(chmname)));
	    system (sprintf ("ssh %s /bin/sh -c \"'(cd %s; touch %s/delete.txt; while [ -O %s/delete.txt ] ; do sleep 1; done; sleep 2; /bin/rm -rf %s; while [ -d %s ] ; do sleep 1; done)'\"",
			     getServerDest (server),
			     shQuote (chmDeployDir(server)),
			     shQuote (chmname),
			     shQuote (chmname),
			     shQuote (chmname),
			     shQuote (chmname),
			     shQuote (chmname)));
	}
     };

    copyCHMtoServer <- function (server, chm) {
	# Copy the compiled CHM in outDir/chmName to username@deployServerName:deployDir/chmName
	# Special care is needed to get the permissions correct.
	removeCHMfromServer (server, chm@name);
	if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
	    systemCheck (sprintf ("/bin/mkdir %s/%s", shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	    systemCheck (sprintf ("/bin/chmod g+s %s/%s", shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	    systemCheck (sprintf ("/bin/cp -r %s/%s/* %s/%s/", shQuote(chm@outDir), shQuote(chmName(chm)), shQuote (chmDeployDir(server)), shQuote(chmName(chm))));
	    systemCheck (sprintf ("/bin/chmod -R go+rwx %s/%s", shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	} else {
	    dest <- getServerDest (server);
	    systemCheck (sprintf ("ssh %s /bin/mkdir %s/%s", dest, shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	    systemCheck (sprintf ("ssh %s /bin/chmod g+s %s/%s", dest, shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	    systemCheck (sprintf ("scp -r %s/%s/* %s:%s/%s/", shQuote(chm@outDir), shQuote(chmName(chm)), dest, shQuote (chmDeployDir(server)), shQuote(chmName(chm))));
	    systemCheck (sprintf ("ssh %s /bin/chmod -R go+rwx %s/%s", dest, shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	}
    };


    chmCreateServerProtocol ("copy",
			     installMethod = copyCHMtoServer,
			     uninstallMethod = removeCHMfromServer,
			     makePrivate = function (server, chmname) {
				if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
				    system (sprintf ("/bin/sh -c \"'(cd %s; touch %s/hidden.txt)'\"",
						     shQuote (chmDeployDir(server)),
						     shQuote (chmname)));
				} else {
				    system (sprintf ("ssh %s /bin/sh -c \"'(cd %s; touch %s/hidden.txt)'\"",
						     getServerDest (server),
						     shQuote (chmDeployDir(server)),
						     shQuote (chmname)));
				}
			     },
			     makePublic = function (server, chmname) {
				if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
				    system (sprintf ("/bin/sh -c \"'(cd %s; /bin/rm %s/hidden.txt)'\"",
						     shQuote (chmDeployDir(server)),
						     shQuote (chmname)));
				} else {
				    system (sprintf ("ssh %s /bin/sh -c \"'(cd %s; /bin/rm %s/hidden.txt)'\"",
						     getServerDest (server),
						     shQuote (chmDeployDir(server)),
						     shQuote (chmname)));
				}
			     });

    chmCreateServerProtocol ("mds",
			     installMethod = function (server, chm) {
				# Install the NGCHM using the MDS system.
				#
				# MDS instructions from Tod (email 09/25/2013):
				# - Copy (cp) the directory you want moved into the working directory into the "STAGE" directory.
				# - Once the copying is done, move (mv) the directory into the "ADD-DIR" directory.
				# - Create (touch) an "add.mds" file in the directory you just moved.
				# - When the MDS cron job runs, the directory will be moved to the working directory.
				# - If there is already an existing directory with the same name in the working directory, the old directory will be deleted.

				# tar lives in different places on different systems - so no absolute path
				tarballName <- sprintf ("%s.ngchm.gz", chm@name);
				thisHost <- system("/bin/hostname -f", intern=TRUE);
				ddir <- chmDeployDir (server);
				if (server@deployServer == thisHost) {
				    systemCheck (sprintf ("tar xfC %s %s/STAGE", shQuote(tarballName), shQuote(ddir)));
				    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/add.mds", shQuote(ddir), shQuote(chm@name)));
				    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/ADD-DIR/", shQuote(ddir), shQuote(chm@name), shQuote(ddir)));
				} else {
				    dest <- getServerDest (server);
				    systemCheck (sprintf ("scp %s %s:%s/STAGE/", shQuote(tarballName), dest, shQuote (chmDeployDir(server))));
				    systemCheck (sprintf ("ssh %s tar xfC %s/STAGE/%s %s/STAGE", dest, shQuote(ddir), shQuote(tarballName), shQuote(ddir)));
				    systemCheck (sprintf ("ssh %s /bin/rm %s/STAGE/%s", dest, shQuote(ddir), shQuote(tarballName)));
				    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/add.mds", dest, shQuote(ddir), shQuote(chm@name)));
				    systemCheck (sprintf ("ssh %s /bin/mv %s/STAGE/%s %s/ADD-DIR/", dest, shQuote(ddir), shQuote(chm@name), shQuote(ddir)));
				}
			     },
			     uninstallMethod = function (server, chmname) {
				# Uninstall the NGCHM using the MDS system.
				#
				# MDS instructions from Tod (email 09/25/2013):
				# - Create a directory with the name of the one you want to delete from the working directory in the "STAGE" directory.
				# - Once the creation is done, move (mv) the directory into the "REMOVE-DIR" directory.
				# - Create (touch) a "remove.mds" file in the directory you just moved.
				# - When the MDS cron job runs, the directory with the corresponding name will be deleted.
				# - Afterward, the directory in the "REMOVE-DIR" directory will be deleted.

				thisHost <- system("/bin/hostname -f", intern=TRUE);
				ddir <- chmDeployDir (server);
				if (server@deployServer == thisHost) {
				    systemCheck (sprintf ("/bin/mkdir %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/remove.mds", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/REMOVE-DIR/", shQuote(ddir), shQuote(chmname), shQuote(ddir)));
				} else {
				    dest <- getServerDest (server);
				    systemCheck (sprintf ("ssh %s /bin/mkdir %s/STAGE/%s", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/remove.mds", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/mv %s/STAGE/%s %s/REMOVE-DIR/", dest, shQuote(ddir), shQuote(chmname), shQuote(ddir)));
				}
			     },
			     makePrivate = function (server, chmname) {
				# Make an NGCHM private (add hidden.txt file) using the MDS system.
				#
				# MDS instructions from Tod (email 09/25/2013):
				# - Create a directory with the files you want added to the working directory in the "STAGE" directory.
				# - Once the copying is done, move (mv) the directory into the "ADD-FILE" directory.
				# - Create (touch) a "add.mds" file in the directory you just moved.
				# - When the MDS cron job runs, the files will be moved to the corresponding directory in the working directory.
				# - Afterward, the directory in the "ADD-FILE" directory will be deleted.
				thisHost <- system("/bin/hostname -f", intern=TRUE);
				ddir <- chmDeployDir (server);
				if (server@deployServer == thisHost) {
				    systemCheck (sprintf ("/bin/mkdir %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/hidden.txt", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/add.mds", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/ADD-FILE/", shQuote(ddir), shQuote(chmname), shQuote(ddir)));
				} else {
				    dest <- getServerDest (server);
				    systemCheck (sprintf ("ssh %s /bin/mkdir %s/STAGE/%s", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/hidden.txt", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/add.mds", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/mv %s/STAGE/%s %s/ADD-FILE/", dest, shQuote(ddir), shQuote(chmname), shQuote(ddir)));
				}
			     },
			     makePublic = function (server, chmname) {
				# Make an NGCHM public (remove hidden.txt file) using the MDS system.
				#
				# MDS instructions from Tod (email 09/25/2013):
				# - Create a directory with the files you want removed from the working directory in the "STAGE" directory.
				# - Files can be zero length and created by touch, only the name matters.
				# - Once the creation is done, move (mv) the directory into the "REMOVE-FILE" directory.
				# - Create (touch) a "remove.mds" file in the directory you just moved.
				# - When the MDS cron job runs, the files in the directory with the corresponding names will be deleted.
				# - Afterward, the directory in the "REMOVE-FILE" directory will be deleted.
				thisHost <- system("/bin/hostname -f", intern=TRUE);
				ddir <- chmDeployDir (server);
				if (server@deployServer == thisHost) {
				    systemCheck (sprintf ("/bin/mkdir %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/hidden.txt", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/remove.mds", shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/REMOVE-FILE/", shQuote(ddir), shQuote(chmname), shQuote(ddir)));
				} else {
				    dest <- getServerDest (server);
				    systemCheck (sprintf ("ssh %s /bin/mkdir %s/STAGE/%s", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/hidden.txt", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/remove.mds", dest, shQuote(ddir), shQuote(chmname)));
				    systemCheck (sprintf ("ssh %s /bin/mv %s/STAGE/%s %s/REMOVE-FILE/", dest, shQuote(ddir), shQuote(chmname), shQuote(ddir)));
				}
			     });

    chmNewFunction ("getLabelValue",
        "This returns the label at the specified index as a list of values.  Can be used whenever the label itself is of the correct type.",
	paste ("function getLabelValue (axis, idx) {",
	       "    return [axis.labels.getLabel (idx)];",
	       "};", sep="\n"));

    chmNewFunction ("openGeneCardPage",
	"Opens the GeneCards page for the first given HUGO gene name.",
	paste ("function openGeneCardPage (names) {",
	       "    var gname = names[0];",
	       "    window.open('http://www.genecards.org/cgi-bin/carddisp.pl?gene=' + gname + '&search=' + gname, 'genecards');",
	       "}", sep="\n"));

    chmRegisterAxisFunction ("bio.gene.hugo", "View Genecard", "openGeneCardPage");

    chmNewFunction ("searchClinicalTrials",
	"Searches ClinicalTrials.gov for the given HUGO gene names.",
	paste ("function searchClinicalTrials (names) {",
	       "    var gname = names.join('+AND+');",
	       "    window.open('http://clinicaltrials.gov/ct2/results?term=' + gname + '&Search=' + 'Search', 'clinicaltrials');",
	       "}", sep="\n"));

    chmRegisterAxisFunction ("bio.gene.hugo", "Search ClinicalTrials.gov for all", "searchClinicalTrials");

    chmNewFunction ("openNCBIGenePage",
	"Opens the NCBI page for the (first) given HUGO gene name.",
	paste ("function openNCBIGenePage (names) {",
	       "    var gname = names[0];",
	       "    window.open('http://www.ncbi.nlm.nih.gov/gene?term=(homo%20sapiens%5BOrganism%5D)%20AND%20' + gname + '%5BGene%20Name%5D', 'NCBI');",
	       "}", sep="\n"));

    chmRegisterAxisFunction ("bio.gene.hugo", "View NCBI Gene", "openNCBIGenePage");

    chmNewFunction ("openNCBIEntrezIDPage",
	"Opens the NCBI page for the (first) given gene Entrez ID",
	paste ("function openNCBIEntrezIDPage (eids) {",
	       "    var gid = eids[0];",
	       "    window.open('http://www.ncbi.nlm.nih.gov/gene/' + gid, 'NCBI');",
	       "}", sep="\n"));

    chmRegisterAxisFunction ("bio.gene.entrezid", "View NCBI Entrez ID", "openNCBIEntrezIDPage");

    chmNewFunction ("viewMiRBasePage",
	"Opens the miRBase page for the (first) given miRNA ID",
	paste ("function viewMiRBasePage (eids) {",
	       "    var gid = eids[0];",
	       "    window.open('http://www.mirbase.org/cgi-bin/query.pl?terms=' + gid, 'miRBase');",
	       "}", sep="\n"));

    chmRegisterAxisFunction ("bio.mirna", "View miRBase page", "viewMiRBasePage");

    chmNewFunction ("viewGenesetIdeogramG",
	"For axis type 'bio.gene.hugo', this function shows the genomic locations of the selected genes.",
	paste ("function viewGenesetIdeogramG (genes) {",
	       "  genes = genes.sort().filter(function(el,i,a){return i==a.indexOf(el);});",
	       "  var labels = genes.map(function(g){return encodeURIComponent(g);});",
	       "  window.open('http://bioinformatics.mdanderson.org/ideogramviewer/Ideogram.html?genelist1=' + labels.join(','),",
	       "              'ideogram');",
	       "};", sep="\n"));
    chmRegisterAxisFunction ("bio.gene.hugo", "View Ideogram", "viewGenesetIdeogramG");

    chmNewFunction ("viewGenesetIdeogramM",
	"For axis type 'bio.mirna', this function shows the genomic locations of the selected mirnas.",
	paste ("function viewGenesetIdeogramM (mirs) {",
	       "  mirs = mirs.sort().filter(function(el,i,a){return i==a.indexOf(el);});",
	       "  var labels = mirs.map(function(g){return encodeURIComponent(g);});",
	       "  window.open('http://bioinformatics.mdanderson.org/ideogramviewer/Ideogram.html?mirlist1=' + labels.join(','),",
	       "              'ideogram');",
	       "};", sep="\n"));
    chmRegisterAxisFunction ("bio.mirna", "View Ideogram", "viewGenesetIdeogramM");

    chmNewFunction ("searchGoogleScholar",
	"For axis type 'bio.pubmed', this function searches Google Scholar for documents containing the selected terms.",
	paste ("function searchGoogleScholar (terms) {",
	       "  terms = terms.sort().filter(function(el,i,a){return i==a.indexOf(el);});",
	       "  var labels = terms.map(function(g){return encodeURIComponent(g);});",
	       "  window.open('http://scholar.google.com/scholar?q=' + labels.join('+OR+'),",
	       "              'pubmed');",
	       "};", sep="\n"));
    chmRegisterAxisFunction (c("bio.gene.hugo","bio.pubmed","bio.mirna"), "Search in Google Scholar", "searchGoogleScholar");

    chmNewFunction ("searchPubmedAll",
	"For axis type 'bio.pubmed', this function searches Pubmed for documents containing all the selected terms.",
	paste ("function searchPubmedAll (terms) {",
	       "  terms = terms.sort().filter(function(el,i,a){return i==a.indexOf(el);});",
	       "  var labels = terms.map(function(g){return encodeURIComponent(g);});",
	       "  window.open('http://www.ncbi.nlm.nih.gov/pubmed/?term=' + labels.join('+AND+'),",
	       "              'pubmed');",
	       "};", sep="\n"));
    chmRegisterAxisFunction ("bio.pubmed", "Search Pubmed for all", "searchPubmedAll");

    chmNewFunction ("searchPubmedAny",
	"For axis type 'bio.pubmed', this function searches Pubmed for documents containing any of the selected terms.",
	paste ("function searchPubmedAny (terms) {",
	       "  terms = terms.sort().filter(function(el,i,a){return i==a.indexOf(el);});",
	       "  var labels = terms.map(function(g){return encodeURIComponent(g);});",
	       "  window.open('http://www.ncbi.nlm.nih.gov/pubmed/?term=' + labels.join('+OR+'),",
	       "              'pubmed');",
	       "};", sep="\n"));
    chmRegisterAxisFunction ("bio.pubmed", "Search Pubmed for any", "searchPubmedAny");

    chmNewFunction ("getBarField0",
	"Splits each input string at vertical bar, and returns the first field.",
	paste ("function getBarField0 (names) {",
	       "    return names.map(function(nm){return nm.split('|')[0];});",
	       "}", sep="\n"));
    chmNewFunction ("getBarField1",
	"Splits each input string at vertical bar, and returns the second field.",
	paste ("function getBarField1 (names) {",
	       "    return names.map(function(nm){return nm.split('|')[1];});",
	       "}", sep="\n"));

    chmNewFunction ("", "Simple reference", "");
    chmRegisterTypeMapper ("bio.gene.hugo.bar.entrezid", "bio.gene.hugo", "getBarField0");
    chmRegisterTypeMapper ("bio.gene.hugo.bar.entrezid", "bio.gene.entrezid", "getBarField1");

    chmRegisterTypeMapper ("bio.meth.infinium.probe.bar.bio.gene.hugo", "bio.meth.infinium.probe", "getBarField0");
    chmRegisterTypeMapper ("bio.meth.infinium.probe.bar.bio.gene.hugo", "bio.gene.hugo", "getBarField1");

    chmRegisterTypeMapper ("bio.gene.hugo", "bio.pubmed", "");
}
