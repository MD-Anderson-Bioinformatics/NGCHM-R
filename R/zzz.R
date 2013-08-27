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

    chmRegisterFunction (chmNewFunction ("getLabelValue",
        "This returns the label at the specified index as a list of values.  Can be used whenever the label itself is of the correct type.",
	paste ("function getLabelValue (axis, idx) {",
	       "    return [axis.labels.getLabel (idx)];",
	       "};", sep="\n"), FALSE));

    chmRegisterFunction (chmNewFunction ("openGeneCardPage",
	"Opens the GeneCards page for the first given HUGO gene name.",
	paste ("function openGeneCardPage (names) {",
	       "    var gname = names[0];",
	       "    window.open('http://www.genecards.org/cgi-bin/carddisp.pl?gene=' + gname + '&search=' + gname, 'genecards');",
	       "}", sep="\n"), FALSE));

    chmRegisterAxisFunction ("bio.gene.hugo", "View Genecard", "openGeneCardPage");

    chmRegisterFunction (chmNewFunction ("openNCBIGenePage",
	"Opens the NCBI page for the (first) given HUGO gene name.",
	paste ("function openNCBIGenePage (names) {",
	       "    var gname = names[0];",
	       "    window.open('http://www.ncbi.nlm.nih.gov/gene?term=(homo%20sapiens%5BOrganism%5D)%20AND%20' + gname + '%5BGene%20Name%5D', 'NCBI');",
	       "}", sep="\n"), FALSE));

    chmRegisterAxisFunction ("bio.gene.hugo", "View NCBI Gene", "openNCBIGenePage");

    chmRegisterFunction (chmNewFunction ("openNCBIEntrezIDPage",
	"Opens the NCBI page for the (first) given gene Entrez ID",
	paste ("function openNCBIEntrezIDPage (eids) {",
	       "    var gid = eids[0];",
	       "    window.open('http://www.ncbi.nlm.nih.gov/gene/' + gid, 'NCBI');",
	       "}", sep="\n"), FALSE));

    chmRegisterAxisFunction ("bio.gene.entrezid", "View NCBI Entrez ID", "openNCBIEntrezIDPage");

    chmRegisterFunction (chmNewFunction ("viewMiRBasePage",
	"Opens the miRBase page for the (first) given miRNA ID",
	paste ("function viewMiRBasePage (eids) {",
	       "    var gid = eids[0];",
	       "    window.open('http://www.mirbase.org/cgi-bin/query.pl?terms=' + gid, 'miRBase');",
	       "}", sep="\n"), FALSE));

    chmRegisterAxisFunction ("bio.mirna", "View miRBase page", "viewMiRBasePage");

    chmRegisterFunction (chmNewFunction ("viewGenesetIdeogram",
	"For axis type 'bio.gene.hugo', this function shows the genomic locations of the selected genes.",
	paste ("function viewGenesetIdeogram (genes) {",
	       "  genes = genes.sort().filter(function(el,i,a){return i==a.indexOf(el);});",
	       "  var labels = genes.map(function(g){return encodeURIComponent(g);});",
	       "  window.open('http://bioinformatics.mdanderson.org/ideogramviewer/Ideogram.html?genelist=' + labels.join(','),",
	       "              'ideogram');",
	       "};", sep="\n"), FALSE));
    chmRegisterAxisFunction ("bio.gene.hugo", "View Ideogram", "viewGenesetIdeogram");

    chmRegisterFunction (chmNewFunction ("getBarField0",
	"Splits each input string at vertical bar, and returns the first field.",
	paste ("function getBarField0 (names) {",
	       "    return names.map(function(nm){return nm.split('|')[0];});",
	       "}", sep="\n"), FALSE));
    chmRegisterFunction (chmNewFunction ("getBarField1",
	"Splits each input string at vertical bar, and returns the second field.",
	paste ("function getBarField1 (names) {",
	       "    return names.map(function(nm){return nm.split('|')[1];});",
	       "}", sep="\n"), FALSE));

    chmRegisterTypeMapper ("bio.gene.hugo.bar.entrezid", "bio.gene.hugo", "getBarField0");
    chmRegisterTypeMapper ("bio.gene.hugo.bar.entrezid", "bio.gene.entrezid", "getBarField1");

    chmRegisterTypeMapper ("bio.meth.infinium.probe.bar.bio.gene.hugo", "bio.meth.infinium.probe", "getBarField0");
    chmRegisterTypeMapper ("bio.meth.infinium.probe.bar.bio.gene.hugo", "bio.gene.hugo", "getBarField1");

}
