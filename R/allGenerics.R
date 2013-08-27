
#' Get the name of a NGCHM.
#'
#' This function returns the name of a Next Generation Clustered Heat Map (NGCHM) object.
#'
#' @param chm The CHM for which the name is required.
#'
#' @return A string.
#'
#' @exportMethod chmName
#' @rdname chmName-method
#'
#' @seealso ngchm-class
#' 
setGeneric ("chmName", function(chm) standardGeneric("chmName"));

#' Set the row order of data shown in a NGCHM.
#'
#' Set the order in which rows in the data will be shown in a Next Generation Clustered Heat Map.
#'
#' @param chm The chm for which to set the row order.
#' @param value An ordering of the row labels.
#' If value is NULL, the labels will be displayed in the same order they are found in the first data layer.
#' If value is a character vector, the labels will be displayed in that order.
#' If value is a dendrogram, the labels displayed in the order they occur in a depth first traversal of the tree.
#'
#' @exportMethod chmRowOrder<-
#' @rdname chmRowOrder-method
#'
#' @seealso "chmColOrder<-"
#'
setGeneric ("chmRowOrder<-", function(chm,value) standardGeneric("chmRowOrder<-"));

#' Set the column order of data shown in a NGCHM.
#'
#' Set the order in which columns in the data will be shown in a Next Generation Clustered Heat Map.
#'
#' @param chm The chm for which to set the column order.
#' @param value An ordering of the column labels.
#' If value is NULL, the labels will be displayed in the same order they are found in the first data layer.
#' If value is a character vector, the labels will be displayed in that order.
#' If value is a dendrogram, the labels displayed in the order they occur in a depth first traversal of the tree.
#'
#' @exportMethod chmColOrder<-
#' @rdname chmColOrder-method
#'
#' @seealso "chmRowOrder<-"
#'
setGeneric ("chmColOrder<-", function(chm,value) standardGeneric("chmColOrder<-"));

#' Set the row meta data.
#'
#' Set the meta data attached to rows in a Next Generation Clustered Heat Map.
#'
#' @param chm The chm for which to set the meta data.
#' @param value A character vector or NULL.
#' If value is NULL, no meta data is associated with the NGCHM's rows.
#' If value is a character vector, elements of the vector will be attached as meta data to to NGCHM row of the same name.
#'
#' @exportMethod chmRowMeta<-
#' @rdname chmRowMeta-method
#'
#' @seealso "chmColMeta<-"
#'
setGeneric ("chmRowMeta<-", function(chm,value) standardGeneric("chmRowMeta<-"));

#' Set the column meta data.
#'
#' Set the meta data attached to columns in a Next Generation Clustered Heat Map.
#'
#' @param chm The chm for which to set the meta data.
#' @param value A character vector or NULL
#' If value is NULL, no meta data is associated with the NGCHM's columns.
#' If value is a character vector, elements of the vector will be attached as meta data to to NGCHM column of the same name.
#'
#' @exportMethod chmColMeta<-
#' @rdname chmColMeta-method
#'
#' @seealso "chmRowMeta<-"
#'
setGeneric ("chmColMeta<-", function(chm,value) standardGeneric("chmColMeta<-"));

#' Add a Layer to a NGCHM.
#'
#' Add a Layer to a Next Generation Clustered Heat Map (NGCHM) and return the extended CHM.  A CHM
#' requires at least one Layer.  The first layer added to a NGCHM becomes the primary layer.
#' The second layer added to a NGCHM, if any, becomes the secondary (flicker) layer. Currently
#' at most two layers can be added to a NGCHM.
#'
#' @param chm The chm to add the layer to.
#' @param layer The layer to add to the chm.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddLayer
#' @rdname chmAddLayer-method
#'
#' @seealso chmNewDataLayer
#' @seealso ngchmLayer-class
setGeneric ("chmAddLayer", function(chm,layer) standardGeneric("chmAddLayer"));

#' Add an auxiliary dataset to a NGCHM.
#'
#' Add an auxiliary dataset to a Next Generation Clustered Heat Map (NGCHM) and return the extended CHM.
#' The auxiliary dataset will be stored with the NGCHM and be available in whole or in part from the
#' same server, for use, for example, in custom Javascript functions. 
#' Do not confuse this function with the one for adding an active data layer to the heatmap itself. For
#' that, please refer to the function chmAddLayer.
#'
#' @param chm The chm to add the dataset to.
#' @param dataset The dataset to add to the chm.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddDataset
#' @rdname chmAddDataset-method
#'
#' @seealso chmNewDataset
#' @seealso ngchmDataset-class
setGeneric ("chmAddDataset", function(chm,dataset) standardGeneric("chmAddDataset"));

#' Add tags to a NGCHM.
#'
#' Add one or more tags to a Next Generation Clustered Heat Map (NGCHM) and return the extended CHM.
#'
#' @param chm The chm to add the dataset to.
#' @param tag The tag(s) to add to the chm.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddTag
#' @rdname chmAddTag-method
setGeneric ("chmAddTag", function(chm,tag) standardGeneric("chmAddTag"));

#' Add a colormap to a NGCHM.
#'
#' Add a colormap to a Next Generation Clustered Heat Map (NGCHM) and return the extended CHM.
#' Duplicate colormaps will be silently dropped.
#'
#' Note that it is not necessary to explicitly add colormaps included with data layers or classification
#' bars.  These will be included automatically.  Explicitly using this function is only required in
#' order to add additional predefined, but unused colormaps to the NGCHM.
#'
#' @param chm The chm to add the colormap to.
#' @param colormap The colormap to add to the chm.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddColormap
#' @rdname chmAddColormap-method
#'
#' @seealso chmNewColormap
#' @seealso ngchmColormap-class
setGeneric ("chmAddColormap", function(chm,colormap) standardGeneric("chmAddColormap"));

#' Add a group of related links to the NGCHM.
#'
#' @param chm The chm to add the related link group to.
#' @param name The name of the group of links.
#' @param header The header that should be displayed for this group of links.
#' @param linktype Type of link belonging to this group.
#' @param blurb An optional descriptive paragraph to include between the group header and the group links.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddRelatedGroup
#' @rdname chmAddRelatedGroup-method
setGeneric ("chmAddRelatedGroup", function(chm, name, header, linktype, blurb) standardGeneric("chmAddRelatedGroup"));

#' Add a link to related information to the NGCHM.
#'
#' @param chm The chm to add the related link to.
#' @param group The name of the group this link belongs to.
#' @param link The link to include. Should be either an absolute URL, or a NGCHM name on the same server.
#' @param description A string describing the referenced link and its relationship to the current NGCHM.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddRelated
#' @rdname chmAddRelated-method
setGeneric ("chmAddRelated", function(chm, group, link, description) standardGeneric("chmAddRelated"));

#' Generate an overview image of the NGCHM when making it.
#'
#' Generate an overview image of the NGCHM when making it.  By default, the system generates two default overview
#' images: a PDF image and a PNG image.  The first two overviews you add to the NGCHM will override the default settings
#' for these overviews.  If only one of width or height is specified, the other is calculate based on the aspect ratio of
#' the map.
#'
#' @param chm The chm to add the overview to.
#' @param format The format of the overview (either 'pdf' or 'png').
#' @param width The width of the overview.
#' @param height A height of the overview.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddOverview
#' @rdname chmAddOverview-method
setGeneric ("chmAddOverview", function(chm, format, width, height) standardGeneric("chmAddOverview"));

#' Add a file template to the NGCHM.
#'
#' @param chm The chm to add the file template to.
#' @param source.path A string giving the path to the template.
#' @param dest.path A string giving the relative path where to store the template in the generated CHM.
#' @param substitutions A list (may be empty) and substitutions to make in the template.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddRelated
#' @rdname chmAddRelated-method
setGeneric ("chmAddTemplate", function(chm, source.path, dest.path, substitutions) standardGeneric("chmAddTemplate"));

#' Add custom CSS to a NGCHM.
#'
#' Add custom Cascading Style Sheet (CSS) to a Next Generation Clustered Heat Map (NGCHM) and
#' return the extended CHM.
#'
#' @param chm The chm to add the CSS to.
#' @param css The css selector and style information.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddCSS
#' @rdname chmAddCSS-method
#'
#' @seealso ngchmCSS-class
setGeneric ("chmAddCSS", function(chm,css) standardGeneric("chmAddCSS"));

#' Add custom property to a NGCHM.
#'
#' Add custom property to a Next Generation Clustered Heat Map (NGCHM) and
#' return the extended CHM.
#'
#' @param chm The chm to add the property to.
#' @param label The property label.
#' @param value The property value.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddProperty
#' @rdname chmAddProperty-method
#'
#' @seealso ngchmProperty-class
setGeneric ("chmAddProperty", function(chm,label,value) standardGeneric("chmAddProperty"));

#' Add a menu entry to a NGCHM.
#'
#' Add a popup menu entry to a Next Generation Clustered Heat Map (NGCHM) and
#' return the extended CHM.
#'
#' @param chm The chm to add the menu entry to.
#' @param where The chm menu(s) to add the menu entry to. Must be one of "row", "column", "both", or "element".
#' @param label The label to display in the menu entry.
#' @param func The javascript function to invoke when the menu entry is selected.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddMenuItem
#' @rdname chmAddMenuItem-method
#'
#' @seealso ngchmMenuItem-class
setGeneric ("chmAddMenuItem", function(chm,where,label,func) standardGeneric("chmAddMenuItem"));

#' Add an axis type to a NGCHM.
#'
#' Adds an axis type to a Next Generation Clustered Heat Map (NGCHM) and
#' returns the extended CHM.  Multiple axis types may be added to either axis.
#' When the NGCHM is made, any Axis functions matching the specified axis type will be
#' automatically added to the appropriate axis menu, and any Matrix functions matching
#' the types of the rows and columns will be automatically added to the matrix menu.
#'
#' @param chm The chm to add the axis type to.
#' @param where The axis to add the axis type to. Must be either "row" or "column".
#' @param type The type to add to the specified axis.
#' @param func A javascript function that gets values of that type from the current selection.
#'             If a string is provided, the function is obtained by calling chmGetFunction.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddAxisType
#' @rdname chmAddAxisType-method
#'
#' @seealso chmListTypes
#' @seealso chmRegisterAxisFunction
#' @seealso chmRegisterMatrixFunction
#' @seealso chmRegisterTypeMapper
#' @seealso ngchmAxisType-class
setGeneric ("chmAddAxisType", function (chm, where, type, func) standardGeneric("chmAddAxisType"));

#' Add a classification bar to a NGCHM.
#'
#' Add a classification bar to a Next Generation Clustered Heat Map (NGCHM) and
#' return the extended CHM.
#'
#' @param chm The chm to add the classification bar to.
#' @param where The chm axis(axes) to add the classification bar to. Must be one of "row", "column", or "both".
#' @param bar The classification bar to add to the chm.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddClassBar
#' @rdname chmAddClassBar-method
#'
#' @seealso chmNewClassBar
#' @seealso ngchmBar-class
setGeneric ("chmAddClassBar", function(chm,where,bar) standardGeneric("chmAddClassBar"));


#' Get the name of a NGCHM server.
#'
#' Return the name of a Next Generation Clustered Heat Map (NGCHM) server.
#'
#' @param server The server whose name is required.
#'
#' @return The name of the server.
#'
#' @exportMethod chmDeployServer
#' @rdname chmDeployServer-method
#'
#' @seealso ngchmServer-class
setGeneric ("chmDeployServer", function(server) standardGeneric("chmDeployServer"));

#' Get the deploy directory of a NGCHM server.
#'
#' Return the name of the directory on a Next Generation Clustered Heat Map (NGCHM) server into
#' which a NGCHM must be copied for installation.
#'
#' @param server The server whose deploy directory is required.
#'
#' @return The path on the server to the deploy directory.
#'
#' @exportMethod chmDeployDir
#' @rdname chmDeployDir-method
#'
#' @seealso ngchmServer-class
setGeneric ("chmDeployDir", function(server) standardGeneric("chmDeployDir"));

#' Get the base URL for a NGCHM installed on a NGCHM server.
#'
#' Return the base URL of a Next Generation Clustered Heat Map (NGCHM) that
#' has been installed on a NGCHM server.
#'
#' @param server The server whose base URL is required.
#'
#' @return The base URL for accessing NGCHMs installed on the server.
#'
#' @exportMethod chmUrlBase
#' @rdname chmUrlBase-method
#'
#' @seealso ngchmServer-class
setGeneric ("chmUrlBase", function(server) standardGeneric("chmUrlBase"));

#' Compile a NGCHM.
#'
#' Compile the specified Next Generation Clustered Heat Map (NGCHM) in preparation for installation
#' on the specified server.
#'
#' @exportMethod chmMake
#' @rdname chmMake-method
#'
#' @param server The server for which to compile the NGCHM.
#' @param chm The NGCHM to compile.
#' @param deleteOld If TRUE, delete any old CHM of this name before beginning build. (Default is TRUE.)
#' @param useJAR If defined, the location (filename) of the chmbuilder jar file. The package will not download
#'        a current jar file from the server. It is the caller's responsibility to ensure the builder jar file
#'        is compatible with the server on which the NGCHM will be installed. (Default is not defined.)
#'
#' @seealso ngchmServer-class
#' @seealso ngchm-class
#' @seealso chmInstall
setGeneric ("chmMake", signature=c("server","chm"), function(server,chm,...) standardGeneric("chmMake"));

#' Install a compiled NGCHM on a server.
#'
#' Install the specified Next Generation Clustered Heat Map (NGCHM) on the specified server.  The installed NGCHM
#' is public by default.
#'
#' @exportMethod chmInstall
#' @rdname chmInstall-method
#'
#' @param server The server on which to install the NGCHM.
#' @param chm The NGCHM to install.
#'
#' @seealso ngchmServer-class
#' @seealso ngchm-class
#' @seealso chmUninstall
#' @seealso chmMakePrivate
#' @seealso chmMakePublic
setGeneric ("chmInstall", function(server,chm) standardGeneric("chmInstall"));

#' Remove an installed NGCHM.
#'
#' Remove the specified Next Generation Clustered Heat Map (NGCHM) from the specified server.
#'
#' @exportMethod chmUninstall
#' @rdname chmUninstall-method
#'
#' @param server The server on which the NGCHM to remove is installed.
#' @param chm The NGCHM to remove.
#'
#' @seealso ngchmServer-class
#' @seealso ngchm-class
#' @seealso chmInstall
setGeneric ("chmUninstall", function(server,chm) standardGeneric("chmUninstall"));

#' Get the URL for an installed NGCHM.
#'
#' Return the URL for accessing the specified Next Generation Clustered Heat Map (NGCHM) on the specified server.
#'
#' @exportMethod chmGetURL
#' @rdname chmGetURL-method
#'
#' @param server The server on which the NGCHM is installed.
#' @param chm The NGCHM for which the URL is required.
#'
#' @seealso ngchmServer-class
#' @seealso ngchm-class
setGeneric ("chmGetURL", function(server,chm) standardGeneric("chmGetURL"));

#' Make an installed NGCHM private.
#'
#' Make private the specified Next Generation Clustered Heat Map (NGCHM) on the specified server.
#'
#' @exportMethod chmMakePrivate
#' @rdname chmMakePrivate-method
#'
#' @param server The server on which the NGCHM to make private is installed.
#' @param chm The NGCHM to make private.
#'
#' @seealso ngchmServer-class
#' @seealso ngchm-class
#' @seealso chmInstall
#' @seealso chmUninstall
#' @seealso chmMakePublic
setGeneric ("chmMakePrivate", function(server,chm) standardGeneric("chmMakePrivate"));

#' Make an installed NGCHM public.
#'
#' Make public the specified Next Generation Clustered Heat Map (NGCHM) on the specified server.
#'
#' @exportMethod chmMakePublic
#' @rdname chmMakePublic-method
#'
#' @param server The server on which the NGCHM to make public is installed.
#' @param chm The NGCHM to make public.
#'
#' @seealso ngchmServer-class
#' @seealso ngchm-class
#' @seealso chmInstall
#' @seealso chmUninstall
#' @seealso chmMakePrivate
setGeneric ("chmMakePublic", function(server,chm) standardGeneric("chmMakePublic"));

