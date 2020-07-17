
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
#' @seealso [ngchm-class]
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

#' Add meta data.
#'
#' Add meta data to the rows/columns of a Next Generation Clustered Heat Map.
#'
#' @param chm The chm to add the meta data to.
#' @param where The axis of the chm to add the meta data to (row, column, or both).
#' @param type The type of the meta data.
#' @param value A character vector or NULL.
#' If value is a character vector, elements of the vector will be attached as meta data to to NGCHM row of the same name.
#'
#' @exportMethod chmAddMetaData
#' @rdname chmAddMetaData-method
#'
setGeneric ("chmAddMetaData", function(chm,where,type,value) standardGeneric("chmAddMetaData"));

#' Add a list of objects to a NGCHM.
#'
#' Each additional parameter is added to the NGCHM according to its type.  Objects that
#' require additional information (such as an axis) cannot be added using this function.
#' Objects that can be added are layers (including numeric matrices), datasets, and colormaps.
#'
#' @param chm The chm to add the object(s) to.
#' @param ... Zero or more objects to add to the NGCHM.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAdd
#' @rdname chmAdd-method
#'
#' @seealso "chmAddAxisType"
#' @seealso "chmAddColormap"
#' @seealso "chmAddDataset"
#' @seealso "chmAddLayer"
#' @seealso "chmAddMetaData"
setGeneric ("chmAdd", function(chm, ...) standardGeneric("chmAdd"));

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
#' @seealso [chmNewDataLayer()]
#' @seealso [ngchmLayer-class]
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
#' @seealso [chmNewDataset()]
#' @seealso [ngchmDataset-class]
setGeneric ("chmAddDataset", function(chm,dataset) standardGeneric("chmAddDataset"));

#' Add an extra dialog to a NGCHM.
#'
#' Add an extra dialog to a Next Generation Clustered Heat Map (NGCHM) and return the extended CHM.
#'
#' @param chm The chm to add the dialog to.
#' @param dialog The dialog to add to the chm.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddDialog
#' @rdname chmAddDialog-method
#'
#' @seealso [chmNewDialog()]
#' @seealso [ngchmDialog-class]
setGeneric ("chmAddDialog", function(chm,dialog) standardGeneric("chmAddDialog"));

#' Add a covariate to an auxiliary dataset.
#'
#' Add a covariate to an auxiliary dataset and return the extended dataset.
#' Do not confuse this function with the one for adding a covariate bar to an NGCHM. For
#' that, please refer to the function chmAddCovariateBar.
#'
#' @param dataset The dataset to add the covariate to.
#' @param where The dataset axis to add the covariate to. Must be one of "row", "column", or "both".
#' @param covariate The covariate to add to the dataset.
#'
#' @return The extended dataset.
#'
#' @exportMethod chmAddCovariate
#' @rdname chmAddCovariate-method
#'
#' @seealso [chmNewCovariate()]
#' @seealso [ngchmCovariate-class]
setGeneric ("chmAddCovariate", function(dataset,where,covariate) standardGeneric("chmAddCovariate"));

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
#' @seealso [chmNewColorMap()]
#' @seealso [ngchmColormap-class]
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
#' Generate an overview image of the NGCHM when making it.  By default, the system generates no default overview
#' images.  If only one of width or height is specified, the other is calculated based on the aspect ratio of
#' the map.
#'
#' @param chm The chm to add the overview to.
#' @param format The format of the overview ('pdf', 'png', or 'svg').
#' @param width The width of the overview.
#' @param height The height of the overview.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddOverview
#' @rdname chmAddOverview-method
setGeneric ("chmAddOverview", function(chm, format, width, height) standardGeneric("chmAddOverview"));

#' Add a file template to the NGCHM.
#'
#' @param chm The chm to add the file template to.
#' @param source.path A string giving the path to the template, or a function that returns the template content as a string.
#' @param dest.path A string giving the relative path where to store the template in the generated CHM.
#' @param substitutions A list (may be empty) of substitutions to make in the template.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddTemplate
#' @rdname chmAddTemplate-method
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
#' @seealso [ngchmCSS-class]
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
#' @seealso [ngchmProperty-class]
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
#' @seealso [ngchmMenuItem-class]
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
#' @seealso [chmListTypes()]
#' @seealso [chmRegisterAxisFunction()]
#' @seealso [chmRegisterMatrixFunction()]
#' @seealso [chmRegisterTypeMapper()]
#' @seealso [ngchmAxisType-class]
setGeneric ("chmAddAxisType", function (chm, where, type, func) standardGeneric("chmAddAxisType"));

#' Add a CHM-specific axis type function to a NGCHM.
#'
#' Adds a CHM-specific axis type function to a Next Generation Clustered Heat Map (NGCHM) and
#' returns the extended CHM.  Multiple axis type functions may be added to either axis.
#' When the NGCHM is made, any specific Axis functions matching the specified axis type will be
#' automatically added to the appropriate axis menu.
#'
#' @param chm The chm to add the axis type to.
#' @param where The axis to add the axis type to. Must be either "row", "column", or "both".
#' @param type The type expected by the specified function.
#' @param label The label to use if and when the function is added to the menu.
#' @param func A javascript function that accepts a list of values of that type.
#'             If a string is provided, the function is obtained by calling chmGetFunction.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddSpecificAxisTypeFunction
#' @rdname chmAddSpecificAxisTypeFunction-method
#'
#' @seealso [chmListTypes()]
#' @seealso [ngchmAxisType-class]
setGeneric ("chmAddSpecificAxisTypeFunction", function (chm, where, type, label, func) standardGeneric("chmAddSpecificAxisTypeFunction"));

#' Add a covariate bar to a NGCHM.
#'
#' Add a covariate bar to a Next Generation Clustered Heat Map (NGCHM) and
#' return the extended CHM.  If passed a covariate, a covariate bar will be created
#' (using any optional parameters supplied) and added.
#'
#' If a covariate bar with the same name already exists on the specified axis or axes,
#' the existing bar will be replaced by the new bar.
#'
#' @param chm The chm to add the covariate bar to.
#' @param where The chm axis(axes) to add the covariate bar to. Must be one of "row", "column", or "both".
#' @param covar The covariate or covariate bar to add to the chm.
#' @param ... Additional parameters passed to chmNewCovariateBar if covar is a covariate.
#' @param display Whether the bar is displayed ("visible") or not ("hidden"). Defaults to "visible".
#' @param thickness Initial thickness of bar in pixels. Defaults to 10.
#' @param merge Algorithm for merging multiple items into a single pixel. Default.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddCovariateBar
#' @rdname chmAddCovariateBar-method
#'
#' @seealso [chmNewCovariate()]
#' @seealso [chmNewCovariateBar()]
#' @seealso [ngchmCovariate-class]
setGeneric ("chmAddCovariateBar", function(chm,where,covar,...) standardGeneric("chmAddCovariateBar"));

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
#' @seealso [ngchmServer-class]
setGeneric ("chmDeployServer", function(server) standardGeneric("chmDeployServer"));

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
#' @seealso [ngchmServer-class]
setGeneric ("chmUrlBase", function(server) standardGeneric("chmUrlBase"));

#' Compile a NGCHM.
#'
#' Deprecated.  Users should no longer call this method directly.
#'
#' Compiles the specified Next Generation Clustered Heat Map (NGCHM) in preparation for installation.
#'
#' @exportMethod chmMake
#' @rdname chmMake-method
#'
#' @param chm The NGCHM to compile.
#' @param ... Additional chmMake options that depend on the format of the NGCHM.  For details
#'        of the additional parameters of format x see ngchmMakeFormat.x (e.g. ngchmMakeFormat.original).
#'
#' @return The chm
#'
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmNew()]
#' @seealso [chmInstall()]
#' @seealso [ngchmMakeFormat.original()]
setGeneric ("chmMake", signature=c("chm"), function(chm,...) standardGeneric("chmMake"));

#' Add an NG-CHM to an NG-CHM collection.
#'
#' Add the given Next-Generation Clustered Heat Map (NG-CHM) to the specified collection (default: current collection).
#'
#' @exportMethod chmInstall
#' @rdname chmInstall-method
#'
#' @param chm The NGCHM to install.
#' @param ... Additional server (protocol) specific parameters.
#' @param path The path to the collection in which to install the NGCHM.
#' @return The installed chm.
#'
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmUninstall()]
#' @seealso [chmMakePrivate()]
#' @seealso [chmMakePublic()]
setGeneric ("chmInstall", function(chm,...) standardGeneric("chmInstall"));

#' Remove an installed NGCHM.
#'
#' Remove the specified Next Generation Clustered Heat Map (NGCHM) from the specified server.
#'
#' @exportMethod chmUninstall
#' @rdname chmUninstall-method
#'
#' @param server The server on which the NGCHM to remove is installed.
#' @param chm The NGCHM to remove.
#' @param ... Additional server (protocol) specific parameters.
#'
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmInstall()]
setGeneric ("chmUninstall", function(chm,...) standardGeneric("chmUninstall"));

#' Get the URL for an installed NGCHM.
#'
#' Return the URL for accessing the specified Next Generation Clustered Heat Map (NGCHM) on the specified server.
#'
#' @exportMethod chmGetURL
#' @rdname chmGetURL-method
#'
#' @param chm The NGCHM for which the URL is required.
#'
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
setGeneric ("chmGetURL", function(chm,...) standardGeneric("chmGetURL"));

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
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmInstall()]
#' @seealso [chmUninstall()]
#' @seealso [chmMakePublic()]
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
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmInstall()]
#' @seealso [chmUninstall()]
#' @seealso [chmMakePrivate()]
setGeneric ("chmMakePublic", function(server,chm) standardGeneric("chmMakePublic"));

#' Set access credentials.
#'
#' Set access credentials for the specific resource.
#'
#' @exportMethod chmSetCredentials
#' @rdname chmSetCredentials-method
#'
#' @param resource The resource (e.g. ngchmServer) to which the credentials apply.
#' @param credentials The credentials.
setGeneric( "chmSetCredentials", function(resource,credentials) standardGeneric("chmSetCredentials"));

#' Bind values to an existing JS function.
#'
#' Create a new JS function by binding values to extra parameters of an existing JS function.
#'
#' @exportMethod chmBindFunction
#' @rdname chmBindFunction-method
#'
#' @param name The name to give the new JS function.
#' @param fn An existing JS function with at least one extra parameter.
#' @param bindings A list containing at least one parameter binding.  Each list element
#'        binds one parameter, starting from the first unbound parameter, and the name
#'        of each list element must match the name of the corresponding parameter.
#'
#' @seealso [chmNewFunction()]
setGeneric ("chmBindFunction", function(name,fn,bindings) standardGeneric("chmBindFunction"));

#' Add standard toolbox to an axis
#'
#' Add a standard toolbox to an axis
#'
#' @exportMethod chmAddToolboxR
#' @rdname chmAddToolboxR-method
#'
#' @param CHM CHM to which the toolbox will be added
#' @param axis the axis to which the toolbix will be added
#' @param axistype the type of the axis
#' @param datasetname the name of the dataset to use
#' @param idstr string to append to toolbox menu labels (default '')
setGeneric ("chmAddToolboxR", function(CHM,axis,axistype,datasetname,idstr) standardGeneric("chmAddToolboxR"));

#' Add standard toolbox to an element
#'
#' Add a standard toolbox to an element
#'
#' @exportMethod chmAddToolboxR2
#' @rdname chmAddToolboxR2-method
#'
#' @param CHM CHM to which the toolbox will be added
#' @param axistype the type of both axes
#' @param datasetname the name of the dataset to use
#' @param idstr string to append to toolbox menu labels (default '')
setGeneric ("chmAddToolboxR2", function(CHM,axistype,datasetname,idstr) standardGeneric("chmAddToolboxR2"));

#' Add standard toolbox to an element
#'
#' Add a standard toolbox to an element
#'
#' @exportMethod chmAddToolboxRC
#' @rdname chmAddToolboxRC-method
#'
#' @param CHM CHM to which the toolbox will be added
#' @param rowtype the type of the row axis
#' @param coltype the type of the column axis
#' @param datasetname the name of the dataset to use
#' @param idstr string to append to toolbox menu labels (default '')
setGeneric ("chmAddToolboxRC", function(CHM,rowtype,coltype,datasetname,idstr) standardGeneric("chmAddToolboxRC"));

#' Load CHM from NG-CHM server
#'
#' Load an R CHM object from an NG-CHM server.  The CHM concerned must have been built
#' using this library, version 0.9.4 or later.
#'
#' @exportMethod chmLoadCHM
#' @rdname chmLoadCHM-method
#'
#' @param serverOrURL Either the server or the CHM url from which to load the CHM object.
#' @param name Name of the CHM to load if the first parameter is a server id
setGeneric ("chmLoadCHM", function(serverOrURL,name) standardGeneric("chmLoadCHM"))

#' Get the dataset from an NG-CHM object
#'
#' Get the ngchmDataset object from an NG-CHM object.
#'
#' @exportMethod chmGetDataset
#' @rdname chmGetDataset-method
#'
#' @param object An NG-CHM object containing an ngchmDataset
setGeneric ("chmGetDataset", function(object) standardGeneric("chmGetDataset"))

#' Determine if the NG-CHM has the given property.
#'
#' Determine if the NG-CHM has the given property.
#'
#' @exportMethod chmHasProperty
#' @rdname chmHasProperty-method
#'
#' @param object An NG-CHM object
#' @param label The name of the property to check
setGeneric ("chmHasProperty", function(object,label) standardGeneric("chmHasProperty"))

#' Get a property value from an NG-CHM object
#'
#' Get a property value from an NG-CHM object.
#'
#' @exportMethod chmGetProperty
#' @rdname chmGetProperty-method
#'
#' @param object An NG-CHM object
#' @param label The name of the property to get
setGeneric ("chmGetProperty", function(object,label) standardGeneric("chmGetProperty"))

#' Get shaid for an object
#'
#' @exportMethod shaidyGetShaid
#' @rdname shaidyGetShaid-method
#'
#' @param object The object, such as a chm, dataset, etc., for which to get the shaid
#'
#' @return The shaid of the object.
#'
setGeneric ("shaidyGetShaid", function(object) standardGeneric("shaidyGetShaid"))

#' Get an object's component shaids
#'
#' @exportMethod shaidyGetComponents
#' @rdname shaidyGetComponents-method
#'
#' @param object The object, such as a chm, dataset, etc., for which to get the component shaids
#'
#' @return A list of shaids.
#'
setGeneric ("shaidyGetComponents", function(object) standardGeneric("shaidyGetComponents"))
