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
setGeneric("chmName", function(chm) standardGeneric("chmName"))
#' Set the row order of data shown in a NGCHM.
#'
#' This function sets the row order for a NG-CHM (Next-Generation Clustered Heat Map) object.
#'
#' @param chm An object of class 'ngchm'.
#' @param value An object of class 'optDendrogram' or 'file' specifying the new row order.
#' If value is NULL, the labels will be displayed in the same order they are found in the first data layer.
#' If value is a character vector, the labels will be displayed in that order.
#' If value is a dendrogram, the labels displayed in the order they occur in a depth first traversal of the tree.
#'
#' @exportMethod chmRowOrder<-
#' @rdname chmRowOrder-method
#'
#' @return An updated 'ngchm' object with the new row order.
#' @seealso "chmColOrder<-"
#'
setGeneric("chmRowOrder<-", function(chm, value) standardGeneric("chmRowOrder<-"))

#' Set the column order of data shown in a NGCHM.
#'
#' This function sets the column order for a NG-CHM (Next-Generation Clustered Heat Map) object.
#'
#' @param chm An object of class 'ngchm'.
#' @param value An object of class 'optDendrogram' or 'file' specifying the new column order.
#' If value is NULL, the labels will be displayed in the same order they are found in the first data layer.
#' If value is a character vector, the labels will be displayed in that order.
#' If value is a dendrogram, the labels displayed in the order they occur in a depth first traversal of the tree.
#'
#' @exportMethod chmColOrder<-
#' @rdname chmColOrder-method
#'
#' @seealso "chmRowOrder<-"
#'
#' @return An updated 'ngchm' object with the new column order.
setGeneric("chmColOrder<-", function(chm, value) standardGeneric("chmColOrder<-"))
#' Add MetaData to NG-CHM
#'
#' This function adds metadata to a NG-CHM (Next-Generation Clustered Heat Map) object.
#'
#' @param chm An object of class 'ngchm'.
#' @param where A single character string specifying where to add the metadata. Can be "row", "column", or "both".
#' @param type A single character string specifying the type of the metadata.
#' @param value A character vector specifying the values of the metadata.
#' If value is a character vector, elements of the vector will be attached as meta data
#' to to NGCHM row of the same name.
#'
#' @exportMethod chmAddMetaData
#' @rdname chmAddMetaData-method
#'
#' @return An updated 'ngchm' object with the new metadata added.
setGeneric("chmAddMetaData", function(chm, where, type, value) standardGeneric("chmAddMetaData"))
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
setGeneric("chmAdd", function(chm, ...) standardGeneric("chmAdd"))
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
setGeneric("chmAddLayer", function(chm, layer) standardGeneric("chmAddLayer"))
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
setGeneric("chmAddDataset", function(chm, dataset) standardGeneric("chmAddDataset"))
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
setGeneric("chmAddDialog", function(chm, dialog) standardGeneric("chmAddDialog"))
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
setGeneric("chmAddCovariate", function(dataset, where, covariate) standardGeneric("chmAddCovariate"))
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
setGeneric("chmAddTag", function(chm, tag) standardGeneric("chmAddTag"))
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
setGeneric("chmAddColormap", function(chm, colormap) standardGeneric("chmAddColormap"))
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
setGeneric("chmAddRelatedGroup", function(chm, name, header, linktype, blurb) standardGeneric("chmAddRelatedGroup"))
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
setGeneric("chmAddRelated", function(chm, group, link, description) standardGeneric("chmAddRelated"))
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
setGeneric("chmAddOverview", function(chm, format, width, height) standardGeneric("chmAddOverview"))
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
setGeneric("chmAddTemplate", function(chm, source.path, dest.path, substitutions) standardGeneric("chmAddTemplate"))
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
setGeneric("chmAddCSS", function(chm, css) standardGeneric("chmAddCSS"))
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
setGeneric("chmAddProperty", function(chm, label, value) standardGeneric("chmAddProperty"))
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
setGeneric("chmAddMenuItem", function(chm, where, label, func) standardGeneric("chmAddMenuItem"))
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
setGeneric("chmAddAxisType", function(chm, where, type, func) standardGeneric("chmAddAxisType"))
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
setGeneric("chmAddSpecificAxisTypeFunction", function(chm, where, type, label, func) standardGeneric("chmAddSpecificAxisTypeFunction"))
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
#' @param covar The covariate or covariate bar (or a list of them) to add to the chm.
#' @param ... Additional parameters passed to chmNewCovariateBar if covar is a covariate.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddCovariateBar
#' @rdname chmAddCovariateBar-method
#'
#' @seealso [chmNewCovariate()]
#' @seealso [chmNewCovariateBar()]
#' @seealso [ngchmCovariate-class]
setGeneric("chmAddCovariateBar", function(chm, where, covar, ...) standardGeneric("chmAddCovariateBar"))
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
setGeneric("chmDeployServer", function(server) standardGeneric("chmDeployServer"))
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
setGeneric("chmUrlBase", function(server) standardGeneric("chmUrlBase"))
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
setGeneric("chmMake", signature = c("chm"), function(chm, ...) standardGeneric("chmMake"))
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
setGeneric("chmInstall", function(chm, ...) standardGeneric("chmInstall"))

#' Remove an NG-CHM from Server
#'
#' This function removes a specific NG-CHM (Next-Generation Clustered Heat Map) from a specified server.
#'
#' @param chm A single character string specifying the NG-CHM's name, or an object of class "ngchm"
#' representing the NG-CHM to be uninstalled.
#' @param server An object of class 'ngchmServer' or a character string representing the server from
#' which the NG-CHM is to be uninstalled. If not provided, the current server is used.
#' @param ... Additional server (protocol) specific parameters.
#'
#' @return No return value. The function is called for its side effect of uninstalling the
#' specified NG-CHM from the specified server.
#'
#' @exportMethod chmUninstall
#' @rdname chmUninstall-method
#'
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmInstall()]
setGeneric("chmUninstall", function(chm, ...) standardGeneric("chmUninstall"))

#' Get the URL for an installed NGCHM.
#'
#' Return the URL for accessing the specified Next Generation Clustered Heat Map (NGCHM) on the specified server.
#'
#' @param chm A single character string specifying the name of the NG-CHM.
#' @param server A single character string specifying the server where the NG-CHM is hosted. If not provided, the current server is used.
#'
#' @return A character string representing the URL of the specified NG-CHM on the specified server.
#'
#' @exportMethod chmGetURL
#' @rdname chmGetURL-method
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
setGeneric("chmGetURL", function(chm, ...) standardGeneric("chmGetURL"))

#' Make NG-CHM Private on Server
#'
#' This function makes a specific NG-CHM (Next-Generation Clustered Heat Map) private on a specified server.
#'
#' @param server An object of class 'ngchmServer' representing the server where the NG-CHM is hosted.
#' @param chm A single character string specifying the name of the NG-CHM to be made private.
#'
#' @return No return value. The function is called for its side effect of making the specified NG-CHM private on the specified server.
#' @exportMethod chmMakePrivate
#' @rdname chmMakePrivate-method
#'
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmInstall()]
#' @seealso [chmUninstall()]
#' @seealso [chmMakePublic()]
setGeneric("chmMakePrivate", function(server, chm) standardGeneric("chmMakePrivate"))

#' Make NG-CHM Public on Server
#'
#' This function makes a specific NG-CHM (Next-Generation Clustered Heat Map) public on a specified server.
#'
#' @param server An object of class 'ngchmServer' representing the server where the NG-CHM is hosted.
#' @param chm A single character string specifying the name of the NG-CHM to be made public.
#'
#' @return No return value. The function is called for its side effect of making the specified NG-CHM public on the specified server.
#'
#' @exportMethod chmMakePublic
#' @rdname chmMakePublic-method
#'
#' @seealso [ngchmServer-class]
#' @seealso [ngchm-class]
#' @seealso [chmInstall()]
#' @seealso [chmUninstall()]
#' @seealso [chmMakePrivate()]
setGeneric("chmMakePublic", function(server, chm) standardGeneric("chmMakePublic"))

#' Set Access Credentials for NG-CHM Server
#'
#' This function sets the credentials for a specific NG-CHM (Next-Generation Clustered Heat Map) server.
#'
#' @param resource An object of class 'ngchmServer' or a character string representing the server for
#' which the credentials are to be set.
#' @param credentials A single character string specifying the credentials to be set for the server.
#'
#' @return No return value. The function is called for its side effect of setting the credentials for
#' the specified server.
#'
#' @exportMethod chmSetCredentials
#' @rdname chmSetCredentials-method
setGeneric("chmSetCredentials", function(resource, credentials) standardGeneric("chmSetCredentials"))

#' Bind values to an existing JS function.
#'
#' Create a new JS function by binding values to extra parameters of an existing JS function.
#'
#' @exportMethod chmBindFunction
#' @rdname chmBindFunction-method
#'
#' @param name A single character string specifying the name of the function.
#' @param fn An object of class 'ngchmJS' representing the function to be bound.
#' @param bindings A list containing at least one parameter binding.  Each list element
#'        binds one parameter, starting from the first unbound parameter, and the name
#'        of each list element must match the name of the corresponding parameter.
#'
#' @return A new 'ngchmJS' object representing the bound function.
#' @seealso [chmNewFunction()]
setGeneric("chmBindFunction", function(name, fn, bindings) standardGeneric("chmBindFunction"))
#' Add standard toolbox to an NG-CHM axis
#'
#' This function adds a toolbox to a NG-CHM (Next-Generation Clustered Heat Map) axis.
#'
#' @exportMethod chmAddToolboxR
#' @rdname chmAddToolboxR-method
#'
#' @param CHM An object of class 'ngchm'.
#' @param axis A single character string specifying the axis where the toolbox will be added. Can be
#' "row", "column", or "both".
#' @param axistype A single character string specifying the type of the axis.
#' @param datasetname A single character string specifying the name of the dataset.
#' @param idstr string to append to toolbox menu labels (default '')
#'
#' @return An updated 'ngchm' object with the new toolbox added.
setGeneric("chmAddToolboxR", function(CHM, axis, axistype, datasetname, idstr) standardGeneric("chmAddToolboxR"))

#' Add Toolbox R2 to NG-CHM
#'
#' This function adds a toolbox of type R2 to a NG-CHM (Next-Generation Clustered Heat Map) object.
#'
#' @param CHM An object of class 'ngchm'.
#' @param axistype A single character string specifying the type of the axis.
#' @param datasetname A single character string specifying the name of the dataset.
#' @param idstr string to append to toolbox menu labels (default '')
#'
#' @return An updated 'ngchm' object with the new toolbox of type R2 added.
#'
#' @exportMethod chmAddToolboxR2
#' @rdname chmAddToolboxR2-method
setGeneric("chmAddToolboxR2", function(CHM, axistype, datasetname, idstr) standardGeneric("chmAddToolboxR2"))

#' Add Toolbox RC to NG-CHM
#'
#' This function adds a toolbox of type RC to a NG-CHM (Next-Generation Clustered Heat Map) object.
#'
#' @param CHM An object of class 'ngchm'.
#' @param rowtype A single character string specifying the type of the row.
#' @param coltype A single character string specifying the type of the column.
#' @param datasetname A single character string specifying the name of the dataset.
#' @param idstr string to append to toolbox menu labels (default '')
#'
#' @return An updated 'ngchm' object with the new toolbox of type RC added.
#'
#' @exportMethod chmAddToolboxRC
#' @rdname chmAddToolboxRC-method
setGeneric("chmAddToolboxRC", function(CHM, rowtype, coltype, datasetname, idstr) standardGeneric("chmAddToolboxRC"))
#' Load CHM from NG-CHM server
#'
#' Load an R CHM object from an NG-CHM server.  The CHM concerned must have been built
#' using this library, version 0.9.4 or later.
#'
#' @exportMethod chmLoadCHM
#' @rdname chmLoadCHM-method
#'
#' @param serverOrURL An object of class 'ngchmServer' representing the server from which the NG-CHM
#' is to be loaded.
#' @param name A single character string specifying the name of the NG-CHM to be loaded.
#'
#' @return An object of class 'ngchm' representing the loaded NG-CHM.
setGeneric("chmLoadCHM", function(serverOrURL, name) standardGeneric("chmLoadCHM"))

#' Get the dataset from an NG-CHM object
#'
#' This function retrieves the dataset associated with a specific NG-CHM
#' (Next-Generation Clustered Heat Map).
#'
#' @param object An NG-CHM object containing an ngchmDataset
#'
#' @return The dataset associated with the specified object.
#'
#' @exportMethod chmGetDataset
#' @rdname chmGetDataset-method
#'
setGeneric("chmGetDataset", function(object) standardGeneric("chmGetDataset"))

#' Determine if the NG-CHM has the given property.
#'
#' This function checks if a specific property exists in a NG-CHM (Next-Generation Clustered Heat Map) object.
#'
#' @param object An object of class 'ngchmVersion2' representing the NG-CHM to be checked.
#' @param label A single character string or a vector of character strings specifying the label(s) of the property(ies) to be checked.
#'
#' @return A logical value indicating whether the specified property(ies) exist in the 'ngchmVersion2' object. If 'label' is a vector, a logical vector is returned.
#'
#' @exportMethod chmHasProperty
#' @rdname chmHasProperty-method
setGeneric("chmHasProperty", function(object, label) standardGeneric("chmHasProperty"))

#' Get Property from NG-CHM
#'
#' This function retrieves a specific property from a NG-CHM (Next-Generation Clustered Heat Map) object.
#'
#' @param object An object of class 'ngchmVersion2' representing the NG-CHM from which
#' the property is to be retrieved.
#' @param label A single character string specifying the label of the property to be retrieved.
#'
#' @return The property associated with the specified label in the 'ngchmVersion2' object.
#' @exportMethod chmGetProperty
#' @rdname chmGetProperty-method
setGeneric("chmGetProperty", function(object, label) standardGeneric("chmGetProperty"))

#' Set number of characters to display for row or column labels
#'
#' @description
#' Sets the maximum number of characters to display for row or column labels in the NG-CHM viewer.
#'
#' @param chm An ngchmVersion2 object to modify
#' @param displayLength Numeric value to set as display length
#' @param rowOrCol Character string indicating which labels to modify: "row" or "col" ("column" also accepted)
#'
#' @return Modified ngchmVersion2 object with updated display length
#'
#' @examples
#' # Create a new NG-CHM object
#' chm <- chmNew("New Heat Map")
#' # Set row labels to display up to 20 characters
#' chm <- chmSetDisplayLength(chm, 20, "row")
#' # Set column labels to display up to 15 characters
#' chm <- chmSetDisplayLength(chm, 15, "col")
#'
#' @rdname chmSetDisplayLength-method
#' @exportMethod chmSetDisplayLength
setGeneric("chmSetDisplayLength", function(chm, displayLength, rowOrCol) standardGeneric("chmSetDisplayLength"))

#' Get shaid for an object
#'
#' @exportMethod shaidyGetShaid
#' @rdname shaidyGetShaid-method
#'
#' @param object The object, such as a chm, dataset, etc., for which to get the shaid
#'
#' @return The shaid of the object.
#'
setGeneric("shaidyGetShaid", function(object) standardGeneric("shaidyGetShaid"))

#' Get an object's component shaids
#'
#' @exportMethod shaidyGetComponents
#' @rdname shaidyGetComponents-method
#'
#' @param object The object, such as a chm, dataset, etc., for which to get the component shaids
#'
#' @return A list of shaids.
#'
setGeneric("shaidyGetComponents", function(object) standardGeneric("shaidyGetComponents"))
