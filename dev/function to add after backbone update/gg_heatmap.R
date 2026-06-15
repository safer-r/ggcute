#' @title gg_heatmap
#' @description
#' ggplot2 heatmap with the possibility to overlay a mask, see also: draw : http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization same range; scale : https://stackoverflow.com/questions/44655723/r-ggplot2-heatmap-fixed-scale-color-between-graphs; for ggplot2 specifications, see: https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
#' @param data1 numeric matrix or data frame resulting from the conversion of the numeric matrix by reshape2::melt(). 
#' @param legend.name1 character string of the data1 heatmap scale legend.
#' @param low.color1 character string of the color (i.e., "blue" or "#0000FF") of the lowest scale value.
#' @param mid.color1 same as low.color1 but for the middle scale value. If NULL, the middle color is the default color between low.color1 and high.color1. BEWARE: argument midpoint1 is not ignored, even if mid.color1 is NULL, meaning that the default mid color can still be controled.
#' @param legend.name1 character string of the data1 heatmap scale legend.
#' @param high.color1 same as low.color1 but for the highest scale value.
#' @param limit1 2 numeric values defining the lowest and higest color scale values. If NULL, take the range of data1 values. Warning values are sorted by the function. Thus, use the low.color1 and high.color1 to reverse the scale color if necessary.
#' @param midpoint1 single numeric value defining the value corresponding to the mid.color1 argument. A warning message is returned if midpoint1 does not correspond to the mean of limit1 values, because the color scale is not linear anymore. If NULL, takes the mean of limit1 values. Mean of data1, instead of mean of limit1, can be used here if required.
#' @param data2 binary mask matrix (made of 0 and 1) of same dimension as data1 or a data frame resulting from the conversion of the binary mask matrix by reshape2::melt(). Value 1 of data2 will correspond to color2 argument (value 0 will be NA color), and the opposite if invert2 argument is TRUE (inverted mask).
#' @param color2 color of the 1 values of the binary mask matrix. The 0 values will be color NA.
#' @param alpha2 numeric value (from 0 to 1) of the mask transparency.
#' @param invert2 logical. Invert the mask (1 -> 0 and 0 -> 1)?.
#' @param text.size numeric value of the size of the texts in scale.
#' @param title character string of the graph title.
#' @param title.text.size numeric value of the title size (in points).
#' @param show.scale logical. Show color scale?.
#' @param rotate logical. Rotate the heatmap 90° clockwise?.
#' @param return logical. Return the graph parameters?.
#' @param plot logical. Plot the graphic? If FALSE and return argument is TRUE, graphical parameters and associated warnings are provided without plotting.
#' @param add character string allowing to add more ggplot2 features (dots, lines, themes, etc.). BEWARE: (1) must start with "+" just after the simple or double opening quote (no space, end of line, carriage return, etc., allowed), (2) must finish with ")" just before the simple or double closing quote (no space, end of line, carriage return, etc., allowed) and (3) each function must be preceded by "ggplot2::" (for instance: "ggplot2::coord_flip()). If the character string contains the "ggplot2::theme" string, then internal ggplot2 theme() and theme_classic() functions will be inactivated to be reused by add. BEWARE: handle this argument with caution since added functions can create conflicts with the preexisting internal ggplot2 functions.
#' @param warn.print logical. Print warnings at the end of the execution? No print if no warning messages.
#' @param lib.path absolute path of the required packages, if not in the default folders.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns a heatmap if plot argument is TRUE; a list of the graph info if return argument is TRUE:$data: a list of the graphic info; $axes: a list of the axes info; $warn: the warning messages. Use cat() for proper display. NULL if no warning.
#' @examples
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), title = "GRAPH 1")
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), return = TRUE)
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), legend.name1 = "VALUE", title = "GRAPH 1", text.size = 5, data2 = matrix(rep(c(1,0,0,0), 4), ncol = 4), invert2 = FALSE, return = TRUE)
#' # diagonal matrix
#' gg_heatmap(data1 = matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), ncol = 4))
#' gg_heatmap(data1 = reshape2::melt(matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), ncol = 4)))
#' \dontrun{
#' # Error messages
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), data2 = matrix(rep(c(1,0,0,0), 5), ncol = 5))
#' }
#' gg_heatmap(data1 = matrix(1:16, ncol = 4), data2 = reshape2::melt(matrix(rep(c(1,0,0,0), 4), ncol = 4)))
#' gg_heatmap(data1 = reshape2::melt(matrix(1:16, ncol = 4)), data2 = reshape2::melt(matrix(rep(c(1,0,0,0), 4), ncol = 4)))
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom reshape2 melt
#' @importFrom saferDev arg_check
#' @importFrom saferTool round2
#' @details
#' - NA and Inf values are displayed as grey.
#' - when using limit1 all values out of the range of limit1 are also displayed as grey.
#' @export
# test plot.margin = margin(up.space.mds, right.space.mds, down.space.mds, left.space.mds, "inches") to set the dim of the region plot ?
# if matrix is full of zero (or same value I guess), heatmap is complicate. Test it and error message
gg_heatmap <- function(
        data1, 
        legend.name1 = "", 
        low.color1 = "blue", 
        mid.color1 = "white", 
        high.color1 = "red", 
        limit1 = NULL, 
        midpoint1 = NULL, 
        data2 = NULL, 
        color2 = "black", 
        alpha2 = 0.5, 
        invert2 = FALSE, 
        text.size = 12, 
        title = "", 
        title.text.size = 12, 
        show.scale = TRUE, 
        rotate = FALSE, 
        return = FALSE, 
        plot = TRUE, 
        add = NULL, 
        warn.print = FALSE, 
        lib.path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # data1 = matrix(1:16, ncol = 4) ; legend.name1 = "" ; low.color1 = "blue" ; mid.color1 = "white" ; high.color1 = "red" ; limit1 = NULL ; midpoint1 = NULL ; data2 = matrix(rep(c(1,0,0,0), 4), ncol = 4) ; color2 = "black" ; alpha2 = 0.5 ; invert2 = FALSE ; text.size = 12 ; title = "" ; title.text.size = 12 ; show.scale = TRUE ; rotate = FALSE ; return = FALSE ; plot = TRUE ; add = NULL ; warn.print = TRUE ; lib.path = NULL ;safer_check = TRUE
    # package name
    package.name <- "saferGG"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        saferGG:::.base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
    )
    }
    # end critical operator checking
    # package checking
    # check of lib.path
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <-base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            base::.libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- base::.libPaths()
        }
    }else{
        lib.path <- base::.libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path


     # check of the required function from the required packages
    if(safer_check == TRUE){
        saferGG:::.pack_and_function_check(
        fun = base::c(
            "ggplot2::aes_string",
            "ggplot2::coord_fixed",
            "ggplot2::element_blank",
            "ggplot2::element_text",
            "ggplot2::geom_raster",
            "ggplot2::ggplot",
            "ggplot2::ggplot_build",
            "ggplot2::ggtitle",
            "ggplot2::scale_y_reverse",
            "ggplot2::scale_fill_gradient2",
            "ggplot2::theme",
            "ggplot2::theme_classic",
            "reshape2::melt",
            "saferDev::arg_check",
            "saferTool::round2"
        ),
        lib.path = lib.path,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data1"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()    
    # argument checking
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    if(base::all(base::is.matrix(data1))){
        tempo <- saferDev::arg_check(data = data1, class = "matrix", mode = "numeric", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else if(base::all(base::is.data.frame(data1))){
        tempo <- saferDev::arg_check(data = data1, class = "data.frame", length = 3, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            # structure of reshape2::melt() data frame
            tempo <- saferDev::arg_check(data = data1[, 1], data.name = "COLUMN 1 OF data1 (reshape2::melt() DATA FRAME)", typeof = "integer", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            tempo <- saferDev::arg_check(data = data1[, 2], data.name = "COLUMN 2 OF data1 (reshape2::melt() DATA FRAME)", typeof = "integer", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            tempo <- saferDev::arg_check(data = data1[, 3], data.name = "COLUMN 3 OF data1 (reshape2::melt() DATA FRAME)", mode = "numeric", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        }
    }else{
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data1 ARGUMENT MUST BE A NUMERIC MATRIX OR A DATA FRAME OUTPUT OF THE reshape::melt() FUNCTION")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    tempo <- saferDev::arg_check(data = legend.name1, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = low.color1, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE & ! (base::all(low.color1 %in% grDevices::colors() | base::grepl(pattern = "^#", low.color1)))){ # check that all strings of low.color1 start by #
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nlow.color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    if( ! base::is.null(mid.color1)){
        tempo <- saferDev::arg_check(data = mid.color1, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE & ! (base::all(mid.color1 %in% grDevices::colors() | base::grepl(pattern = "^#", mid.color1)))){ # check that all strings of mid.color1 start by #
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nmid.color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = high.color1, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE & ! (base::all(high.color1 %in% grDevices::colors() | base::grepl(pattern = "^#", high.color1)))){ # check that all strings of high.color1 start by #
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nhigh.color1 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    if( ! base::is.null(limit1)){
        tempo <- saferDev::arg_check(data = limit1, class = "vector", mode = "numeric", length = 2, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE & base::any(limit1 %in% base::c(Inf, -Inf))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nlimit1 ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    if( ! base::is.null(midpoint1)){
        tempo <- saferDev::arg_check(data = midpoint1, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(data2)){
        if(base::all(base::is.matrix(data2))){
            tempo <- saferDev::arg_check(data = data2, class = "matrix", mode = "numeric", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            if(tempo$problem == FALSE & ! base::all(base::unique(data2) %in% base::c(0,1))){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nMATRIX IN data2 MUST BE MADE OF 0 AND 1 ONLY (MASK MATRIX)")
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.matrix(data1)) & ! base::identical(base::dim(data1), base::dim(data2))){ # matrix and matrix
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nMATRIX DIMENSION IN data2 MUST BE IDENTICAL AS MATRIX DIMENSION IN data1. HERE IT IS RESPECTIVELY:\n", base::paste(base::dim(data2), collapse = " "), "\n", base::paste(base::dim(data1), collapse = " "))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.data.frame(data1)) & base::nrow(data1) != base::prod(base::dim(data2))){ # reshape2 and matrix
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDATA FRAME IN data2 MUST HAVE ROW NUMBER EQUAL TO PRODUCT OF DIMENSIONS OF data1 MATRIX. HERE IT IS RESPECTIVELY:\n", base::paste(base::nrow(data1), collapse = " "), "\n", base::paste(base::prod(base::dim(data2)), collapse = " "))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }else if(base::all(base::is.data.frame(data2))){
            tempo <- saferDev::arg_check(data = data2, class = "data.frame", length = 3, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            if(tempo$problem == FALSE){
                # structure of reshape2::melt() data frame
                tempo <- saferDev::arg_check(data = data2[, 1], data.name = "COLUMN 1 OF data2 (reshape2::melt() DATA FRAME)", typeof = "integer", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
                tempo <- saferDev::arg_check(data = data2[, 2], data.name = "COLUMN 2 OF data2 (reshape2::melt() DATA FRAME)", typeof = "integer", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
                tempo <- saferDev::arg_check(data = data2[, 3], data.name = "COLUMN 3 OF data2 (reshape2::melt() DATA FRAME)", mode = "numeric", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
            }
            if(tempo$problem == FALSE & ! base::all(base::unique(data2[, 3]) %in% base::c(0,1))){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHIRD COLUMN OF DATA FRAME IN data2 MUST BE MADE OF 0 AND 1 ONLY (MASK DATA FRAME)")
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.data.frame(data1)) & ! base::identical(base::dim(data1), base::dim(data2))){ # data frame and data frame
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDATA FRAME DIMENSION IN data2 MUST BE IDENTICAL TO DATA FRAME DIMENSION IN data1. HERE IT IS RESPECTIVELY:\n", base::paste(base::dim(data2), collapse = " "), "\n", base::paste(base::dim(data1), collapse = " "))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }else if(tempo$problem == FALSE & base::all(base::is.matrix(data1)) & base::nrow(data2) != base::prod(base::dim(data1))){ # reshape2 and matrix
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDATA FRAME IN data2 MUST HAVE ROW NUMBER EQUAL TO PRODUCT OF DIMENSION OF data1 MATRIX. HERE IT IS RESPECTIVELY:\n", base::paste(base::nrow(data2), collapse = " "), "\n", base::paste(base::prod(base::dim(data1)), collapse = " "))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data2 ARGUMENT MUST BE A NUMERIC MATRIX OR A DATA FRAME OUTPUT OF THE reshape::melt() FUNCTION")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = color2, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE & ! (base::all(color2 %in% grDevices::colors() | base::grepl(pattern = "^#", color2)))){ # check that all strings of color2 start by #
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\ncolor2 ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # AND/OR COLOR NAMES GIVEN BY grDevices::colors()")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    tempo <- saferDev::arg_check(data = alpha2, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = invert2, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = text.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = title, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = title.text.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = show.scale, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = return, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = plot, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(add)){
        tempo <- saferDev::arg_check(data = add, class = "vector", mode = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE & ! base::grepl(pattern = "^\\+", add)){ # check that the add string start by +
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nadd ARGUMENT MUST START WITH \"+\": ", base::paste(base::unique(add), collapse = " "))
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }else if(tempo$problem == FALSE & ! base::grepl(pattern = "ggplot2::", add)){ #
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nadd ARGUMENT MUST CONTAIN \"ggplot2::\" IN FRONT OF EACH GGPLOT2 FUNCTION: ", base::paste(base::unique(add), collapse = " "))
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }else if(tempo$problem == FALSE & ! base::grepl(pattern = ")$", add)){ # check that the add string  finished by )
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nadd ARGUMENT MUST FINISH BY \")\": ", base::paste(base::unique(add), collapse = " "))
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/gmillot/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_saferDev::arg_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "data1", 
        "legend.name1", 
        "low.color1",
        "mid.color1",
        "high.color1",
        # "limit1", # inactivated because can be null 
        # "midpoint1", # inactivated because can be null 
        # "data2", # inactivated because can be null 
        "color2",
        "alpha2",
        "invert2",
        "text.size",
        "title",
        "title.text.size",
        "show.scale",
        "rotate",
        "return",
        "plot",
        # "add",# inactivated because can be null
        "warn.print"
        # "lib.path" # inactivated because can be null 
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    # end other checkings
    # end second round of checking and data preparation
   
    # main code
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    if(base::all(base::is.matrix(data1))){
        data1 <- reshape2::melt(data1) # transform a matrix into a data frame with 2 coordinates columns and the third intensity column
    }
    if(rotate == TRUE){
        data1[, 1] <- base::rev(data1[, 1])
    }
    if(base::is.null(limit1)){
        if(base::any( ! base::is.finite(data1[, 3]))){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") THE data1 ARGUMENT CONTAINS -Inf OR Inf VALUES IN THE THIRD COLUMN, THAT WILL NOT BE CONSIDERED IN THE PLOT RANGE")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
        }
        limit1 <- base::range(data1[, 3], na.rm = TRUE, finite = TRUE) # finite = TRUE removes all the -Inf and Inf except if only this. In that case, whatever the -Inf and/or Inf present, output -Inf;Inf range. Idem with NA only
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") THE limit1 ARGUMENT IS NULL -> RANGE OF data1 ARGUMENT HAS BEEN TAKEN: ", base::paste(saferTool::round2(limit1, safer_check = FALSE), collapse = " "))
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
        if(base::suppressWarnings(base::any(limit1 %in% base::c(Inf, -Inf)))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " COMPUTED LIMIT CONTAINS Inf VALUES, BECAUSE VALUES FROM data1 ARGUMENTS ARE NA OR Inf ONLY")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        limit1 <- base::sort(limit1)
    }
    if(base::is.null(midpoint1)){
        midpoint1 <- base::mean(limit1, na.rm = TRUE)
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") THE midpoint1 ARGUMENT IS NULL -> MEAN OF limit1 ARGUMENT HAS BEEN TAKEN: ", base::paste(saferTool::round2(midpoint1, safer_check = FALSE), collapse = " "))
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }else if(saferTool::round2(midpoint1, 9, safer_check = FALSE) != saferTool::round2(base::mean(limit1), 9, safer_check = FALSE)){
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") THE midpoint1 ARGUMENT (", saferTool::round2(base::mean(midpoint1), 9, safer_check = FALSE), ") DOES NOT CORRESPOND TO THE MEAN OF THE limit1 ARGUMENT (", saferTool::round2(base::mean(limit1), 9, safer_check = FALSE), "). COLOR SCALE IS NOT LINEAR")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }
    if( ! base::is.null(data2)){
        if(base::all(base::is.matrix(data2))){
            data2 <- reshape2::melt(data2) # transform a matrix into a data frame with 2 coordinates columns and the third intensity column
        }
        if(rotate == TRUE){
            data2[, 1] <- base::rev(data2[, 1])
        }
        data2[, 3] <- base::factor(data2[, 3]) # to converte continuous scale into discrete scale
    }
    tempo.gg.name <- "gg.indiv.plot."
    tempo.gg.count <- 0 # to facilitate debugging
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggplot())
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_raster(data = data1, mapping = ggplot2::aes_string(x = base::names(data1)[base::ifelse(rotate == FALSE, 2, 1)], y = base::names(data1)[base::ifelse(rotate == FALSE, 1, 2)], fill = base::names(data1)[3]), show.legend = show.scale)) # show.legend option do not remove the legend, only the aesthetic of the legend (dot, line, etc.)
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_fill_gradient2(low = low.color1, high = high.color1, mid = mid.color1, midpoint = midpoint1, limit = limit1, breaks = base::c(limit1[1], midpoint1, limit1[2]), labels = saferTool::round2(base::c(limit1[1], midpoint1, limit1[2]), safer_check = FALSE), name = legend.name1))
    if( ! base::is.null(data2)){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_raster(data = data2, mapping = ggplot2::aes_string(x = base::names(data2)[base::ifelse(rotate == FALSE, 2, 1)], y = base::names(data2)[base::ifelse(rotate == FALSE, 1, 2)], alpha = base::names(data2)[3]), fill = color2, show.legend = FALSE))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_discrete_manual(aesthetics = "alpha", values = if(invert2 == FALSE){base::c(0, alpha2)}else{base::c(alpha2, 0)}, guide = FALSE))
        # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_raster(data = data2, mapping = ggplot2::aes_string(x = names(data2)[ifelse(rotate == FALSE, 2, 1)], y = names(data2)[ifelse(rotate == FALSE, 1, 2)], group = names(data2)[3]), fill = data2[, 3], alpha = alpha2, show.legend = FALSE)) # BEWARE: this does not work if NA present, because geom_raster() has a tendency to complete empty spaces, and thus, behave differently than geom_tile(). See https://github.com/tidyverse/ggplot2/issues/3025
    }
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::coord_fixed()) # x = y
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_y_reverse())
    base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(title))
    add.check <- TRUE
    if( ! base::is.null(add)){ # if add is NULL, then = 0
        if(base::grepl(pattern = "ggplot2::theme", add) == TRUE){
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") \"ggplot2::theme\" STRING DETECTED IN THE add ARGUMENT -> INTERNAL GGPLOT2 THEME FUNCTIONS ggplot2::theme() AND ggplot2::theme_classic() HAVE BEEN INACTIVATED, TO BE USED BY THE USER")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            add.check <- FALSE
        }
    }
    if(add.check == TRUE){
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_classic(base_size = text.size))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme(
            text = ggplot2::element_text(size = text.size), 
            plot.title = ggplot2::element_text(size = title.text.size), # stronger than text
            line = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank()
        ))
    }
    if(plot == TRUE){
        # suppressWarnings(
        base::print(base::eval(base::parse(text = base::paste(base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "), if(base::is.null(add)){NULL}else{add}))))
        # )
    }else{
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") PLOT NOT SHOWN AS REQUESTED")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(exp = base::options(warning.length = ini.warning.length), add = TRUE)
    if(return == TRUE){
        output <- ggplot2::ggplot_build(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + "))))
        output <- output$data
        base::names(output)[1] <- "heatmap"
        if( ! base::is.null(data2)){
            base::names(output)[2] <- "mask"
        }
        base::return(base::list(data = output, axes = output$layout$panel_params[[1]], scale = base::c(limit1[1],  midpoint1, limit1[2]), warn = warn))
    }
    #end main code
}
