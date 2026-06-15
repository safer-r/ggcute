#' @title gg_point_rast
#' @description
#' equivalent to ggplot2::geom_point() but in raster mode use it like ggplot2::geom_point() with the main raster.dpi additional argument.
#' @details
#' can be long to generate the plot.
#' use a square plot region. Otherwise, the dots will have ellipsoid shape.
#' solve the transparency problems with some GUI.
#' this function is derived from the geom_point_rast() function, created by Viktor Petukhov , and present in the ggrastr package (https://rdrr.io/github/VPetukhov/ggrastr/src/R/geom-point-rast.R, MIT License, Copyright (c) 2017 Viktor Petukhov). Has been placed here to minimize package dependencies.
#' @param data classical arguments of geom_point(), shown here https://rdrr.io/github/VPetukhov/ggrastr/man/geom_point_rast.html
#' @param mapping idem
#' @param stat idem
#' @param position idem
#' @param na.rm idem
#' @param show.legend idem
#' @param inherit.aes idem
#' @param raster.width width of the result image (in inches). Default: deterined by the current device parameters.
#' @param raster.height height of the result image (in inches). Default: deterined by the current device parameters.
#' @param raster.dpi resolution of the result image.
#' @param fun.name logical. Inactivate the fun.name argument of the saferDev::arg_check() function? If TRUE, the name of the saferDev::arg_check() function in error messages coming from this function. Use TRUE if gg_point_rast() is used like this: eval(parse(text = "gg_point_rast")).
#' @param lib.path character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns a raster scatter plot.
#' @examples
#' \dontrun{
#' # Two pdf in the current directory
#' set.seed(1) ; data1 = data.frame(x = rnorm(100000), y = rnorm(100000), stringsAsFactors = TRUE) ; saferGraph::open2(pdf.name = "Raster") ; ggplot2::ggplot() + gg_point_rast(data = data1, mapping = ggplot2::aes(x = x, y = y), fun.name = FALSE) ; saferGraph::open2(pdf.name = "Vectorial") ; ggplot2::ggplot() + ggplot2::geom_point(data = data1, mapping = ggplot2::aes(x = x, y = y)) ; dev.off();dev.off()
#' }
#' @importFrom Cairo Cairo
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 GeomPoint
#' @importFrom grid grid.cap
#' @importFrom grid grid.points
#' @importFrom grid popViewport
#' @importFrom grid pushViewport
#' @importFrom grid rasterGrob
#' @importFrom grid viewport
#' @importFrom saferDev arg_check
#' @importFrom saferGraph open2
#' @export
gg_point_rast <- function(
        data = NULL, 
        mapping = NULL, 
        stat = "identity", 
        position = "identity", 
        na.rm = FALSE, 
        show.legend = NA, 
        inherit.aes = TRUE, 
        raster.width = NULL, 
        raster.height = NULL, 
        raster.dpi = 300, 
        fun.name = TRUE, 
        lib.path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # package name
    package.name <- "saferGG"
    # end package name
    # function name
    if(base::all(fun.name == FALSE)){ # fun.name has to be used here but will be fully checked below
         function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
        if(function.name[1] == "::()"){
            function.name <- function.name[3]
        }
        arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
        arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    }else if(base::all(fun.name == TRUE)){
        function.name <- NULL
        arg.names <- NULL
        arg.user.setting <- NULL
    }else{
        tempo.cat <- base::paste0("ERROR IN gg_point_rast(): CODE INCONSISTENCY 1")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
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
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
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
            "Cairo::Cairo",
            "ggplot2::ggproto",
            "ggplot2::layer",
            "ggplot2::GeomPoint",
            "graphics::par",
            "grDevices::dev.cur",
            "grDevices::dev.off",
            "grDevices::dev.set",
            "grid::grid.cap",
            "grid::grid.points",
            "grid::popViewport",
            "grid::pushViewport",
            "grid::rasterGrob",
            "grid::viewport",
            "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    # end arg with no default values
    # argument checking with arg_check()    
    # argument checking
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    if( ! base::is.null(data)){
        tempo <- saferDev::arg_check(data = data, class = "data.frame", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(mapping)){
        tempo <- saferDev::arg_check(data = mapping, class = "uneval", typeof = "list", fun.name = function.name, safer_check = FALSE) ; base::eval(ee) # aes() is tested
    }
    # stat and position not tested because too complicate
    tempo <- saferDev::arg_check(data = na.rm, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = show.legend, class = "vector", mode = "logical", length = 1, na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = inherit.aes, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(raster.width)){
        tempo <- saferDev::arg_check(data = raster.width, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(raster.height)){
        tempo <- saferDev::arg_check(data = raster.height, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = raster.dpi, class = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = fun.name, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::dir.exists(lib.path))){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- base::paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # source("C:/Users/gmillot/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_saferDev::arg_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end argument checking

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
        "stat",
        "position",
        "na.rm",
        "show.legend",
        "inherit.aes",
        "raster.dpi",
        "fun.name"
        # "raster.width",  # inactivated because can be null
        # "raster.height",  # inactivated because can be null      
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

    # additional functions
    DrawGeomPointRast <- function(data, panel_params, coord, na.rm = FALSE, raster.width = NULL, raster.height= NULL, raster.dpi = raster.dpi){
        if (base::is.null(raster.width)){
            raster.width <- graphics::par('fin')[1]
        }
        if (base::is.null(raster.height)){
            raster.height <- graphics::par('fin')[2]
        }
        prev_dev_id <- grDevices::dev.cur()
        p <- ggplot2::GeomPoint$draw_panel(data, panel_params, coord)
        dev_id <- Cairo::Cairo(type='raster', width = raster.width*raster.dpi, height = raster.height*raster.dpi, dpi = raster.dpi, units = 'px', bg = "transparent")[1]
        grid::pushViewport(grid::viewport(width = 1, height = 1))
        grid::grid.points(x = p$x, y = p$y, pch = p$pch, size = p$size,
                          name = p$name, gp = p$gp, vp = p$vp, draw = T)
        grid::popViewport()
        cap <- grid::grid.cap()
        base::invisible(grDevices::dev.off(dev_id))
        base::invisible(grDevices::dev.set(prev_dev_id))
        grid::rasterGrob(cap, x = 0, y = 0, width = 1, height = 1, default.units = "native", just = base::c("left","bottom"))
    }
    # end additional functions
    # main code
    GeomPointRast <- ggplot2::ggproto("GeomPointRast", ggplot2::GeomPoint, draw_panel = DrawGeomPointRast)
    ggplot2::layer(
        data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomPointRast, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = base::list(
            na.rm = na.rm, 
            raster.width = raster.width, 
            raster.height = raster.height, 
            raster.dpi = raster.dpi
        )
    )
    # end main code
}
