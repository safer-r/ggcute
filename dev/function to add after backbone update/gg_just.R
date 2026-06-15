#' @title gg_just
#' @description
#' provide correct justification for text labeling, depending on the chosen angle
#' @param angle integer value of the text angle, using the same rules as in ggplot2. Positive values for counterclockwise rotation: 0 for horizontal, 90 for vertical, 180 for upside down etc. Negative values for clockwise rotation: 0 for horizontal, -90 for vertical, -180 for upside down etc. 
#' @param pos where text is? Either "top", "right", "bottom" or "left" of the elements to justify from.
#' @param kind kind of text? Either "axis" or "text". In the first case, the pos argument refers to the axis position, and in the second to annotated text (using ggplot2::annotate() or ggplot2::geom_text()).
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns a list containing: $angle: the submitted angle (value potentially reduced to fit the [-360 ; 360] interval, e.g., 460 -> 100, without impact on the final angle displayed); $pos: the selected position (argument pos); $kind: the selected kind of text (argument kind); $hjust: the horizontal justification; $vjust: the vertical justification.
#' @examples
#' gg_just(angle = 45, pos = "bottom")
#' gg_just(angle = (360*2 + 45), pos = "left")
#' output <- gg_just(angle = 45, pos = "bottom") ; obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; ggplot2::ggplot() + ggplot2::geom_bar(data = obs1, mapping = ggplot2::aes(x = group, y = time), stat = "identity") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = output$angle, hjust = output$hjust, vjust = output$vjust))
#' output <- gg_just(angle = -45, pos = "left") ; obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; ggplot2::ggplot() + ggplot2::geom_bar(data = obs1, mapping = ggplot2::aes(x = group, y = time), stat = "identity") + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = output$angle, hjust = output$hjust, vjust = output$vjust)) + ggplot2::coord_flip()
#' output1 <- gg_just(angle = 90, pos = "bottom") ; output2 <- gg_just(angle = -45, pos = "left") ; obs1 <- data.frame(time = 1:20, group = rep(c("CLASS_1", "CLASS_2"), times = 10), stringsAsFactors = TRUE) ; ggplot2::ggplot() + ggplot2::geom_bar(data = obs1, mapping = ggplot2::aes(x = group, y = time), stat = "identity") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = output1$angle, hjust = output1$hjust, vjust = output1$vjust), axis.text.y = ggplot2::element_text(angle = output2$angle, hjust = output2$hjust, vjust = output2$vjust))
#' output <- gg_just(angle = -45, pos = "left") ; obs1 <- data.frame(time = 1, km = 1, bird = "pigeon", stringsAsFactors = FALSE) ; ggplot2::ggplot(data = obs1, mapping = ggplot2::aes(x = time, y = km)) + ggplot2::geom_point() + ggplot2::geom_text(mapping = ggplot2::aes(label = bird), angle = output$angle, hjust = output$hjust, vjust = output$vjust)
#' obs1 <- data.frame(time = 1:10, km = 1:10, bird = c(NA, NA, NA, "pigeon", NA, "cat", NA, NA, NA, NA), stringsAsFactors = FALSE) ; for(i0 in c("text", "axis")){for(i1 in c("top", "right", "bottom", "left")){for(i2 in c(0, 45, 90, 135, 180, 225, 270, 315, 360)){output <- gg_just(angle = i2, pos = i1, kind = i0) ; title <- paste0("kind: ", i0, " | pos: ", i1, " | angle = ", i2, " | hjust: ", output$hjust, " | vjust: ", output$vjust) ; if(i0 == "text"){print(ggplot2::ggplot(data = obs1, mapping = ggplot2::aes(x = time, y = km)) + ggplot2::geom_point(color = gg_palette(1), alpha = 0.5) + ggplot2::ggtitle(title) + ggplot2::geom_text(mapping = ggplot2::aes(label = bird), angle = output$angle, hjust = output$hjust, vjust = output$vjust) + ggplot2::theme(title = ggplot2::element_text(size = 5)))}else{print(ggplot2::ggplot(data = obs1, mapping = ggplot2::aes(x = time, y = km)) + ggplot2::geom_point(color = gg_palette(1), alpha = 0.5) + ggplot2::ggtitle(title) + ggplot2::geom_text(mapping = ggplot2::aes(label = bird)) + ggplot2::scale_x_continuous(position = ifelse(i1 == "top", "top", "bottom")) + ggplot2::scale_y_continuous(position = ifelse(i1 == "right", "right", "left")) + ggplot2::theme(title = ggplot2::element_text(size = 5), axis.text.x = if(i1 %in% c("top", "bottom")){ggplot2::element_text(angle = output$angle, hjust = output$hjust, vjust = output$vjust)}, axis.text.y = if(i1 %in% c("right", "left")){ggplot2::element_text(angle = output$angle, hjust = output$hjust, vjust = output$vjust)}))}}}}
#' @importFrom saferDev arg_check
#' @details
#' - justification behave differently on plot, depending whether it is used for annotayed text or for axis labelling. Indeed the latter has labelling constrained.
#' - Of note, a bug in ggplot2: vjust sometimes does not work, i.e., the same justification result is obtained whatever the value used. This is the case with angle = 90, pos = "top", kind = "axis". While everything is fine with angle = 90, pos = "bottom", kind = "axis". At least, everything seems fine for kind = "axis" and pos = c("left", "bottom").
#' @export
gg_just <- function(
        angle, 
        pos, 
        kind = "axis",
        safer_check = TRUE
    ){
    # DEBUGGING
    # angle = 45 ; pos = "left" ; kind = "axis" ; safer_check = TRUE
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
    # end check of lib.path
    # check of the required function from the required packages
    if(safer_check == TRUE){
        saferGG:::.pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check"      
        ),
        lib.path = NULL,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "angle", 
        "pos"
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
    tempo <- saferDev::arg_check(data = angle , class = "integer", length = 1, double_as_integer_allowed = TRUE, neg_values = TRUE, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = pos, options = base::c("top", "right", "bottom", "left"), length = 1, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = kind, options = base::c("axis", "text"), length = 1, safer_check = FALSE) ; base::eval(ee)

    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
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
        "angle", 
        "pos", 
        "kind" 
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
    # end second round of checking and data preparation

    
    # main code
    # to get angle between -360 and 360
    while(angle > 360){
        angle <- angle - 360
    }
    while(angle < -360){
        angle <- angle + 360
    }
    # end to get angle between -360 and 360
    # justifications
    if(pos %in% base::c("bottom", "top")){
        # code below is for if(pos == "bottom"){
        if(base::any(base::sapply(FUN = all.equal, base::c(-360, -180, 0, 180, 360), angle) == TRUE)){ # equivalent of angle == -360 | angle == -180 | angle == 0 | angle == 180 | angle == 360 but deals with floats
            hjust <- 0.5
            if(kind == "text"){
                if(base::any(base::sapply(FUN = all.equal, base::c(-360, 0, 360), angle) == TRUE)){
                    vjust <- 1
                }else if(base::any(base::sapply(FUN = all.equal, base::c(-180, 180), angle) == TRUE)){
                    vjust <- 0
                }
            }else{
                vjust <- 0.5
            }
        }else if(base::any(base::sapply(FUN = all.equal, base::c(-270, 90), angle) == TRUE)){
            hjust <- 1
            vjust <- 0.5
        }else if(base::any(base::sapply(FUN = all.equal, base::c(-90, 270), angle) == TRUE)){
            hjust <- 0
            vjust <- 0.5
        }else if((angle > -360 & angle < -270) | (angle > 0 & angle < 90)){
            hjust <- 1
            vjust <- 1
        }else if((angle > -270 & angle < -180) | (angle > 90 & angle < 180)){
            hjust <- 1
            vjust <- 0
        }else if((angle > -180 & angle < -90) | (angle > 180 & angle < 270)){
            hjust <- 0
            vjust <- 0
            if(kind == "text" & pos == "top"){
                hjust <- 1
            }
        }else if((angle > -90 & angle < 0) | (angle > 270 & angle < 360)){
            hjust <- 0
            vjust <- 1
        }
        if(pos == "top"){
            if( ! ((angle > -180 & angle < -90) | (angle > 180 & angle < 270))){
                hjust <- 1 - hjust
            }
            vjust <- 1 - vjust
        }
    }else if(pos %in% base::c("left", "right")){
        # code below is for if(pos == "left"){
        if(base::any(base::sapply(FUN = all.equal, base::c(-270, -90, 90, 270), angle) == TRUE)){ # equivalent of angle == -270 | angle == -90 | angle == 90 | angle == 270 but deals with floats
            hjust <- 0.5
            if(kind == "text"){
                if(base::any(base::sapply(FUN = all.equal, base::c(-90, 90), angle) == TRUE)){
                    vjust <- 0
                }else if(base::any(base::sapply(FUN = all.equal, base::c(-270, 270), angle) == TRUE)){
                    vjust <- 1
                }
            }else{
                vjust <- 0.5
            }
        }else if(base::any(base::sapply(FUN = all.equal, base::c(-360, 0, 360), angle) == TRUE)){
            hjust <- 1
            vjust <- 0.5
        }else if(base::any(base::sapply(FUN = all.equal, base::c(-180, 180), angle) == TRUE)){
            hjust <- 0
            vjust <- 0.5
        }else if((angle > -360 & angle < -270) | (angle > 0 & angle < 90)){
            hjust <- 1
            vjust <- 0
        }else if((angle > -270 & angle < -180) | (angle > 90 & angle < 180)){
            hjust <- 0
            vjust <- 0
        }else if((angle > -180 & angle < -90) | (angle > 180 & angle < 270)){
            hjust <- 0
            vjust <- 1
        }else if((angle > -90 & angle < 0) | (angle > 270 & angle < 360)){
            hjust <- 1
            vjust <- 1
        }
        if(pos == "right"){
            hjust <- 1 - hjust
            if( ! (((angle > -270 & angle < -180) | (angle > 90 & angle < 180)) | ((angle > -180 & angle < -90) | (angle > 180 & angle < 270)))){
                vjust <- 1 - vjust
            }
        }
    }
    # end justifications
    output <- base::list(angle = angle, pos = pos, kind = kind, hjust = hjust, vjust = vjust)
    base::return(output)
    # end main code
}
