#' @title gg_miami
#' @description
#' Plot two ggplot2 manhattan plots in mirror.
#' @param file single character string of the vcf.tsv file path. Example: "/mnt/c/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/other/fisher.tsv".
#' @param chr single character string of the human chromo infos file.tsv path. Example: chr_path = "/mnt/c/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/hg19_grch37p5_chr_size_cumul.txt".
#' @param top.y.column single character string of any of the quantitative column of the vcf.tsv file for the y-axis of the manhattan plot at the top of the miami plot. Can also be an added column through the tsv_extra_fields parameter.
#' @param bottom.y.column as the top.y.column parameter but for the bottom manhattan plot of the miami plot. "NULL" generates a simple manhattan plot
#' @param x_lim single character string of the x-axis limits. Either "whole" for the whole genome or a special character string to have the x-axis limited to the x_lim parameter. Write NULL to do not plot results. For special character strings,  write "chr1:11000-15000" for a single region, "chr1:11000-" for a single region non delimited on the right, "chr7:0-147000000,chr10:1000000-2000000" if two regions, "chr7" for a whole chromosome, "chr7,chr1" for two chromosomes.
#' @param vgrid single character string of logical value TRUE or FALSE. Display the chromosome separators in the miami plot? Example: vgrid = TRUE.
#' @param color.column single character string of one of the column name of the .tsv file (see bottom.y.column) in order to color the dots. Write NULL if not required (dots will be alternatively grey and blue, according to chromo order).
#' @param dot.border.color single color character string to color the border of the dots. Write NULL if not required.
#' @param y_lim1 2 numeric values indicating the range of the top y-axis. Order matters (for inverted axis). If NULL, the range of the values from top.y.column will be used.
#' @param y_lim2 2 numeric values indicating the range of the bottom y-axis. Order matters (for inverted axis). If NULL, the range of the values from bottom.y.column will be used.
#' @param reverse1 single character string of logical value TRUE or FALSE, y-axis coordinates flip for the top panel in the miami plot. Example: reverse1 = TRUE.
#' @param reverse2 single character string of logical value TRUE or FALSE, y-axis coordinates flip for the bottom panel in the miami plot. Example: reverse2 = TRUE.
#' @param y.threshold1 single numeric value for the y-axis threshold of the top panel in the miami plot, beyond which values are of interest. Example: y.threshold1 = 3. Write NULL for no particular threshold.
#' @param y.threshold2 single numeric value for the y-axis threshold of the bottom panel in the miami plot, beyond which values are of interest. Example: y.threshold2 = 3. Write NULL for no particular threshold. Not considered if bottom.y.column is NULL.
#' @param y.log1 single logical value TRUE or FALSE for the  y-axis log10 scale of the top panel in the miami plot. Example: y.log1 = TRUE.
#' @param y.log2 single logical value TRUE or FALSE for the  y-axis log10 scale of the bottom panel in the miami plot. Example: y.log2 = TRUE.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns a list containing: $angle: the submitted angle (value potentially reduced to fit the [-360 ; 360] interval, e.g., 460 -> 100, without impact on the final angle displayed); $pos: the selected position (argument pos); $kind: the selected kind of text (argument kind); $hjust: the horizontal justification; $vjust: the vertical justification.
#' @examples
#' gg_miami(file = "C:/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/other/fisher.tsv",  chr = "C:/Users/gmillot/Documents/Git_projects/fisher_for_vcf/dataset/hg19_grch37p5_chr_size_cumul.txt", top.y.column = "NEG_LOG10_P_VALUE_CARRIER_MODEL", bottom.y.column = "AF")
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid unit.pmax
#' @importFrom gridExtra grid.arrange
#' @importFrom saferDev arg_check
#' @importFrom scales math_format
#' @importFrom scales rescale_none
#' @importFrom scales trans_breaks
#' @importFrom scales trans_format
#' @importFrom scales rescale_none
#' @export
gg_miami <- function(
        file, 
        chr, 
        top.y.column,
        bottom.y.column,
        x_lim = "whole", 
        vgrid = FALSE, 
        color.column = NULL,
        dot.border.color = NULL, 
        y_lim1 = NULL, 
        y_lim2 = NULL,
        reverse1 = FALSE, 
        reverse2 = FALSE, 
        y.threshold1 = NULL, 
        y.threshold2 = NULL, 
        y.log1 = FALSE, 
        y.log2 = FALSE,
        safer_check = TRUE
    ){
    # DEBUGGING
    #
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
            "ggplot2::annotation_custom",
            "ggplot2::ggtitle",
            "gridExtra::grid.arrange",
            "saferDev::arg_check",
            "scales::math_format",
            "scales::rescale_none",
            "scales::trans_breaks",
            "scales::trans_format",
            "scales::rescale_none"
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
        "file", 
        "chr", 
        "top.y.column",
        "bottom.y.column"
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
    tempo <- saferDev::arg_check(data = file, class = "vector", typeof = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = chr, class = "vector", typeof = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = top.y.column, class = "vector", typeof = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(bottom.y.column)){
        tempo <- saferDev::arg_check(data = bottom.y.column, class = "vector", typeof = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; 
    }
    if( ! base::is.null(x_lim)){
        tempo <- saferDev::arg_check(data = x_lim, class = "vector", typeof = "character", length = 1, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = vgrid, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(color.column)){
        tempo <- saferDev::arg_check(data = color.column, class = "vector", typeof = "character", length = 1, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(dot.border.color)){
        tempo <- saferDev::arg_check(data = dot.border.color, class = "vector", typeof = "character", length = 1, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(y_lim1)){
        tempo <- saferDev::arg_check(data = y_lim1, class = "vector", mode = "numeric", length = 2, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(y_lim2)){
        tempo <- saferDev::arg_check(data = y_lim2, class = "vector", mode = "numeric", length = 2, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = reverse1, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = reverse2, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(y.threshold1)){
        tempo <- saferDev::arg_check(data = y.threshold1, class = "vector", typeof = "numeric", length = 1, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(y.threshold2)){
        tempo <- saferDev::arg_check(data = y.threshold2, class = "vector", typeof = "numeric", length = 1, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = y.log1, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.log2, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = safer_check, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
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
        "file",
        "chr", 
        "top.y.column",
        # "bottom.y.column", 
        # "x_lim", 
        "vgrid", 
        # "color.column", # inactivated because can be null
        # "dot.border.color", # inactivated because can be null
        # "y_lim1", # inactivated because can be null
        # "y_lim2", # inactivated because can be null
        "reverse1", 
        "reverse2", 
        # "y.threshold1", # inactivated because can be null
        # " y.threshold2", # inactivated because can be null
        "y.log1", 
        "y.log2", 
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    ######## management of "" in arguments of mode character
    tempo.arg <-base::c(
        "file", 
        "chr", 
        "top.y.column",
        "bottom.y.column", 
        "x_lim", 
        "vgrid", 
        "color.column", 
        "dot.border.color", 
        "y_lim1", 
        "y_lim2", 
        "reverse1", 
        "reverse2",
        "y.threshold1", 
        "y.threshold2", 
        "y.log1", 
        "y.log2"
    )
    tempo.log <- ! sapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = function(x){base::all(base::mode(x) == "character", na.rm = TRUE)})
    if(base::any(tempo.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("INTERNAL ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " ARE NOT MODE \"character\":\n", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = function(x){base::any(x == "")})
        if(base::any(tempo.log, na.rm = TRUE)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT CONTAIN \"\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    # warn.count <- 0 # not required
    # end warning initiation
    # other checkings
    for(i0 in base::c("vgrid", "reverse1", "reverse2", "y.log1", "y.log2")){
        if(base::get(i0) == "TRUE"){
            base::assign(i0, TRUE)
        }else if(base::get(i0) == "FALSE"){
            base::assign(i0, FALSE)
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", i0, " PARAMETER CAN ONLY BE \"TRUE\" OR \"FALSE\": ", base::get(i0))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end other checkings
    # end second round of checking and data preparation

    # main code
    # ignition
    # end ignition


    # data import
    if( ! base::file.exists(file)){
        base::stop(base::paste0("\n\n============\n\nERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n\nFILE INDICATED IN THE file PARAMETER DOES NOT EXISTS: ", file, "\n\n============\n\n"), call. = FALSE)
    }else{
        obs <- utils::read.table(file, sep = "\t", stringsAsFactors = FALSE, header = TRUE, comment.char = "")
        if(base::length(obs) > 0 & base::nrow(obs) > 0){
            empty.obs <- FALSE
        }else{
            empty.obs <- TRUE
        }
    }
    if( ! base::file.exists(chr)){
        base::stop(base::paste0("\n\n============\n\nERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFILE INDICATED IN THE chr PARAMETER DOES NOT EXISTS: ", chr, "\n\n============\n\n"), call. = FALSE)
    }else{
        chr <- utils::read.table(chr, sep = "\t", stringsAsFactors = FALSE, header = TRUE, comment.char = "")
    }
    # end data import
    # modifications of imported tables
    is.whole <- FALSE
    xmin_plot <- 0 # coordinates for plotting 
    if(base::length(obs) > 0 & base::nrow(obs) > 0){
        # names(obs)[names(obs) == "NEG_LOG10_P_VALUE"] <- "neg.log10.p"
        # names(obs)[names(obs) == "PATIENT_NB"] <- "Nb_of_indiv"
        if(base::any(base::grepl(x = obs$CHROM, pattern = "chr"))){
            obs$CHROM <- base::sub(x = obs$CHROM, pattern = "chr", replacement = "")
        }
        if(base::any(base::grepl(x = obs$CHROM, pattern = "^X$"))){
            obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^X$")] <- "23"
        }
        if(base::any(base::grepl(x = obs$CHROM, pattern = "^Y$"))){
            obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^Y$")] <- "24"
        }
        if(base::any(base::grepl(x = obs$CHROM, pattern = "^MT$|^M$"))){
            obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^MT$|^M$")] <- "25"
        }
        if(base::any( ! base::grepl(x = obs$CHROM, pattern = "\\d"))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE chr COLUMN of the .tsv FILE HAS LETTERS IN IT, OTHER THAN X, Y and MT:\n", base::paste0(obs$CHROM[base::grepl(x = obs$CHROM, pattern = "^\\d")], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            obs$CHROM <- base::as.integer(obs$CHROM)
        }
        # add the chr info to obs
        obs <- base::data.frame(obs, coord = base::vector(mode = "numeric", length = base::nrow(obs)))
        for(i1 in chr$CHR_NB){
            obs$coord[obs$CHROM == i1] <- obs$POS[obs$CHROM == i1] + chr$LENGTH_CUMUL_TO_ADD[i1]
        }
        # preparation of the x coordinates: three solutions: 1) whole object (see above), 2) single chromo "chr7" or "chr7:0-15", 3) several chromo chr7, chr8" or "chr7:0-15, chr8" or "chr7:0-15, chr8:0-20"
        # The idea is to select rows of chr and potentially restrict some chr limits
        if( ! base::is.null(x_lim)){
            if(x_lim == "whole"){ #at that stage, x_lim is a single character
                is.whole <- TRUE
            }else{
                tempo1 <- base::strsplit(x = x_lim, split = ",")[[1]]
                tempo1 <- base::gsub(x = tempo1, pattern = " ", replacement = "")
                chr_x_lim <- tempo1
                coord_x_lim <- NULL
                xmin_x_lim <- NULL
                xmax_x_lim <- NULL
                if( ! base::all(base::grepl(x = tempo1, pattern = "^chr.+"))){
                    tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE x_lim PARAMETER MUST START WITH \"chr\" IF NOT \"whole\":\n", base::paste0(x_lim, collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
                if(base::any(base::grepl(x = tempo1, pattern = ":"))){
                    # means that there are coordinates
                    if( ! base::all(base::grepl(tempo1, pattern = "-"))){# normally no NA with is.null()
                        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE x_lim PARAMETER MUST BE WRITTEN LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\" IF COORDINATES ARE SPECIFIED: \n", base::paste0(x_lim, collapse = " "))
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                    }
                    tempo2 <- base::strsplit(x = tempo1, split = ":")
                    chr_x_lim <- base::sapply(X = tempo2, FUN = function(x){x[1]})
                    chr_x_lim <- base::gsub(x = chr_x_lim, pattern = " ", replacement = "")
                    coord_x_lim <- base::sapply(X = tempo2, FUN = function(x){x[2]})
                    tempo3 <- base::strsplit(x = coord_x_lim, split = "-")
                    xmin_x_lim <- base::sapply(X = tempo3, FUN = function(x){x[1]})
                    xmin_x_lim <- base::gsub(x = xmin_x_lim, pattern = " ", replacement = "")
                    xmax_x_lim <- base::sapply(X = tempo3, FUN = function(x){x[2]})
                    xmax_x_lim <- base::gsub(x = xmax_x_lim, pattern = " ", replacement = "")
                    if(base::any(base::grepl(xmin_x_lim, pattern = "\\D")) | base::any(base::grepl(xmax_x_lim, pattern = "\\D"))){# normally no NA with is.null()
                        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE x_lim PARAMETER MUST BE WRITTEN LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\" IF COORDINATES ARE SPECIFIED: \n", base::paste0(x_lim, collapse = " "))
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                    }else{
                        xmin_x_lim <- base::as.integer(xmin_x_lim)
                        xmax_x_lim <- base::as.integer(xmax_x_lim)
                        if(base::any(xmax_x_lim - xmin_x_lim < 0)){
                            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE x_lim PARAMETER MUST BE WRITTEN WITH ORDERED COORDINATES, LIKE THIS \"chr7:0-147000000, chr10:1000000-2000000\", IF COORDINATES ARE SPECIFIED: \n", base::paste0(x_lim, collapse = " "))
                            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                        }
                    }
                }
                # modification of the chr object for restricted plotting
                tempo.coord <- base::which(chr$CHR %in% chr_x_lim) # which rows of chr to take for plotting
                if(base::any(chr$BP_LENGTH[tempo.coord] - xmax_x_lim < 0)){
                    tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE x_lim PARAMETER HAS AT LEAST ONE COORDINATE THAT IS ABOVE THE MAX LENGTH OF THE CHROMO.\nCHROMO LENGTH: ", base::paste0(chr$BP_LENGTH[tempo.coord], collapse = " "), "\nMAX COORDINATE: ", base::paste0(xmax_x_lim, collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
                if(tempo.coord[1] > 1){
                    xmin_plot <- chr$LENGTH_CUMUL[tempo.coord[1] - 1]
                }
                chr <- chr[tempo.coord[1]:tempo.coord[base::length(tempo.coord)], ]
                if( ! base::is.null(coord_x_lim)){
                    xmin_plot <- xmin_plot + xmin_x_lim[1] # the left boundary of the plot is corrected
                    chr$LENGTH_CUMUL[base::nrow(chr)] <- chr$LENGTH_CUMUL[base::nrow(chr)] - chr$BP_LENGTH[base::nrow(chr)] + xmax_x_lim[base::length(xmax_x_lim)] # the right boundary of the plot is corrected
                    chr$CHR_NAME_POS <- (base::c(xmin_plot, chr$LENGTH_CUMUL[-base::nrow(chr)]) + chr$LENGTH_CUMUL) / 2 # the positions of names in the x-axis of the plot are corrected
                }
                # restriction of obs
                obs <- obs[obs$coord >= xmin_plot & obs$coord <= chr$LENGTH_CUMUL[base::nrow(chr)], ]
            }
        }else{
            tempo.warn <- base::paste0("x_lim is NULL: NO PLOT DRAWN")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn,base::paste0(warn, "\n\n", tempo.warn)))
        }
    }

    for(i0 in base::c("y_lim1", "y_lim2")){
        if( ! base::is.null(base::get(i0))){
            tempo <- base::unlist(base::strsplit(x = base::get(i0), split = " "))
            if(base::length(tempo) != 2 | ! base::all(base::grepl(tempo, pattern = "^[0123456789.\\-\\+eE]*$"))){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE ", i0, " PARAMETER MUST BE TWO NUMERIC VALUES SEPARATED BY A SINGLE SPACE\nHERE IT IS: \n", base::paste0(base::get(i0), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                base::assign(i0, base::as.numeric(tempo))
            }
        }
    }
    for(i0 in base::c("y.threshold1", "y.threshold2")){
        if( ! base::is.null(base::get(i0))){
            tempo <- base::unlist(base::strsplit(x = base::get(i0), split = " "))
            if(base::length(tempo) != 1 | ! base::all(base::grepl(tempo, pattern = "^[0123456789.\\-\\+eE]*$"))){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE ", i0, " PARAMETER MUST BE TWO NUMERIC VALUES SEPARATED BY A SINGLE SPACE\nHERE IT IS: \n", base::paste0(base::get(i0), collapse = " "))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                base::assign(i0, base::as.numeric(tempo))
            }
        }
    }

    if( ! top.y.column %in% base::names(obs)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE top.y.column PARAMETER MUST BE A COLUMN NAME OF THE FISHER TABLE.\n\ntop.y.column PARAMETER:\n", base::paste0(top.y.column, collapse = " "), "\n\nCOLUMN NAMES:\n", base::paste0(base::names(obs), collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! bottom.y.column %in% base::names(obs)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE bottom.y.column PARAMETER MUST BE A COLUMN NAME OF THE FISHER TABLE.\n\nbottom.y.column PARAMETER:\n", base::paste0(bottom.y.column, collapse = " "), "\n\nCOLUMN NAMES:\n", base::paste0(base::names(obs), collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }



    # end modifications of imported tables


    # plotting
    #fun_open(width = 12, height = 4, pdf.name = base::paste0("plot_read_length_", kind)) # must be systematically opened for main.nf
    png.size <- 1800 # px
    grDevices::png(filename = base::paste0("miami.png"), width = png.size * 2, height = png.size, units = "px", res = 300)

    if(empty.obs == TRUE){
        saferGG::gg_empty(text = base::paste0("NO PLOT DRAWN\nTHE region PARAMETER\nMIGHT BE OUTSIDE\nOF THE RANGE OF THE VCF FILE"))
    }else if(base::length(obs) > 0 & base::nrow(obs) > 0 & ! base::is.null(x_lim)){
        marging <- (chr$LENGTH_CUMUL[base::nrow(chr)] - xmin_plot) * 0.005 # chr$LENGTH_CUMUL and xmin_plot have been corrected depending on x_lim boundaries
        y.min.pos <- base::ifelse(base::is.null(y_lim1), base::min(obs[ , top.y.column]), base::min(y_lim1))
        y.max.pos <- base::ifelse(base::is.null(y_lim1), base::max(obs[ , top.y.column]), base::max(y_lim1))
        tempo.gg.name <- "gg.indiv.plot."
        tempo.gg.count <- 0
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggplot(obs, ggplot2::aes_string(x = "coord", y = top.y.column)))
        if(vgrid){
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_vline(
                xintercept = base::c(xmin_plot, chr$LENGTH_CUMUL),
                size = 0.25,
                color = "grey80"
            ))
        }
        if( ! base::is.null(y.threshold1)){
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_hline(
                yintercept = y.threshold1,
                linetype = "22", 
                size = 0.25, 
                color = "red"
            ))
        }
        if(base::is.null(color.column)){
            if(base::is.null(dot.border.color)){
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                    ggplot2::aes(color = base::as.factor(CHROM)), 
                    alpha = 1, 
                    pch = 16, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_color_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
            }else{
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                    ggplot2::aes(fill = base::as.factor(CHROM)), 
                    alpha = 1, 
                    color = dot.border.color, 
                    pch = 21, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_fill_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
            }
        }else{
            if(base::is.null(dot.border.color)){
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                    ggplot2::aes(color = color.column), 
                    alpha = 1, 
                    pch = 16, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_color_gradient2())
            }else{
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_point(
                    ggplot2::aes(fill = base::as.factor(CHROM)), 
                    alpha = 1, 
                    color = dot.border.color, 
                    pch = 21, 
                    size = 1
                ))
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_fill_gradient2())
            }
        }
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::ggtitle(
            base::paste0("x_lim: ", base::ifelse(is.whole, "whole genome", x_lim), 
            base::ifelse( ! base::is.null(y.threshold1), base::paste0(", top threshold: ", y.threshold1), ""), 
            base::ifelse( ! (base::is.null(y.threshold2) & base::is.null(bottom.y.column)), base::paste0(", bottom threshold: ", y.threshold2), ""), 
            base::ifelse(y.log1, ", top y-axis: log10", ""), base::ifelse(y.log2, ", bottom y-axis: log10", ""))
        ))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_x_continuous(
            name = "CHR", 
            expand = base::c(0, 0), # remove space after after axis limits
            oob = scales::rescale_none,
            label = chr$CHR_NAME, 
            breaks= chr$CHR_NAME_POS, 
            limits = base::c(xmin_plot - marging, base::max(chr$LENGTH_CUMUL) + marging)
        ))
        if(y.log1){
            if(base::any(obs[ , top.y.column] <= 0)){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE y.log1 PARAMETER CANNOT BE SET TO \"TRUE\" IF 0 OR NEG VALUES IN THE ", top.y.column, " FIELD OF THE TSV OR VCF")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_y_continuous(
                    expand = base::c(0, 0), # remove space after after axis limits
                    limits = if(reverse1){base::c(y.max.pos, y.min.pos)}else{base::c(y.min.pos, y.max.pos)}, # NA indicate that limits must correspond to data limits but ylim() already used
                    oob = scales::rescale_none, 
                    trans = "log10", 
                    breaks = scales::trans_breaks("log10", function(x){10^x}), 
                    labels = scales::trans_format("log10", scales::math_format(10^.x))
                ))
                # assign(paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), annotation_logticks(outside = TRUE))
            }
        }else{
            base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::scale_y_continuous(
                expand = base::c(0, 0), # remove space after after axis limits
                limits = if(reverse1){base::c(y.max.pos, y.min.pos)}else{base::c(y.min.pos, y.max.pos)}, # NA indicate that limits must correspond to data limits but ylim() already used
                oob = scales::rescale_none, 
                trans = "identity"
            ))
        }
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme_bw())
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::theme(
            plot.title = ggplot2::element_text(size = 8), 
            legend.position=if(base::is.null(color.column)){"none"}, 
            panel.border = ggplot2::element_blank(), 
            panel.grid = ggplot2::element_blank(), 
            axis.ticks.x = ggplot2::element_blank(), 
            axis.ticks.y.left = ggplot2::element_line(size = 0.25), 
            # axis.line.x.bottom = ggplot2::element_line(size = 0.25), # ugly, thus i added geom_hline below
            axis.line.y.left = ggplot2::element_line(size = 0.25) 
        ))
        base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::geom_hline(
            yintercept = base::ifelse(
                base::is.null(y_lim1), 
                base::ifelse(reverse1, base::max(obs[ , top.y.column]), base::min(obs[ , top.y.column])), 
                base::ifelse(reverse1, base::max(y_lim1), base::min(y_lim1))
            ), 
            size = 0.25
        ))
        # add tick lines if vgrid is FALSE
        if( ! vgrid){
            gline = grid::linesGrob(y = base::c(-0.02, 0),  gp = grid::gpar(col = "black", lwd = 0.5))
            for(i2 in base::c(xmin_plot, chr$LENGTH_CUMUL)){
                base::assign(base::paste0(tempo.gg.name, tempo.gg.count <- tempo.gg.count + 1), ggplot2::annotation_custom(gline, xmin = i2, xmax = i2, ymin = -Inf, ymax = Inf))
            }
        }


        fin.plot1 <- base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name, 1:tempo.gg.count), collapse = " + ")))))
        # tempo.output <- ggplot2::ggplot_build(fin.plot1)

        if(base::is.null(bottom.y.column)){
            base::suppressMessages(base::suppressWarnings(gridExtra::grid.arrange(fin.plot1, ncol=1, nrow = 1)))
        }else{
            y.min.pos2 <- base::ifelse(base::is.null(y_lim2), base::min(obs[ , bottom.y.column]), base::min(y_lim2))
            y.max.pos2 <- base::ifelse(base::is.null(y_lim2), base::max(obs[ , bottom.y.column]), base::max(y_lim2))
            tempo.gg.name2 <- "gg.indiv.plot."
            tempo.gg.count2 <- 0
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::ggplot(obs, ggplot2::aes_string(x = "coord", y = bottom.y.column)))
            if(vgrid){
                base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::geom_vline(
                    xintercept = base::c(xmin_plot, chr$LENGTH_CUMUL),
                    size = 0.25,
                    color = "grey80"
                ))
            }
            if( ! base::is.null(y.threshold2)){
                base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::geom_hline(
                    yintercept = y.threshold2,
                    linetype = "22", 
                    size = 0.25, 
                    color = "red"
                ))
            }
            if(base::is.null(color.column)){
                if(base::is.null(dot.border.color)){
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::geom_point(
                        ggplot2::aes(color = base::as.factor(CHROM)), 
                        alpha = 1, 
                        pch = 16, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::scale_color_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
                }else{
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::geom_point(
                        ggplot2::aes(fill = base::as.factor(CHROM)), 
                        alpha = 1, 
                        color = dot.border.color, 
                        pch = 21, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::scale_fill_manual(values = base::rep(base::c("grey20", "skyblue"), 25)))
                }
            }else{
                if(base::is.null(dot.border.color)){
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::geom_point(
                        ggplot2::aes(color = color.column), 
                        alpha = 1, 
                        pch = 16, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::scale_color_gradient2())
                }else{
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::geom_point(
                        ggplot2::aes(fill = base::as.factor(CHROM)), 
                        alpha = 1, 
                        color = dot.border.color, 
                        pch = 21, 
                        size = 1
                    ))
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::scale_fill_gradient2())
                }
            }
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::scale_x_continuous(
                expand = base::c(0, 0), # remove space after after axis limits
                oob = scales::rescale_none,
                label = chr$CHR_NAME, 
                breaks= chr$CHR_NAME_POS, 
                limits = base::c(xmin_plot - marging, base::max(chr$LENGTH_CUMUL) + marging)
            ))
            if(y.log2){
                if(base::any(obs[ , bottom.y.column] <= 0)){
                    tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE y.log2 PARAMETER CANNOT BE SET TO \"TRUE\" IF 0 OR NEG VALUES IN THE ", bottom.y.column, " FIELD OF THE TSV OR VCF")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }else{
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::scale_y_continuous(
                        expand = base::c(0, 0), # remove space after after axis limits
                        limits = if(reverse2){base::c(y.min.pos2, y.max.pos2)}else{base::c(y.max.pos2, y.min.pos2)}, # NA indicate that limits must correspond to data limits but ylim() already used
                        oob = scales::rescale_none, 
                        trans = "log10", 
                        breaks = scales::trans_breaks("log10", function(x){10^x}), 
                        labels = scales::trans_format("log10", scales::math_format(10^.x))
                    ))
                    # assign(paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), annotation_logticks(outside = TRUE)) # 
                }
            }else{
                base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::scale_y_continuous(
                    expand = base::c(0, 0), # remove space after after axis limits
                    limits = if(reverse2){base::c(y.min.pos2, y.max.pos2)}else{base::c(y.max.pos2, y.min.pos2)}, # NA indicate that limits must correspond to data limits but ylim() already used
                    oob = scales::rescale_none, 
                    trans = "identity" # equivalent to ggplot2::scale_y_reverse() but create the problem of y-axis label disappearance with y.lim decreasing. Thus, do not use. Use ylim() below and after this
                ))
            }
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::theme_bw())
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::theme(
                legend.position=if(base::is.null(color.column)){"none"},
                panel.border = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(), 
                axis.ticks.y.left = ggplot2::element_line(size = 0.25), 
                # axis.line.x.top = ggplot2::element_line(size = 0.25), # is not displayed. Thus, I add a geom_hline below
                axis.line.y.left = ggplot2::element_line(size = 0.25),
                axis.title.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(), 
            ))
            # add x-axis line
            base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::geom_hline(
                yintercept = base::ifelse(
                    base::is.null(y_lim2), 
                    base::ifelse(reverse2, base::max(obs[ , bottom.y.column]), base::min(obs[ , bottom.y.column])), 
                    base::ifelse(reverse2, base::max(y_lim2), base::min(y_lim2))
                ),
                size = 0.25
            ))
            # add tick lines if vgrid is FALSE
            if( ! vgrid){
                gline = grid::linesGrob(y = base::c(1, 1.02),  gp = grid::gpar(col = "black", lwd = 0.5))
                for(i2 in base::c(xmin_plot, chr$LENGTH_CUMUL)){
                    base::assign(base::paste0(tempo.gg.name2, tempo.gg.count2 <- tempo.gg.count2 + 1), ggplot2::annotation_custom(gline, xmin = i2, xmax = i2, ymin = -Inf, ymax = Inf))
                }
            }

            fin.plot2 <- base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = base::paste(base::paste0(tempo.gg.name2, 1:tempo.gg.count2), collapse = " + ")))))
            gl <- base::lapply(X = base::list(fin.plot1, fin.plot2), FUN = ggplot2::ggplotGrob)  
            wd <- base::do.call(what = grid::unit.pmax, args = base::lapply(gl, "[[", 'widths'))
            gl <- base::lapply(gl, function(x){x[['widths']] = wd ; x})
            if( ! vgrid){
                gl[[1]]$layout$clip[gl[[1]]$layout$name=="panel"] <- "off"
                gl[[2]]$layout$clip[gl[[2]]$layout$name=="panel"] <- "off"
            }
            base::suppressMessages(base::suppressWarnings(gridExtra::grid.arrange(gl[[1]], gl[[2]], ncol=1, nrow = 2)))
        }
    }else{
        saferGG::gg_empty(text = base::paste0("NO PLOT DRAWN\nTHE x_lim PARAMETER\nMIGHT BE OUTSIDE\nOF THE RANGE OF THE VCF FILE\nOR THE RANGE OF THE region PARAMETER\nOR NULL"))
    } 
    # else already dealt above
    # end main code
}



