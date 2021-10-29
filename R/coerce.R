#' @name SpatialExperiment-coercion
#' @title SCE to SpE coercion
#' 
#' @aliases 
#' coerce, SingleCellExperiment, SpatialExperiment-method
#' toSpatialExperiment
#' 
#' @description 
#' The \code{SpatialExperiment} (SpE) class inherits from the 
#' \code{SingleCellExperiment} (SCE) class, making it necessary 
#' to coerce between these classes.
#' Two different methods may be used to do this: 
#' the traditional \code{as} method and 
#' the \code{toSpatialExperiment} function (recommended).
#' 
#' The \code{as} method expects a \code{SingleCellExperiment} with 
#' valid \code{int_colData} fields \code{spatialData} and \code{spatialCoords}.
#' 
#' The \code{toSpatialExperiment} expects a \code{SingleCellExperiment} 
#' and additional arguments providing relevant spatial data (see below).
#' 
#' @param sce A \code{\link{SingleCellExperiment}} object 
#'   with populated slots of the base class.
#' @inheritParams SpatialExperiment
#' 
#' @examples 
#' dir <- system.file(
#'   file.path("extdata", "10xVisium", "section1"),
#'   package = "SpatialExperiment")
#' 
#' # read in counts
#' fnm <- file.path(dir, "raw_feature_bc_matrix")
#' sce <- DropletUtils::read10xCounts(fnm)
#' 
#' # read in spatial coordinates
#' fnm <- file.path(dir, "spatial", "tissue_positions_list.csv")
#' xyz <- read.csv(fnm, 
#'   header = FALSE,
#'   col.names = c(
#'     "barcode", "in_tissue", "array_row", "array_col",
#'     "pxl_row_in_fullres", "pxl_col_in_fullres"))
#' 
#' ###############
#' # 'as' method #
#' ###############
#' int_colData(sce)$spatialData <- DataFrame(xyz[, c(1:4)])
#' int_colData(sce)$spatialCoords <- as.matrix(xyz[, c(5, 6)])
#' 
#' # coercing a SCE without 'imgData'
#' spe <- as(sce, "SpatialExperiment")
#' 
#' # coercing a SCE with 'imgData'
#' img <- readImgData(
#'     path = file.path(dir, "spatial"),
#'     sample_id = "sample01")
#' int_colData(sce)$imgData <- img
#' spe <- as(sce, "SpatialExperiment")
#' 
#' ################################
#' # 'toSpatialExperiment' method #
#' ################################
#' (spe <- toSpatialExperiment(sce,
#'   imgData = img,
#'   spatialData = DataFrame(xyz), 
#'   spatialCoordsNames = c("pxl_col_in_fullres", "pxl_row_in_fullres"),
#'   sample_id = "sample01"))
#'   
#' @importFrom methods is
#' @importFrom SingleCellExperiment int_colData int_metadata
#' @export
toSpatialExperiment <- function(sce,
    sample_id = "sample01",
    spatialDataNames = NULL,
    spatialCoordsNames = NULL,
    spatialData = NULL,
    spatialCoords = NULL,
    scaleFactors = 1,
    imageSources = NULL,
    image_id = NULL,
    loadImage = TRUE,
    imgData = NULL) {
    
    stopifnot(is(sce, "SingleCellExperiment"))
    
    # if unspecified, try to get these from the SCE
    if (is.null(imgData)) imgData <- int_metadata(sce)$imgData
    if (is.null(spatialData)) spatialData <- int_colData(sce)$spatialData
    if (is.null(spatialCoords)) spatialCoords <- int_colData(sce)$spatialCoords

    spe <- .sce_to_spe(
        sce = sce,
        sample_id = sample_id,
        spatialDataNames = spatialDataNames,
        spatialCoordsNames = spatialCoordsNames,
        spatialData = spatialData,
        spatialCoords = spatialCoords,
        scaleFactors = scaleFactors,
        imageSources = imageSources,
        image_id = image_id,
        loadImage = loadImage,
        imgData = imgData)
    
    return(spe)
}

setAs(
    from="SingleCellExperiment", 
    to="SpatialExperiment", 
    function(from) {
        .sce_to_spe(from, 
            imgData=int_metadata(from)$imgData,
            spatialData=int_colData(from)$spatialData,
            spatialCoords=int_colData(from)$spatialCoords)
    }
)
