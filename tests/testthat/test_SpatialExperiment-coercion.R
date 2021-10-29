# mock up some SCE, 'imgData', 'spatialData' & '-Coords'
example(SingleCellExperiment, echo=FALSE)
spi <- new(
    "LoadedSpatialImage", 
    image = as.raster(matrix(0, 10, 10)))
id <- DataFrame(
    sample_id = "foo", 
    image_id = "foo", 
    data = I(list(spi)),
    scaleFactor = 1)
sd <- DataFrame(replicate(n <- 5, seq(ncol(sce))))
names(sd) <- letters[seq_len(n)]
xy <- matrix(0, ncol(sce), 2)
colnames(xy) <- c("a", "b")

.test <- function(fun, method) {
    # missing 'spatialData' & '-Coords'
    switch(method,
        as = expect_error(fun(sce)),
        to = {
            # empty coercion
            spe <- toSpatialExperiment(.sce)
            expect_true(isEmpty(imgData(spe)))
            expect_true(isEmpty(spatialData(spe)))
            expect_true(isEmpty(spatialCoords(spe)))
            expect_true(is.character(spe$sample_id))
        })
    int_colData(sce)$spatialData <- sd
    int_colData(sce)$spatialCoords <- xy
    
    # missing 'imgData'
    switch(method,
        as = expect_message(spe <- fun(sce)),
        to = expect_silent(spe <- fun(sce)))
    expect_true(isEmpty(imgData(spe)))
    
    # complete coercion
    sce$sample_id <- "foo"
    int_metadata(sce)$imgData <- id
    expect_silent(spe <- fun(sce))
    expect_s4_class(spe, "SpatialExperiment")
    expect_identical(imgData(spe), id)
    expect_identical(spatialData(spe), sd)
    expect_identical(spatialCoords(spe), xy)
    expect_identical(sce$sample_id, spe$sample_id)
    expect_identical(spatialDataNames(spe), names(sd))
    expect_identical(spatialCoordsNames(spe), colnames(xy))
}

test_that("as(SCE, SpE)", .test(\(.) as(., "SpatialExperiment"), "as"))
test_that("toSpatialExperiment()", .test(\(.) toSpatialExperiment(.), "to"))
