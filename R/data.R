
cacheEnv <- new.env()

getBuiltInData <- function(nm, platform='', subdir='') {
  if (platform != '') {
    datanm <- paste0(platform,'.',nm)
  } else {
    datanm <- nm
  }
  if (exists(datanm, envir=cacheEnv)) {
    return(get(datanm, envir=cacheEnv))
  }

  x <- NULL
  base.dir <- 'https://dl.dropboxusercontent.com/u/6647241/wzard/'
  if (subdir != '') {
    base.dir <- paste0(base.dir, subdir, '/');
  }

  wzhome <- Sys.getenv('WZARDHOME')
  dir.create(wzhome, showWarnings = FALSE) # make sure directory exists
  localpath <- paste0(wzhome, '/', datanm, '.rds')
  if (wzhome != "") {
    if (file.exists(localpath)) {
      x <- readRDS(localpath)
    } else {
      x <- NULL
    }
  }

  if (is.null(x)) {
    message('Caching ', datanm, '... ', appendLF=FALSE)
    x <- readRDS(gzcon(url(paste0(base.dir, datanm, '.rds'))))
    if (wzhome != "") {
      saveRDS(x, file=localpath)
    }
    message('Done.')
  }
  assign(datanm, x, envir=cacheEnv)
  x
}
