

types <- function (attribute, ...) UseMethod("types");
regions <- function (attribute, ...) UseMethod("regions");
tokens <- function (attribute, ...) UseMethod("tokens");

size <- function (x) UseMethod("size");


ntypes <- function (attribute, ...) UseMethod("ntypes");
nvalues <- function (attribute, ...) UseMethod("nvalues");
ntokens <- function (attribute, ...) UseMethod("ntokens");
nregions <- function (attribute, ...) UseMethod("nregions");



region_sizes <- function (attribute) UseMethod("region_sizes");

cqp_flist <- function(x, ...) UseMethod("cqp_flist");
cqp_ftable <- function(x, ...) UseMethod("cqp_ftable");
cqp_kwic <- function(x, ...) UseMethod("cqp_kwic");
