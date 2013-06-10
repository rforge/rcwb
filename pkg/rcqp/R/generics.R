.cqp_name <- function (x, ...) UseMethod(".cqp_name");

types <- function (attribute, ...) UseMethod("types");
regions <- function (attribute, ...) UseMethod("regions");
tokens <- function (attribute, ...) UseMethod("tokens");

size <- function (x) UseMethod("size");


ntype <- function (attribute, ...) UseMethod("ntype");
nvalue <- function (attribute, ...) UseMethod("nvalue");
ntoken <- function (attribute, ...) UseMethod("ntoken");
nregion <- function (attribute, ...) UseMethod("nregion");

region_sizes <- function (attribute) UseMethod("region_sizes");

cqp_flist <- function(x, ...) UseMethod("cqp_flist");
cqp_ftable <- function(x, ...) UseMethod("cqp_ftable");
cqp_kwic <- function(x, ...) UseMethod("cqp_kwic");
