# This code has been used in order to patch the initial CWB code base imported
# into rcqp. Several function calls forbiden in R shared libraries have been
# replaced with calls to R functions.

# This code is not intended to be applied every time a wew version of the CWB
# code base is used. If you upgrade to a new version of the CWB code base,
# please use rather the vendor directory and the -merge mechanism of svn.

## Changes:

# exit(x) -> rcqp_receive_error(x)
# fprintf(x,...) -> Rprintf(__VA_ARGS__)
# vfprintf(x,y,z) -> Rvprintf(y,z)
# fflush(stdout) -> rcqp_flush() 
# fflush(stderr) -> rcqp_flush() 
# printf -> Rprintf
# putchar(x) -> Rprintf("%d", x)
# fputc(x, y) -> Rprintf("%d", x)
# puts -> Rprintf
# stdout -> NULL

SOURCE=../pkg/rcqp/src/cwb/
# -
#cp input.source input.txt
#SOURCE=input.txt
# -
#cp PatchCWB.test.source PatchCWB.test
#SOURCE=PatchCWB.test

find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/exit(/rcqp_receive_error(/"

# There are two distinct cases for sed to match the "fprintf" *token* (without
# matching "fprintf" as part or a larger token, such as "vfprintf") : either
# printf is at the beginning of the line, or it is preceded by a word boundary.
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/^fprintf(stderr,/Rprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/\bfprintf(stderr,/Rprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/ fprintf(stderr,/Rprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/	fprintf(stderr,/Rprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/^fprintf(stdout,/Rprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/\bfprintf(stdout,/Rprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/ fprintf(stdout,/Rprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/	fprintf(stdout,/Rprintf(/"
# some fprintf have to be corrected by hand (when there is a new line after parenthesis:
# fprintf(\n
# 3 in cwb/cl/corpus.c
#         (void) fprintf( stderr, "%s\n", msg );
# in cwb/cqp/parser.yy.c
# and
#        (void) fprintf( stderr, "%s\n", msg );
# in cwb/cl/registry.yy.c


find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/vfprintf(stderr,/Rvprintf(/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/fflush(stdout)/rcqp_flush()/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/fflush(stderr)/rcqp_flush()/"

# There are two distinct cases for sed to match the "printf" *token* (without
# matching "printf" as part or a larger token, such as "Rprintf") : either
# printf is at the beginning of the line, or it is preceded by a word boundary.
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/^printf/Rprintf/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/\<printf/Rprintf/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/ printf/Rprintf/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/	printf/Rprintf/"

find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/putchar(/Rprintf(\"%d\", /"
# I have manually check that all fputc call have no "," in the first argument.
# but args have the other order : fputc('\n', stderr); in cqp/regex2dfa.c ?
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/fputc(\([^,]\{1,\}\), stderr)/Rprintf(\"%d\",\1)/"
#find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/fputs([^;]\{1,\},\([^,]\{1,\}\));/Rprintf(\"%d\",\1/);"
# Not to be confused with fputs:
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/^puts/Rprintf/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/\<puts/Rprintf/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/ puts/Rprintf/"
find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/	puts/Rprintf/"

find $SOURCE -type f -print0 |  xargs -0 sed -i "" "s/stdout/NULL/"

