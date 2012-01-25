### ===========================================================================
### File: "configure.ac"
###                        Created: 2010-06-24 08:46:27
###              Last modification: 2012-01-25 10:41:35
### Author: Bernard Desgraupes
### e-mail: <bernard.desgraupes@u-paris10.fr>
### ===========================================================================
###
### Process this file with autoconf to produce a configure script.
###


AC_PREREQ(2.60)

AC_INIT([rcqp],[1.0])


# C support
# ---------
AC_PROG_CC()
AC_PROG_LEX()
AC_PROG_YACC()

# AC_CHECK_LIB(pcre,pcre_free,,AC_MSG_ERROR(please install pcre library))

# Process the .in files
# ---------------------
AC_CONFIG_FILES(
[ src/Makefile
cwb/cl/MakefileRcqp
cwb/cqp/MakefileRcqp
])

AC_OUTPUT


