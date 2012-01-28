### ===========================================================================
### File: "configure.ac"
###                        Created: 2010-06-24 08:46:27
###              Last modification: 2012-01-28 12:49:10
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


LDFLAGS="$LDFLAGS -L/opt/local/lib"

# Check for the required libraries
AC_CHECK_LIB(pcre,pcre_free,,AC_MSG_ERROR(the pcre library is required. Please install it.))
AC_CHECK_LIB(glib-2.0, g_list_append,,AC_MSG_ERROR(the glib-2.0 library is required. Please install it.))



# Process the .in files
# ---------------------
AC_CONFIG_FILES(
[ src/Makefile
cwb/cl/MakefileRcqp
cwb/cqp/MakefileRcqp
])

AC_OUTPUT


