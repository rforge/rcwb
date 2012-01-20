### ===========================================================================
### File: "configure.ac"
###                        Created: 2010-06-24 08:46:27
###              Last modification: 2010-06-24 19:34:44
### Author: Bernard Desgraupes
### e-mail: <bernard.desgraupes@u-paris10.fr>
### ===========================================================================
###
### Process this file with autoconf to produce a configure script.
###


AC_PREREQ(2.60)

AC_INIT([rcqp],[1.0])


# # Define and declare substitutable variables
# : ${R_HOME=`R RHOME`}
# if test -z "${R_HOME}"; then
#     echo "could not determine R_HOME"
#     exit 1
# fi
# R_INCLUDE_DIR=`R CMD config --cppflags | sed 's/-I//g'`
# LIBR=`R CMD config --ldflags`
# LAPACK_LIBS=`R CMD config LAPACK_LIBS`

# AC_SUBST(R_HOME)
# AC_SUBST(R_INCLUDE_DIR)
# AC_SUBST(LIBR)
# AC_SUBST(LAPACK_LIBS)


# C support
# ---------
AC_PROG_CC()
AC_PROG_LEX()
AC_PROG_YACC()


# # Fortran support
# # ---------------
# AC_PROG_FC()
# AC_FC_WRAPPERS()
# AC_FC_LIBRARY_LDFLAGS()


# Process the .in files
# ---------------------
AC_CONFIG_FILES(
[ src/Makefile
cwb/cl/MakefileRcqp
cwb/cqp/MakefileRcqp
])

AC_OUTPUT


