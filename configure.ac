AC_INIT([Chil], [0.1])
AC_CONFIG_SRCDIR([source])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign])

# Checks for a Lisp program.
AC_SUBST(LISP_DIALECT)
AC_ARG_WITH(lisp,    [  --with-lisp=IMPL        use the specified lisp dialect],
            [LISP_DIALECT=$withval],
            [LISP_PROGRAM="sbcl"])

AC_PATH_PROG(LISP_PROGRAM, $LISP_DIALECT, no)
if test "$LISP_PROGRAM" = "no"; then
   AC_MSG_ERROR([$LISP_DIALECT is not available. Please install it or make it available in your \$PATH.])
fi

# Check for makeinfo
AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)

if test "$MAKEINFO" = "no"; then
   AC_MSG_WARN([You do not seem to have makeinfo, so you will not be able
to build the manuals. Please install makeinfo for the manual.])
   elif test "$MAKEINFO" = "yes"; then
   MAKEINFO=makeinfo
fi

AC_SUBST([MAKEINFO])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
