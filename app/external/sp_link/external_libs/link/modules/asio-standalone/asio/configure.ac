AC_INIT(asio, [1.28.0])
AC_CONFIG_SRCDIR(include/asio.hpp)
AM_MAINTAINER_MODE
AM_INIT_AUTOMAKE([tar-pax])

AC_CANONICAL_HOST
AM_PROG_CC_C_O
AC_PROG_CXX
AC_LANG(C++)
AC_PROG_RANLIB
PKG_INSTALLDIR

AC_DEFINE(_REENTRANT, [1], [Define this])

AC_ARG_WITH(boost,
  AC_HELP_STRING([--with-boost=DIR],[location of boost distribution]),
[
  if test "${withval}" = no; then
    STANDALONE="yes"
  else
    if test "${withval}" != system; then
      CPPFLAGS="$CPPFLAGS -I${withval}"
      LIBS="$LIBS -L${withval}/stage/lib"
    fi
    CPPFLAGS="$CPPFLAGS -DASIO_ENABLE_BOOST -DBOOST_CHRONO_HEADER_ONLY -DBOOST_CHRONO_DONT_PROVIDE_HYBRID_ERROR_HANDLING"
  fi
],
[
  STANDALONE="yes"
])

AC_ARG_ENABLE(separate-compilation,
[  --enable-separate-compilation  separate compilation of asio source],
[
  SEPARATE_COMPILATION=yes
])

AC_ARG_ENABLE(boost-coroutine,
[  --enable-boost-coroutine  use Boost.Coroutine to implement stackful coroutines],
[
  HAVE_BOOST_COROUTINE=yes
])

if test "$STANDALONE" != yes; then
  AC_CHECK_HEADER([boost/noncopyable.hpp],,
  [
    echo "Can't find boost headers. Please check the location of the boost"
    echo "distribution and rerun configure using the --with-boost=DIR option."
    echo "Alternatively, run with --without-boost to enable standalone build."
    exit 1
  ],[])
fi

AC_ARG_WITH(openssl,
  AC_HELP_STRING([--with-openssl=DIR],[location of openssl]),
[
  CPPFLAGS="$CPPFLAGS -I${withval}/include"
  LIBS="$LIBS -L${withval}/lib"
],[])

AC_CHECK_HEADER([openssl/ssl.h],,
[
  OPENSSL_FOUND=no
],[])

if test x$OPENSSL_FOUND != xno; then
  LIBS="$LIBS -lssl -lcrypto"
fi

AM_CONDITIONAL(HAVE_OPENSSL,test x$OPENSSL_FOUND != xno)

WINDOWS=no
case $host in
  *-*-linux*)
    CXXFLAGS="$CXXFLAGS -pthread"
    LDFLAGS="$LDFLAGS -pthread"
    LIBS="$LIBS -lrt"
    ;;
  *-*-solaris*)
    if test "$GXX" = yes; then
      CXXFLAGS="$CXXFLAGS -D_PTHREADS"
    else
      # We'll assume Sun's CC.
      CXXFLAGS="$CXXFLAGS -mt"
    fi
    LIBS="$LIBS -lsocket -lnsl -lpthread"
    ;;
  *-*-mingw32*)
    CXXFLAGS="$CXXFLAGS -mthreads"
    LDFLAGS="$LDFLAGS -mthreads"
    LIBS="$LIBS -lws2_32 -lmswsock"
    WINDOWS=yes
    ;;
  *-*-mingw64*)
    CXXFLAGS="$CXXFLAGS -mthreads"
    LDFLAGS="$LDFLAGS -mthreads"
    LIBS="$LIBS -lws2_32 -lmswsock"
    WINDOWS=yes
    ;;
  *-pc-cygwin*)
    CXXFLAGS="$CXXFLAGS -D__USE_W32_SOCKETS -D_WIN32_WINNT=0x0601"
    LIBS="$LIBS -lws2_32 -lmswsock"
    WINDOWS=yes
    ;;
  *-apple-darwin*)
    CXXFLAGS="$CXXFLAGS"
    LDFLAGS="$LDFLAGS"
    ;;
  *-*-freebsd*)
    CXXFLAGS="$CXXFLAGS -pthread"
    LDFLAGS="$LDFLAGS -pthread"
    ;;
  *-*-netbsd*)
    CXXFLAGS="$CXXFLAGS -pthread"
    LDFLAGS="$LDFLAGS -pthread"
    ;;
  *-*-haiku*)
    CXXFLAGS="$CXXFLAGS -lnetwork"
    LDFLAGS="$LDFLAGS -lnetwork"

esac

if test "$GXX" = yes; then
  CXXFLAGS="$CXXFLAGS -ftemplate-depth-256"
fi

if test "$STANDALONE" = yes; then
  CPPFLAGS="$CPPFLAGS -DASIO_STANDALONE"
fi

if test "$SEPARATE_COMPILATION" = yes; then
  CPPFLAGS="$CPPFLAGS -DASIO_SEPARATE_COMPILATION"
fi

AC_MSG_CHECKING([whether C++11 is enabled])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(
    [[#if __cplusplus < 201103L]]
    [[#error C++11 not available]]
    [[#endif]])],
  [AC_MSG_RESULT([yes])
    HAVE_CXX11=yes;],
  [AC_MSG_RESULT([no])
    HAVE_CXX11=no;])

AC_MSG_CHECKING([whether C++14 is enabled])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(
    [[#if defined(__GNUC__) && !defined(__clang__)]]
    [[# if (__GNUC__ <= 6)]]
    [[#  error C++14 support on this compiler not sufficiently compliant]]
    [[# endif]]
    [[#endif]]
    [[#if __cplusplus < 201402L]]
    [[#error C++14 not available]]
    [[#endif]])],
  [AC_MSG_RESULT([yes])
    HAVE_CXX14=yes;],
  [AC_MSG_RESULT([no])
    HAVE_CXX14=no;])

AC_MSG_CHECKING([whether C++17 is enabled])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(
    [[#if __cplusplus < 201703L]]
    [[#error C++17 not available]]
    [[#endif]])],
  [AC_MSG_RESULT([yes])
    HAVE_CXX17=yes;],
  [AC_MSG_RESULT([no])
    HAVE_CXX17=no;])

AC_MSG_CHECKING([whether C++20 is enabled])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(
    [[#if __cplusplus < 202002L]]
    [[#error C++20 not available]]
    [[#endif]])],
  [AC_MSG_RESULT([yes])
    HAVE_CXX20=yes;],
  [AC_MSG_RESULT([no])
    HAVE_CXX20=no;])

AC_MSG_CHECKING([whether coroutines are enabled])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(
    [[#if defined(__clang__)]]
    [[# if (__clang_major__ >= 14)]]
    [[#  if (__cplusplus >= 202002) && (__cpp_impl_coroutine >= 201902)]]
    [[#   if __has_include(<coroutine>)]]
    [[#    define ASIO_HAS_CO_AWAIT 1]]
    [[#   endif]]
    [[#  elif (__cplusplus >= 201703) && (__cpp_coroutines >= 201703)]]
    [[#   if __has_include(<experimental/coroutine>)]]
    [[#    define ASIO_HAS_CO_AWAIT 1]]
    [[#   endif]]
    [[#  endif]]
    [[# else]]
    [[#  if (__cplusplus >= 201703) && (__cpp_coroutines >= 201703)]]
    [[#   if __has_include(<experimental/coroutine>)]]
    [[#    define ASIO_HAS_CO_AWAIT 1]]
    [[#   endif]]
    [[#  endif]]
    [[# endif]]
    [[#elif defined(__GNUC__)]]
    [[# if (__cplusplus >= 201709) && (__cpp_impl_coroutine >= 201902)]]
    [[#  if __has_include(<coroutine>)]]
    [[#   define ASIO_HAS_CO_AWAIT 1]]
    [[#  endif]]
    [[# endif]]
    [[#endif]]
    [[#ifndef ASIO_HAS_CO_AWAIT]]
    [[# error coroutines not available]]
    [[#endif]])],
  [AC_MSG_RESULT([yes])
    HAVE_COROUTINES=yes;],
  [AC_MSG_RESULT([no])
    HAVE_COROUTINES=no;])

if test "$GXX" = yes; then
  if test "$STANDALONE" = yes; then
    if test "$HAVE_CXX11" = no; then
      HAVE_CXX11=yes
      CPPFLAGS="-std=c++0x $CPPFLAGS"
    fi
  fi
fi

AM_CONDITIONAL(STANDALONE,test x$STANDALONE = xyes)

AM_CONDITIONAL(SEPARATE_COMPILATION,test x$SEPARATE_COMPILATION = xyes)

AM_CONDITIONAL(HAVE_BOOST_COROUTINE,test x$HAVE_BOOST_COROUTINE = xyes)

AM_CONDITIONAL(WINDOWS_TARGET,test x$WINDOWS != xno)

AM_CONDITIONAL(HAVE_CXX11,test x$HAVE_CXX11 = xyes)

AM_CONDITIONAL(HAVE_CXX14,test x$HAVE_CXX14 = xyes)

AM_CONDITIONAL(HAVE_CXX17,test x$HAVE_CXX17 = xyes)

AM_CONDITIONAL(HAVE_CXX20,test x$HAVE_CXX20 = xyes)

AM_CONDITIONAL(HAVE_COROUTINES,test x$HAVE_COROUTINES = xyes)

AC_CONFIG_FILES([asio.pc])

AC_OUTPUT([
  Makefile
  include/Makefile
  src/Makefile
  src/tests/Makefile
  src/tests/properties/Makefile
  src/examples/cpp03/Makefile
  src/examples/cpp11/Makefile
  src/examples/cpp14/Makefile
  src/examples/cpp17/Makefile
  src/examples/cpp20/Makefile])
