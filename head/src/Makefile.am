bin_PROGRAMS = xquery
xquery_SOURCES = xquery.cpp parser.cpp xquerygrammar.cpp

# set the include path found by configure
INCLUDES= $(all_includes)

# the library search path.
xquery_LDFLAGS = $(all_libraries) 
noinst_HEADERS = parser.h smartpointer.h xquerygrammar.h
lib_LIBRARIES = libsoul.a
libsoul_a_SOURCES = parser.cpp
