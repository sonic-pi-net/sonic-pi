/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_GLOBAL_H
#define QWT_GLOBAL_H

#include <qglobal.h>

// QWT_VERSION is (major << 16) + (minor << 8) + patch.

#define QWT_VERSION       0x060104
#define QWT_VERSION_STR   "6.1.4"

#if defined(_MSC_VER) /* MSVC Compiler */
/* template-class specialization 'identifier' is already instantiated */
#pragma warning(disable: 4660)
/* inherits via dominance */
#pragma warning(disable: 4250)
#endif // _MSC_VER

#ifdef QWT_DLL

#if defined(QWT_MAKEDLL)     // create a Qwt DLL library
#define QWT_EXPORT Q_DECL_EXPORT
#else                        // use a Qwt DLL library
#define QWT_EXPORT Q_DECL_IMPORT
#endif

#endif // QWT_DLL

#ifndef QWT_EXPORT
#define QWT_EXPORT
#endif

#endif
