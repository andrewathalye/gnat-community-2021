/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S T A N D A R D                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *                       Copyright (C) 2015-2021, AdaCore                   *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

#ifndef STANDARD_H
#define STANDARD_H

#ifdef __vxworks
#include "vxWorks.h"
#endif

/* stdint.h is only guaranteed to be there starting with C99 */
#if __STDC_VERSION__ >= 199901L
#include <stdint.h>   /* for [u]int*_t */
#endif

#ifndef CCG_NO_STD_INCLUDE

#include <string.h>
/* for memcpy(), memcmp(), used to implement assignments and comparisons
   between composite objects.  */
#include <math.h>
/* for fabs*() - Ada abs operator on floating point
   isfinite() - 'Valid on floating point
   powl() - "**" operator on floating point
   Ada.Numerics package hierarchy.  */

#ifndef __USE_ISOC99
#define __USE_ISOC99
/* for enabling llabs() - implementation of abs operator on long long int */
#endif
#include <stdlib.h>
/* for *abs() - abs operator on integers
   malloc()  - Dynamic memory allocation via "new".  */

#endif  /* CCG_NO_STD_INCLUDE */

/* Allow overriding GNAT_INLINE from the command line, and default to "inline"
   which is suitable for almost all C compilers in practice. */
#ifndef GNAT_INLINE
#define GNAT_INLINE inline
#endif

/* Definition of Standard.Boolean type and True/False values */

#if __STDC_VERSION__ >= 199901L
#include <stdbool.h>

typedef bool boolean;

#else

typedef unsigned char boolean;

#ifndef false
#define false 0
#endif

#ifndef true
#define true 1
#endif
#endif

/* Definition of various C extensions needed to support alignment and packed
   clauses, as well as pragma Linker_Section and Machine_Attribute.
   These are supported out of the box with a GNU C compatible compiler, and
   are defined by default as no-op otherwise.  */

#ifdef __GNUC__
#define GNAT_ALIGN(ALIGNMENT) __attribute__((aligned(ALIGNMENT)))
#define GNAT_ALIGNOF(ENTITY) __alignof__(ENTITY)
#define GNAT_PACKED __attribute__ ((packed))
#define GNAT_LINKER_SECTION(SECTION) __attribute__((section(SECTION)))
#define GNAT_ATTRIBUTE(ATTR) __attribute__((ATTR))
#define GNAT_ATTRIBUTE_INFO(ATTR,INFO) __attribute__((ATTR(INFO)))
#else
#define GNAT_ALIGN(ALIGNMENT)
#define GNAT_ALIGNOF(ENTITY)
#define GNAT_PACKED
#define GNAT_LINKER_SECTION(SECTION)
#define GNAT_ATTRIBUTE(ATTR)
#define GNAT_ATTRIBUTE_INFO(ATTR,INFO)
#endif

/* Definition of entities needed by the code generator to define base types */

#ifdef INT8_MIN  /* stdint.h supported */
typedef uint8_t unsigned_8;
typedef uint16_t unsigned_16;
typedef uint32_t unsigned_32;
typedef uint64_t unsigned_64;

typedef int8_t integer_8;
typedef int16_t integer_16;
typedef int32_t integer_32;
typedef int64_t integer_64;

typedef intptr_t integer_ptr_t;
#else
typedef unsigned char unsigned_8;
typedef unsigned short unsigned_16;
typedef unsigned unsigned_32;
typedef unsigned long long unsigned_64;

typedef signed char integer_8;
typedef short integer_16;
typedef int integer_32;
typedef long long integer_64;

typedef long integer_ptr_t;
#endif

/* Definition of entities defined in package Standard */

typedef int integer;

typedef int natural;
typedef int positive;

typedef signed char short_short_integer;
typedef short short_integer;
typedef long long_integer;
typedef long long long_long_integer;
typedef long long long_long_long_integer;
typedef long long universal_integer;

typedef unsigned char short_short_unsigned;
typedef unsigned short short_unsigned;
typedef unsigned long long long_long_unsigned;
typedef unsigned long long long_long_long_unsigned;

typedef float short_float;
typedef double long_float;
typedef long double long_long_float;
typedef long double universal_real;

typedef unsigned char character;
typedef unsigned_16 wide_character;
typedef unsigned_32 wide_wide_character;

typedef character *access_character;
typedef character *string;
typedef wide_character *wide_string;
typedef wide_wide_character *wide_wide_string;

typedef integer_32 duration;

/* Definitions needed by exception handling */

typedef void *_void_ptr;
extern void __gnat_last_chance_handler (const _void_ptr file, integer line);

#ifndef GNAT_LAST_CHANCE_HANDLER
#define GNAT_LAST_CHANCE_HANDLER(FILE,LINE) \
	__gnat_last_chance_handler(FILE, LINE)
#endif

/* Fat pointer of unidimensional unconstrained arrays */

typedef struct {
  void *all;
  integer_ptr_t first;
  integer_ptr_t last;
} _fatptr_UNCarray;

static GNAT_INLINE _fatptr_UNCarray
_fatptr_UNCarray_CONS (void *all, integer_ptr_t first, integer_ptr_t last)
{
  _fatptr_UNCarray fatptr;
  fatptr.all   = all;
  fatptr.first = first;
  fatptr.last  = last;
  return fatptr;
}

/* Support for 'Max and 'Min attributes on all integer sizes */

static GNAT_INLINE integer_8
_imax8(const integer_8 a, integer_8 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE integer_16
_imax16(const integer_16 a, integer_16 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE integer_32
_imax32(const integer_32 a, integer_32 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE integer_64
_imax64(const integer_64 a, const integer_64 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE integer_8
_imin8(const integer_8 a, const integer_8 b)
{
   return (b < a) ? b : a;
}

static GNAT_INLINE integer_16
_imin16(const integer_16 a, const integer_16 b)
{
   return (b < a) ? b : a;
}

static GNAT_INLINE integer_32
_imin32(const integer_32 a, const integer_32 b)
{
   return (b < a) ? b : a;
}

static GNAT_INLINE integer_64
_imin64(const integer_64 a, const integer_64 b)
{
   return (b < a) ? b : a;
}

static GNAT_INLINE unsigned_8
_umax8(const unsigned_8 a, unsigned_8 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE unsigned_16
_umax16(const unsigned_16 a, unsigned_16 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE unsigned_32
_umax32(const unsigned_32 a, unsigned_32 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE unsigned_64
_umax64(const unsigned_64 a, const unsigned_64 b)
{
   return (b > a) ? b : a;
}

static GNAT_INLINE unsigned_8
_umin8(const unsigned_8 a, const unsigned_8 b)
{
   return (b < a) ? b : a;
}

static GNAT_INLINE unsigned_16
_umin16(const unsigned_16 a, const unsigned_16 b)
{
   return (b < a) ? b : a;
}

static GNAT_INLINE unsigned_32
_umin32(const unsigned_32 a, const unsigned_32 b)
{
   return (b < a) ? b : a;
}

static GNAT_INLINE unsigned_64
_umin64(const unsigned_64 a, const unsigned_64 b)
{
   return (b < a) ? b : a;
}

#endif /* STANDARD_H */
