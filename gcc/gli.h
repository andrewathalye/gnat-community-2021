/* Interface for GLI files handling.
   Copyright (C) 2010-2018 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef C_GLI_H
#define C_GLI_H

/* In gli.c  */

extern FILE *gli_open (const char *main_file);
extern void gli_close (FILE *f);
extern unsigned gli_filenum (const char *filename);
extern void gli_finalize (void);

#endif
