/* Output to GLI files.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "gli.h"
#include "flag-types.h"
#include "flags.h"
#include "options.h"
#include "version.h"
#include "vec.h"


/* True when the GLI file has already been started for the current compile
   unit.  Used to output the header only the first time the file is opened, and
   to open the file in "append" mode the next times.  */
static bool started = false;

/* True when the GLI file is opened, false when it's closed.  Used to assert
   only one is open at a time.  */
static bool opened = false;

/* True when gli_finalized has been called.  */
static bool finalized = false;

/* List of registered filenames.  */
typedef const char *filename_t;
static vec<filename_t> files;

/* Filenames couple used to sort D lines.  */
struct dependency_entry {
  /* Filename as found in line maps.  */
  filename_t map_filename;
  /* Full path as found in dependency "D" lines, in GLI files.  */
  char *dep_fullpath;
};

static void generate_dep_lines (FILE *f);

/* Open and return the GLI file corresponding to the given MAIN_FILE.  Output
   library headers if it is opened for the first time.  */
FILE *
gli_open (const char *main_file)
{
  const char *file_name = lbasename (main_file);

  if (opened)
    internal_error ("GLI file is opened twice.");
  else if (finalized)
    internal_error ("Tried to open GLI file after finalization.");

  char *gli_name = concat (file_name, ".gli", NULL);

  /* Override the GLI file if it is opened for the first time.  */
  FILE *f = fopen (gli_name, started ? "a" : "w");
  free (gli_name);

  if (!f)
    return NULL;

  /* Output the GLI header if needed.  */
  if (!started)
    {
      fprintf (f, "V \"%s\"\n", version_string);
      fprintf (f, "P\n");
      fprintf (f, "R\n");
      fprintf (f, "U %s %s\n", file_name, file_name);
      if (flag_dump_scos)
	fprintf (f, "A -fdump-scos\n");
      if (flag_preserve_control_flow)
        fprintf (f, "A -fpreserve-control-flow\n");
      if (debug_info_level >= DINFO_LEVEL_NORMAL)
        fprintf (f, "A -g\n");
      if (profile_arc_flag)
        fprintf (f, "A -fprofile-arcs\n");
      fprintf (f, "\n");
      generate_dep_lines (f);
    }

  started = true;
  opened = true;

  return f;
}

/* Close the given GLI file F.  Do not use fclose () instead, since internal
   data must be updated.  */
void
gli_close (FILE *f)
{
  fclose (f);
  opened = false;
}

/* Return FILENAME's file number, or 0 if FILENAME is not registered.  */
unsigned
gli_filenum (const char *filename)
{
  unsigned idx;
  const char *fn;

  FOR_EACH_VEC_ELT (files, idx, fn)
    {
      if (!strcmp(filename, fn))
        return idx + 1;
    }

  return 0;
}

/* Must be called when the GLI production is completely done.  No GLI file must
   be open.  Free all allocated datastructures.  */
void
gli_finalize (void)
{
  if (opened)
    internal_error ("Tried to finalize while GLI file still open.");

  files.release ();

  finalized = true;
}

/* Print some path to file F, adding quotes if needed.  */
static void
print_path (FILE *f, const char *path)
{
  /* If a not printable/space/quote character is found, quote the path.  */
  for (int i = 0; path[i] != '\0'; ++i)
    if (path[i] <= '!' || path[i] == '"' || path[i] > '~')
      {
        fputc ('"', f);
        for (i = 0; path[i] != '\0'; ++i)
          {
            fputc (path[i], f);
            /* Double existing quotes.  */
            if (path[i] == '"')
              fputc ('"', f);
          }
        fputc ('"', f);
        return;
      }

  /* If this point is reached, then no "special" character was found: print the
     path as-is.  */
  fprintf (f, "%s", path);
}

/* Return true if PATH is an absolute path. Only relevant on unix systems.  */
static bool
is_unix_absolute_path (const char *path)
{
  return path != NULL && *path == '/';
}

/* Return a normalized path from a potentially relative PATH, without resolving
   symlinks (unlike lrealpath).  */
static char *
normalize_path (const char *path)
{
#if defined (WINNT)
  return lrealpath (path);
#else
  /* Do not use lrealpath on unix systems, since we do not want to resolve
     symlinks.  */

  if (is_unix_absolute_path (path))
    return xstrdup (path);
  else
    return concat (getpwd (), "/", path, NULL);
#endif
}

/* Comparison function between DEP1 and DEP2, to be used by vec.qsort.
   Comparison is alphanumerical on the full path used in dependency lines.  */
static int
compare_dep_files (const void *dep1, const void *dep2)
{
  const dependency_entry *e1 = static_cast<const dependency_entry *> (dep1);
  const dependency_entry *e2 = static_cast<const dependency_entry *> (dep2);

  return strcmp (e1->dep_fullpath, e2->dep_fullpath);
}

/* Generate D lines for all source files referenced by ordinary line maps.  */
static void
generate_dep_lines (FILE *f)
{
  vec <dependency_entry, va_heap> deps;
  unsigned dep_no;
  dependency_entry *dep_cur;

  deps.create (0);

  /* First, build a sorted table for dependency lines.  */
  for (unsigned map_no = 0;
       map_no < LINEMAPS_USED (line_table, false);
       ++map_no)
    {
      const line_map_ordinary *map
        = LINEMAPS_ORDINARY_MAP_AT (line_table, map_no);
      const char *filename = LINEMAP_FILE (map);
      bool to_add = true;

      /* Skip built-in line maps.  */
      if (linemap_included_from(map) == 0
	  && strcmp (filename, main_input_filename))
	continue;

      /* Do not add the same filenames multiple times.  */
      FOR_EACH_VEC_ELT (deps, dep_no, dep_cur)
	{
	  if (!strcmp (filename, dep_cur->map_filename))
	    {
	      to_add = false;
	      break;
	    }
	}

      if (to_add)
	{
	  /* All strings dynamically allocated by normalize_path will be free'd
	     in the loop below.  */
	  const dependency_entry e = { filename, normalize_path (filename) };
	  deps.safe_push (e);
	}
    }

  deps.qsort (compare_dep_files);

  /* Then output D lines and fill the definitive files table.  */
  FOR_EACH_VEC_ELT (deps, dep_no, dep_cur)
    {
      char *path = dep_cur->dep_fullpath;

      fprintf (f, "D ");
      print_path (f, path ? path : dep_cur->map_filename);
      fprintf (f, " 00000000000000 00000000\n");

      if (path != NULL)
	{
	  dep_cur->dep_fullpath = NULL;
	  free (path);
	}

      files.safe_push (dep_cur->map_filename);
    }

  deps.release ();
}
