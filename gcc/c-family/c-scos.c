/* Output Source Coverage Obligations (SCOs) for C and C++.
   Copyright (C) 2017-2018
   Free Software Foundation, Inc.

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
#include "input.h"
#include "gli.h"
#include "toplev.h"
#include "tm.h"
#include "bitmap.h"
#include "tree.h"
#include "tree-iterator.h"
#include "hash-set.h"
#include "vec.h"
#include "version.h"
#include "options.h"

#include "c-family/c-scos.h"

/* This source file is made up of two parts.  First, some helpers for GENERIC
   trees walking, then the SCOs generation itself, which takes advantage of the
   tree walking helpers.  */

/*  -----------------------------------------------------------------------  *
 *                            Tree walking facilities                        *
 *  -----------------------------------------------------------------------  */

/* This part contains code that walk through the GENERIC tree, but is not
   praticularly tied to the SCOs generation.

   The walk_tree function from tree.h is not used since information gathering
   in the context of SCOs generation is not powerful enough with it (eg. it is
   very hard to keep track of the depth with walk_tree).  */

struct walk_context;

/* In all the functions below, subtrees passed in arguments can be NULL, since
   NULL is an empty subtree, and as such is a legit value.  Thus, all
   function must handle the NULL case.  */

/* These constants are used both as flags to specify a set of traversal orders
   and as single traversal order when needed.  */
#define PREFIX_ORDER  (1 << 0)
#define INFIX_ORDER   (1 << 1)
#define POSTFIX_ORDER (1 << 2)

/* Callback type used by walk_tree to let the user process tree nodes.  It
   takes the node to process, the walking context and returns whether its
   subtrees must be skipped by walk_tree.  */
typedef bool (*tree_handler) (tree, struct walk_context *);

/* Callback type used for deferred actions in the tree traversal.  */
typedef void (*deferred_action) (tree,
                                 struct walk_context *,
                                 unsigned,
                                 void *);

/* Linked list to hold deferred actions.  */
struct deferred_action_list
{
  unsigned traversal_orders;
  deferred_action action_callback;
  void *action_data;

  struct deferred_action_list *next;
};

/* Data structure used to hold context information when walking a tree.  */
struct walk_context
{
  /* The following fields are handled by walk_tree itself.  They
     shouldn't be meaningful to the user: they are used to implement tree
     walking features.  Information they contain can be accessed using the
     dedicated functions.  */

  /* tree node handler to call for each visited node.  */
  tree_handler handler;

  /* Set of already visited tree nodes, to avoid visiting sub-trees of
     already-visited nodes more than once.  */
  hash_set<tree> *GTY (()) visited;

  /* Registers the traversal depth.  */
  unsigned depth;

  /* Contains the parent node of the current sub-tree.  It is exclusively
     updated by scos_walk_tree_helper.  */
  tree parent_node;

  /* Contains the index of the current sub-tree considering its parent
     traversal: 0 for the first child, 1 for the second, etc.  */
  unsigned child_index;

  /* Contains deferred actions to run between and after each sub-tree
     traversal.  This list is saved and made empty at each sub-tree visit.
     User can register an entry using the scos_walk_register_deferred_action.
     This list is freed and restored when returning to the parent node.  */
  struct deferred_action_list *deferred_actions;

  /* User data, for custom context information.  */
  void *data;
};


/* Given some traversal context CTX, return the parent of the currently visited
   node.  */
static tree
scos_walk_get_parent (struct walk_context *ctx)
{
  return ctx->parent_node;
}

/* Given some traversal context CTX, return the child index of the currently
   visited node.  */
static unsigned
scos_walk_get_child_index (struct walk_context *ctx)
{
  return ctx->child_index;
}

/* Append a deferred action (ACTION_CALLBACK and ACTION_DATA) for current
   sub-trees traversal (targetted by the given context CTX) and for the given
   TRAVERSAL_ORDERS.  */
static void
scos_walk_register_deferred_action (struct walk_context *ctx,
                                    unsigned traversal_orders,
                                    deferred_action action_callback,
                                    void *action_data)
{
  struct deferred_action_list *new_action;

  new_action = (struct deferred_action_list *) xmalloc (sizeof (*new_action));
  new_action->traversal_orders = traversal_orders;
  new_action->action_callback = action_callback;
  new_action->action_data = action_data;
  new_action->next = ctx->deferred_actions;
  ctx->deferred_actions = new_action;
}

/* Browse and trigger the deferred action list of the given traversal context
   CTX to the NODE tree, only for the actions that registered for the
   TRAVERSAL_ORDER.  */
static void
scos_walk_trigger_deferred_actions (tree node,
                                    struct walk_context *ctx,
                                    unsigned traversal_order)
{
  struct deferred_action_list *list = ctx->deferred_actions;

  while (list)
    {
      if (list->traversal_orders & traversal_order)
        list->action_callback (node, ctx, traversal_order, list->action_data);
      list = list->next;
    }
}

/* Free a deferred action LIST and set given LIST to NULL.  */
static void
scos_walk_free_deferred_actions (struct deferred_action_list **list)
{
  struct deferred_action_list *next;

  while (*list)
    {
      next = (*list)->next;
      free (*list);
      *list = next;
    }
}

/* Walk on each sub-tree in the NODE tree in the prefix order using the CTX
   context.  */
static void
scos_walk_tree_helper (tree node, struct walk_context *ctx)
{
  /* Traversal context saved during sub-trees traversal.  */
  struct walk_context my_ctx;

  bool skip_subtree;

  /* First register NODE as visited and call the node handler on the given
     node.  */
  skip_subtree = ctx->handler (node, ctx);

  /* Then process recursively on each child node.  Most node kinds share the
     same topology (their child nodes are all operands: this is the `default'
     case, and other deserve a specific process.  */

  /* Save the traversal context for this node into MY_CTX.  MY_CTX will be then
     used for deferred actions.  This way, deferred actions will deal with the
     same context as the code that registered them.  */
  my_ctx = *ctx;

  /* Handler must now that the tree is empty (so the callback is invoked
     first), but we cannot inspect an empty tree's child nodes.  */
  if (node == NULL || ctx->visited->add (node) || skip_subtree)
    goto exit_traversal;

  /* Update the traversal context for the sub-trees.  */
  ctx->parent_node = node;
  ctx->child_index = 0;
  ctx->deferred_actions = NULL;
  ++ctx->depth;

  switch (TREE_CODE (node))
  {
    case STATEMENT_LIST:
      /* Statements in statement lists are stored as a list, not in operands:
         iterate on each statement.  */
      for (tree_stmt_iterator ts = tsi_start (node);
	   !tsi_end_p (ts);
	   tsi_next (&ts))
        {
          scos_walk_tree_helper (tsi_stmt (ts), ctx);
          if (!tsi_one_before_end_p (ts))
            scos_walk_trigger_deferred_actions (node, &my_ctx, INFIX_ORDER);
          ++ctx->child_index;
        }
      break;

    case TREE_VEC:
      /* Browse the vector elements in reverse order.  */
      for (int i = TREE_VEC_LENGTH (node); i >= 0; --i)
        {
          scos_walk_tree_helper (TREE_VEC_ELT (node, i), ctx);
          if (i > 0)
            scos_walk_trigger_deferred_actions (node, &my_ctx, INFIX_ORDER);
          ++ctx->child_index;
        }
      break;

    case CONSTRUCTOR:
      int i;
      constructor_elt *ce;

      FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (node), i, ce)
        {
          scos_walk_tree_helper (ce->value, ctx);
          if ((unsigned) (i + 1) < vec_safe_length (CONSTRUCTOR_ELTS (node)))
            scos_walk_trigger_deferred_actions (node, &my_ctx, INFIX_ORDER);
          ++ctx->child_index;
        }
      break;

    case BIND_EXPR:
      for (tree subnode = BIND_EXPR_VARS (node);
           subnode;
           subnode = DECL_CHAIN (subnode))
        {
          scos_walk_tree_helper (subnode, ctx);
          scos_walk_trigger_deferred_actions (node, &my_ctx, INFIX_ORDER);
          ++ctx->child_index;
        }
      scos_walk_tree_helper (BIND_EXPR_BODY (node), ctx);
      break;

    case TYPE_DECL:
      /* Type declaration has no useful expression in the context of SCOs
         generation, but type declaration is considered as a statement, thus
         calling the handler with it is enough.  */
      break;

    case VAR_DECL:
      /* Variable declarations can contain an expression to compute their
         initial value.  */
      scos_walk_tree_helper (DECL_INITIAL (node), ctx);
      break;

    case CALL_EXPR:
      /* Funcion calls have a variable number of arguments, so we cannot
         inspect their operands in a generic way.  */

      /* First browse the function-value expression.  */
      scos_walk_tree_helper (CALL_EXPR_FN (node), ctx);
      if (call_expr_nargs (node) > 0)
        scos_walk_trigger_deferred_actions (node, &my_ctx, INFIX_ORDER);
      ++ctx->child_index;

      /* Then browse the argument list.  */
      for (int i = 0; i < call_expr_nargs (node); ++i)
        {
          scos_walk_tree_helper (CALL_EXPR_ARG (node, i), ctx);
          if (i + 1 < call_expr_nargs (node))
            scos_walk_trigger_deferred_actions (node, &my_ctx, INFIX_ORDER);
          ++ctx->child_index;
        }
      break;

    default:
      /* Default case: iterate on each operands.  */
      for (int i = 0; i < tree_code_length[TREE_CODE (node)]; ++i)
        {
          scos_walk_tree_helper (TREE_OPERAND (node, i), ctx);
          if (i + 1 < tree_code_length[TREE_CODE (node)])
            scos_walk_trigger_deferred_actions (node, &my_ctx, INFIX_ORDER);
          ++ctx->child_index;
        }
      break;
  }

exit_traversal:

  scos_walk_trigger_deferred_actions (node, &my_ctx, POSTFIX_ORDER);

  /* Restore traversal context for the caller.  */
  scos_walk_free_deferred_actions (&my_ctx.deferred_actions);
  *ctx = my_ctx;
}


/* Walk on each sub-tree in the NODE tree in prefix order and call HANDLER on
   them passing user DATA around.  */
static void
scos_walk_tree (tree node, tree_handler handler, void *data)
{
  struct walk_context context;

  context.handler = handler;
  context.visited = hash_set<tree>::create_ggc (13);
  context.depth = 0;
  context.parent_node = NULL;
  context.child_index = 0;
  context.deferred_actions = NULL;
  context.data = data;

  scos_walk_tree_helper (node, &context);
}

/* Likewise, but resume a tree walk at NODE from an earlier STATE. It works
   only for a sub-tree: this is done to resume sub-trees traversal after having
   skipped some nodes.  */
static void
scos_resume_walk_tree (tree node, tree parent, struct walk_context *state)
{
  struct walk_context new_ctx;

  new_ctx = *state;
  if (parent != NULL)
    new_ctx.parent_node = parent;
  new_ctx.child_index = 0;
  new_ctx.deferred_actions = NULL;
  scos_walk_tree_helper (node, &new_ctx);
}


/*  -----------------------------------------------------------------------  *
 *                               SCO generation                              *
 *  -----------------------------------------------------------------------  */

/* Whether the first SCO is still to be emitted.  Used to insert a blank line
   before the first SCO.  */
static bool no_sco_yet = true;


/* SRC/SLOC tracking: To each source filename we associate a single SRC_ENTRY
   which might be extended to hold data of relevance for the designated
   file.  */

typedef struct {
  const char * filename;
} src_entry;

/* Every time a new source filename, designated by a linemap, is encountered,
   we allocate a src_entry and store the address in the SRC_TABLE table:  */

static src_entry ** src_table = NULL;

/* Then we memorize for each linemap index the src_table index corresponding
   to the source filename associated with the linemap structure:  */

static unsigned * linemap_to_src_index = NULL;

/* Retrieve the src_table index for the filename associated with the
   MAP linemap structure:  */

#define SRC_INDEX_FOR(MAP) \
  linemap_to_src_index[ORDINARY_MAP_INDEX((MAP), line_table)]

/* Compare !NULL filenames FN1 and FN2:  */

#define FILENAME_EQ(FN1,FN2) \
  ((FN1) == (FN2) || !filename_cmp ((FN1), (FN2)))

/* This defines a SLOC range.  We assume for the moment that all locations
   within one range are in the same input file.  */
typedef struct sloc_range
{
  location_t start;
  location_t end;
} sloc_range;


#define INITIAL_SLOC_RANGE {UNKNOWN_LOCATION, UNKNOWN_LOCATION}
static const struct sloc_range initial_sloc_range = INITIAL_SLOC_RANGE;

/* Return whether LOC1 and LOC2 belong to the same source file.  */

static bool
same_source_file (location_t loc1, location_t loc2)
{
  const line_map_ordinary *map1, *map2;

  if (loc1 == loc2)
    return true;

  if (loc1 == UNKNOWN_LOCATION || loc2 == UNKNOWN_LOCATION)
    return false;

  linemap_resolve_location
    (line_table, loc1, LRK_SPELLING_LOCATION, &map1);

  linemap_resolve_location
    (line_table, loc2, LRK_SPELLING_LOCATION, &map2);

  if (map1 == map2)
    return true;

  return SRC_INDEX_FOR(map1) == SRC_INDEX_FOR(map2);
}

/* Print SLOC in the file F in the format that is required by the SCOs
   format.  */
static void
print_sloc (FILE *f, location_t loc)
{
  expanded_location el = expand_location (loc);
  fprintf (f, "%d:%d", el.line, el.column);
}

/* Print a sloc range with a leading C (if C is not a space) in the file F.  */
static void
print_sloc_range (char c, const sloc_range *sr, FILE *f)
{
  /* Always write a heading blank.  */
  fputc (' ', f);

  if (sr->start == UNKNOWN_LOCATION)
    internal_error ("Some SCO item has an unknown location.");

  /* Bounds are not supposed to come from different line maps (and thus from
     different files.  */
  if (!same_source_file (sr->start, sr->end))
    internal_error ("Some locations come from different line maps.");

  /* Write out the line and column numbers, then see whether this is in the
     main or a different file.  */
  if (c != ' ')
    fprintf (f, "%c", c);
  print_sloc (f, sr->start);
  fprintf (f, "-");
  print_sloc (f, sr->end);
}

/* Assuming LEFT and RIGHT belong to the same source file, return whether LEFT
   is before RIGHT.  */
static bool
sloc_less_than (location_t left, location_t right)
{
  const expanded_location l = expand_location (left);
  const expanded_location r = expand_location (right);

  if (l.line < r.line)
    return true;
  else if (l.line == r.line)
    return l.column < r.column;
  else
    return false;
}

/* Expand LOC location and extend the sloc range SR to include it.  Return if
   successful.  */
static bool
expand_to_sloc_range (sloc_range *sr, location_t loc)
{
  /* Some synthetic nodes (for example: goto_expr created while expanding
     loops) have no location: just ignore them.  */
  if (loc == UNKNOWN_LOCATION)
    return false;

  /* If SR is empty, set it to LOC.  */
  if (sr->start == UNKNOWN_LOCATION)
    {
      sr->start = sr->end = loc;
      return true;
    }

  /* Otherwise, if SR bounds and LOC do not come from the same line map, ignore
     LOC and raise a warning.  */
  else if (!same_source_file (sr->start, loc))
    {
      warning_at (loc, 0,
                  "a syntactic element spreads through multiple files");
      return false;
    }

  /* Extend backward.  */
  if (sloc_less_than (loc, sr->start))
    sr->start = loc;

  /* Extend forward.  */
  else if (sloc_less_than (sr->end, loc))
    sr->end = loc;

  return true;
}

/* Extend the sloc range SR to include the location of EXPR.  */
static void
add_to_sloc_range (sloc_range *sr, tree expr)
{
  /* If we meet an empty subtree, there is obviously no location to add.  */
  if (expr == NULL)
      return;

  if (DECL_P (expr))
    expand_to_sloc_range (sr, DECL_SOURCE_LOCATION (expr));
  else
    {
      source_range sr_node = get_expr_source_range (expr);

      expand_to_sloc_range (sr, sr_node.m_start);
      expand_to_sloc_range (sr, sr_node.m_finish);
    }
}

/* Helper data structure for making a sloc range from a tree traversing.  */
struct walk_add_sloc_args
{
  /* Whether or not to include the root node into the generated sloc range.  */
  bool avoid_root;
  /* Propagated extra sloc.  */
  location_t extra_sloc;
  /* Result sloc range.  */
  sloc_range *sr;
};

/* Likewise, but as callback from a tree walk.  */
static bool
walk_add_sloc (tree node, struct walk_context *ctx)
{
  struct walk_add_sloc_args *args = (struct walk_add_sloc_args *) ctx->data;
  tree parent = scos_walk_get_parent (ctx);

  /* Skip root node if asked to, and skip NULL nodes, too.  */
  if ((parent == NULL && args->avoid_root) || node == NULL)
    return false;

  /* Expressions are supposed to always have a ready-to-go source range.
     Unfortunately, for WHILE loops expanded into COND_EXPR, for instance, the
     source range does not cover the whole condition. So we cannot rely on
     them *only*: also traverse the expression tree.  */
  if (EXPR_P (node))
    add_to_sloc_range (args->sr, node);
  /* References to declarations are materialized as these declarations
     themselves, so they have slocs that are completely unrelated to their
     embedding expressions.  Similarily, some literals have no location.  Try
     to rely on the context in this case.  */
  else if ((DECL_P (node) && TREE_CODE (node) != TYPE_DECL)
	   || CONSTANT_CLASS_P (node))
    {
      expand_to_sloc_range (args->sr, args->extra_sloc);
      return true;
    }

  /* By default, use the NODE location.  */
  add_to_sloc_range (args->sr, node);

  return false;
}

/* Extend the sloc range SR using the location of each node in the T tree.  */
static void
add_tree_sloc_range (sloc_range *sr, tree t)
{
  struct walk_add_sloc_args args;
  args.avoid_root = false;
  args.extra_sloc = UNKNOWN_LOCATION;
  args.sr = sr;
  scos_walk_tree (t, walk_add_sloc, &args);
}

/* Likewise, but with more options: AVOID_ROOT node if asked to, and fallback
   to EXTRA_SLOT for *_DECL nodes and INTEGER_CST ones.  */
static void
add_tree_sloc_range_with_extra (sloc_range *sr, tree t,
                                bool avoid_root, location_t extra_sloc)
{
  struct walk_add_sloc_args args;
  args.avoid_root = avoid_root;
  args.extra_sloc = extra_sloc;
  args.sr = sr;
  scos_walk_tree (t, walk_add_sloc, &args);
}

/* Set SR to the sloc range for the T expression tree.  */
static void
make_sloc_range (sloc_range *sr, tree t)
{
  *sr = initial_sloc_range;
  add_tree_sloc_range (sr, t);
}

/* Return whether LOC is included in the given SR sloc range.  */
static bool
sloc_in_range (location_t loc, const sloc_range *sr)
{
  return same_source_file (loc, sr->start)
         && (sr->start <= loc)
         && (sr->end   >= loc);
}

/* Information about a decision to be processed.  */
struct scos_decision
{
  /* This will be the * in the C* line prefix for this decision.  */
  char type;
  /* Source location of the decision.  */
  location_t loc;
  /* Tree to process in order to get the decision expression.  */
  tree decision_expr;
};

/* Dominance marker for the current scope: such markers are stacked when
   walking though the TREE.  */
struct scos_dom_marker
{
  /* 'T' for a TRUE outcome, 'F' for a false one, 'S' for a statement dominant
     and ' ' for no dominant.  */
  char type;
  /* Source location of the dominator.  */
  location_t loc;

  /* For one scope, points to the directly enclosing SWITCH_EXPR, if any, NULL
     otherwise.  */
  tree switch_dominator;
};

typedef struct scos_decision *scos_decision_p;

typedef struct scos_dom_marker *scos_dom_marker_p;

struct scos_buffered_stmt
{
  bool present;
  struct scos_dom_marker dom_marker;
  char cs_char;
  sloc_range sr;
};

/* Data structure to keep some state used during the SCOs generation in the
   tree traversal.  */
struct scos_context
{
  /* Output file for the SCOs.  */
  FILE *file;

  /* The filename where the last output statement comes from, or NULL.  */
  const char *current_file;

  /* Number of statements that have been output in the current statement line.
     Zero means that no statement line is started.  */
  int stmt_count;
  /* Set to true when at least 6 statements have been added to a statement line
     and it hasn't been flushed.  */
  bool stmt_continuation_line;

  struct scos_buffered_stmt buffered_stmt;

  /* Decisions are handled (= processed and printed) *after* the statement
     sequence they belong to is processed.  This is used to accumulate
     reference to decisions to process when detecting them, and this should be
     flushed each time the statement sequence is stopped.  */
  vec<scos_decision_p> *deferred_decisions;

  /* Dominance markers are stacked for each met scope.  */
  vec<scos_dom_marker_p> *dom_markers;

  /* Set of COND_EXPR nodes that have been considered as being part of an
     expanded WHILE statement.  */
  hash_set<tree> while_cond;
  /* True if we just left a GOTO_EXPR that is part of a WHILE statement and set
     to False as soon as the next statement is met.  It is used to avoid the
     first label of the expanded WHILE loop in order to add a dominance marker
     to the first statement of a while loop.  */
  bool while_first_label_expected;

  /* Whether the current subtree belongs to an expression.  */
  bool in_expression;
  /* Likewise, but for decisions.  */
  unsigned decision_level;
};

static void flush_stmt_line (struct scos_context *scos_ctx);

/* Set attributes of the last-in dominance marker.  */
static void
set_dom_marker (char type,
                location_t loc,
                struct scos_context *scos_ctx)
{
  scos_dom_marker_p &dom = scos_ctx->dom_markers->last();

  dom->type = type;
  dom->loc = loc;
}

static tree *
switch_dominator (struct scos_context *scos_ctx)
{
  scos_dom_marker_p &dom = scos_ctx->dom_markers->last();
  return &dom->switch_dominator;
}

/* Push a new dominance marker slot for a nested scope.  Initially, the
   dominance marker is an invalid one (= it will not be output, so keeping it
   as-is is safe).  */
static void
push_dom_marker (struct scos_context *scos_ctx)
{
  scos_dom_marker_p dom =
    (scos_dom_marker_p) xmalloc (sizeof (scos_dom_marker));

  scos_ctx->dom_markers->safe_push(dom);
  set_dom_marker (' ', UNKNOWN_LOCATION, scos_ctx);
  *(switch_dominator (scos_ctx)) = NULL;
}

static void
pop_dom_marker (struct scos_context *scos_ctx)
{
  free (scos_ctx->dom_markers->pop());
}

/* Mark the last (if not ALL) dominance marker as invalid.  Invalidating all
   markers also affects the switch dominators.  Also flush the current
   statements sequence, since it contains implicit dominance markers.  */
static void
invalidate_dom_marker (bool all, struct scos_context *scos_ctx)
{
  scos_dom_marker_p dom;
  int i;

  if (all)
    {
      FOR_EACH_VEC_ELT (*scos_ctx->dom_markers, i, dom)
        {
          dom->type = ' ';
          dom->switch_dominator = NULL;
        }
    }
  else
    {
      dom = scos_ctx->dom_markers->last();
      dom->type = ' ';
      dom->switch_dominator = NULL;
    }

  flush_stmt_line (scos_ctx);
}

/* Tree_walk helper used to process most of the tree nodes during the SCOs
   generation.  */
static bool
generate_stmts_scos_helper (tree node, struct walk_context *ctx);

/* Append a deferred decision to the SCOS_CTX context.  */
static void
defer_decision (char type, location_t loc, tree decision_expr,
                struct scos_context *scos_ctx)
{
  scos_decision_p d
    = (struct scos_decision *) xmalloc (sizeof (*d));

  d->type = type;
  d->loc = loc;
  d->decision_expr = decision_expr;
  scos_ctx->deferred_decisions->safe_push (d);
}

/* Helper data structure for printing a SCO decision tree.  */
struct print_decision_subtree_args
{
  /* The decision being processed.  */
  struct scos_decision *decision;
  /* File to print to.  */
  FILE *f;
  /* Propagated extra sloc.  */
  location_t extra_sloc;
};

/* Helper for print_decision_subtree: print the prefix representation of the
   given NODE boolean expression, given the traversal context CTX.  */
static bool
print_decision_subtree_helper (tree node, struct walk_context *ctx)
{
  sloc_range sr = INITIAL_SLOC_RANGE;
  char type;
  int tree_code;

  struct print_decision_subtree_args *args =
    (struct print_decision_subtree_args *) ctx->data;
  FILE *f = args->f;

  tree parent = scos_walk_get_parent (ctx);
  unsigned child_index = scos_walk_get_child_index (ctx);

  if (node == NULL)
    internal_error ("Invalid decision sub-expression (null).");

  tree_code = TREE_CODE (node);

  switch (tree_code)
  {
    /* There can be NOP_EXPR nodes inside decision trees: just skip them.  */
    case NOP_EXPR:
      break;

    case TRUTH_NOT_EXPR:
      fprintf (f, " !");
      print_sloc (f, EXPR_LOCATION (node));
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      if (tree_code == TRUTH_ANDIF_EXPR)
        type = '&';
      else
        type = '|';
      fprintf (f, " %c", type);
      print_sloc (f, EXPR_LOCATION (node));
      break;

    /* The following cases are where the recursion ends: a node that is not
       part of the decision is found.  */

    default:
      {
	location_t extra_sloc = UNKNOWN_LOCATION;

	/* References to declarations are materialized as these declarations
	   themselves, so they have slocs that are completely unrelated to
	   their embedding expressions.  Similarily, some literals have no
	   location.  Try to rely on the context in this case.  */
	if (parent != NULL_TREE)
	  {
	    switch (TREE_CODE (parent))
	      {
	      case TRUTH_ANDIF_EXPR:
	      case TRUTH_ORIF_EXPR:
		{
		  source_range sr = get_expr_source_range (parent);

		  switch (child_index)
		    {
		    case 0:
		      extra_sloc = sr.m_start;
		      break;
		    case 1:
		      extra_sloc = sr.m_finish;
		      break;
		    default:
		      gcc_unreachable ();
		    }
		  break;
		}

	      case TRUTH_NOT_EXPR:
		extra_sloc = EXPR_LOCATION (parent);
		break;

	      default:
		break;
	      }
	  }

	if (TREE_CODE (node) == INTEGER_CST)
	  {
	    /* An INTEGER_CST node has no location: just in case we cannot get
	       use extra locations for them.  Fallback on the decision location
	       if extra locations do not help.  */
	    if (extra_sloc == UNKNOWN_LOCATION)
	      sr.start = sr.end = args->decision->loc;
	    else
	      expand_to_sloc_range (&sr, extra_sloc);

	    if (!tree_fits_shwi_p (node) || TREE_INT_CST_LOW (node))
	      type = 't';
	    else
	      type = 'f';
	    print_sloc_range (type, &sr, f);
	    return true;
	  }

	/* If the location of this node is the same as the one of the parent,
	   this is probably a generated node: do not include it in the
	   condition sloc range.  */
	if (parent != NULL && EXPR_LOCATION (node) == EXPR_LOCATION (parent))
	  add_tree_sloc_range_with_extra (&sr, node, true, extra_sloc);
	else
	  {
	    bool extra_sloc_needed
	      = DECL_P (node) || TREE_CODE (node) == INTEGER_CST;
	    add_tree_sloc_range_with_extra (&sr, node, false,
					    (extra_sloc_needed
					     ? extra_sloc
					     : UNKNOWN_LOCATION));
	  }

	print_sloc_range ('c', &sr, f);
	return true;
      }
  }

  return false;
}

/* Print a decision sub-expression NODE to the file F.  */
static void
print_decision_subtree (struct scos_decision *d, FILE *f)
{
  struct print_decision_subtree_args args;
  args.decision = d;
  args.f = f;
  args.extra_sloc = UNKNOWN_LOCATION;
  scos_walk_tree (d->decision_expr, print_decision_subtree_helper, &args);
}

/* Process the deferred decision D with respects to the SCOS_CTX context.  */
static void
process_decision (struct scos_decision *d, struct scos_context *scos_ctx)
{
  FILE *f = scos_ctx->file;

  fprintf (f, "C%c", d->type);
  if (d->type != 'X')
    {
      fprintf (f, " ");
      print_sloc (f, d->loc);
    }
  print_decision_subtree (d, f);
  fputc ('\n', f);
}

/* Return whether the NODE tree is a decision.  */
static bool
is_decision (tree node)
{
  int tree_code;

  /* Go deeper and deeper until the end of the nested TRUTH_NOT_EXPR or
     NOP_EXPR nodes.  */
  while (node != NULL
      && (TREE_CODE (node) == TRUTH_NOT_EXPR || TREE_CODE (node) == NOP_EXPR))
    node = TREE_OPERAND (node, 0);
  /* The given NODE is a decision if we ended up with a ANDIF or ORIF node.  */
  if (node == NULL)
    return false;
  tree_code = TREE_CODE (node);
  return tree_code == TRUTH_ANDIF_EXPR || tree_code == TRUTH_ORIF_EXPR;
}

/* Take the root tree NODE of a decision and resume the SCOs generation at the
   first sub-trees that are not part of it (ie. at the conditions of the
   decision).  */
static void
skip_decision (tree node, tree parent, struct walk_context *ctx)
{
  while (node != NULL)
    switch (TREE_CODE (node))
    {
      /* If NODE is just a TRUTH_NOT_EXPR node or a NOP_EXPR one, process
         instead its only sub-tree.  */
      case TRUTH_NOT_EXPR:
      case NOP_EXPR:
        node = TREE_OPERAND (node, 0);
        break;

      /* If it is an ANDIF/ORIF, recursively process its first operand, then
         continue with its right one.  */
      case TRUTH_ANDIF_EXPR:
      case TRUTH_ORIF_EXPR:
        skip_decision (TREE_OPERAND (node, 0), node, ctx);
        node = TREE_OPERAND (node, 1);
        break;

      /* Otherwise, resume the SCOs generation and finish this sub-tree
         walk.  */
      default:
        scos_resume_walk_tree (node, parent, ctx);
        return;
    }
}

/* Append the DOM marker (if starting a sequence) and the statement type
   (CS_CHAR) + statement location range (SR) to the output of the SCOS_CTX.  */
static void
write_unbuffered_stmt (struct scos_dom_marker *dom,
                       char cs_char, const sloc_range *sr,
                       struct scos_context *scos_ctx)
{
  FILE *f = scos_ctx->file;

  /* If no CS line is started yet, start one.  */
  if (scos_ctx->stmt_count == 0)
    {
      fprintf (f, "C%c", scos_ctx->stmt_continuation_line ? 's' : 'S');
      /* Print the dominance marker, if any, but only if we start a statement
         sequence.  */
      if (!scos_ctx->stmt_continuation_line)
        {
          if (dom->type != ' ')
            {
              fprintf (f, " >%c", dom->type);
              print_sloc (f, dom->loc);
            }
        }
    }
  print_sloc_range (cs_char, sr, f);
  ++scos_ctx->stmt_count;

  /* If we reached the maximum number of sloc_range for this line, schedule the
     start of another one.  */
  if (scos_ctx->stmt_count >= 6)
    {
      fputc ('\n', f);
      scos_ctx->stmt_count = 0;
      scos_ctx->stmt_continuation_line = true;
    }
}

static void
discard_buffered_stmt (struct scos_context *scos_ctx)
{
  struct scos_buffered_stmt *stmt = &scos_ctx->buffered_stmt;

  if (stmt->present)
    /* Restore the last-in dominance marker to point to the previous
       statement.  */
    set_dom_marker (stmt->dom_marker.type,
                    stmt->dom_marker.loc,
                    scos_ctx);
  stmt->present = false;
}

/* Flush the buffered SCO statement in SCOS_CTX, if any.  */
static void
flush_buffered_stmt (struct scos_context *scos_ctx)
{
  struct scos_buffered_stmt *stmt = &scos_ctx->buffered_stmt;
  if (scos_ctx->buffered_stmt.present)
    {
      write_unbuffered_stmt (&stmt->dom_marker, stmt->cs_char, &stmt->sr,
                             scos_ctx);
      stmt->present = false;
    }
}

/* Append a buffered SCO statement to the output of the SCOS_CTX.  */
static void
write_stmt (char cs_char, const sloc_range *sr, struct scos_context *scos_ctx)
{
  struct scos_buffered_stmt *stmt = &scos_ctx->buffered_stmt;

  /* Get the reference string for the filename so we can use pointer equality
     for comparison.  */
  const struct line_map_ordinary *map;
  linemap_resolve_location
    (line_table, sr->start, LRK_SPELLING_LOCATION, &map);
  const char *filename = src_table[SRC_INDEX_FOR (map)]->filename;

  /* There is only one room in the statement buffer: firstly, flush any
     existing buffered statement.  */
  flush_buffered_stmt (scos_ctx);

  /* If this statement is not in the same file as the previous one, invalidate
     all dominance mankers (SCOs format won't let us define cross-file ones)
     and emit a file change line.  */
  if (filename != scos_ctx->current_file)
    {
      invalidate_dom_marker (true, scos_ctx);

      /* For clarity, output a blank line before the first SCO.  */
      if (no_sco_yet)
	{
	  fprintf (scos_ctx->file, "\n");
	  no_sco_yet = false;
	}

      /* The filename comes from "line_table" ordinary line maps, thus it must
         have already been registered.  */
      fprintf (scos_ctx->file, "C %u %s\n",
               gli_filenum (filename),
               lbasename (filename));
      scos_ctx->current_file = filename;
    }

  /* Take a snapshot of all the context information that the unbuffered write
     would need to simulate that there is no buffer.  */
  stmt->present = true;
  stmt->dom_marker = *scos_ctx->dom_markers->last();
  stmt->cs_char = cs_char;
  stmt->sr = *sr;

  /* Update the last-in dominance marker to point to this statement.  */
  set_dom_marker ('S', sr->start, scos_ctx);
}

/* End the current statement line if any.  Thus, it also resets the
   continuation line state to "no continuation" and it process deferred
   decisions.  */
static void
flush_stmt_line (struct scos_context *scos_ctx)
{
  struct scos_decision *d;
  int i;

  flush_buffered_stmt (scos_ctx);

  /* There is no statement line to flush if no statement has been written.  */
  if (scos_ctx->stmt_count != 0)
    fputc ('\n', scos_ctx->file);

  scos_ctx->stmt_count = 0;
  scos_ctx->stmt_continuation_line = false;

  FOR_EACH_VEC_ELT (*scos_ctx->deferred_decisions, i, d)
    {
      process_decision (d, scos_ctx);
      free (d);
    }
  scos_ctx->deferred_decisions->truncate (0);
}

/* Helper for the tree traversal: update traversal context information when
   leaving a top-level expression.  */
static void
deferred_leave_expr (tree node ATTRIBUTE_UNUSED,
                     struct walk_context *ctx,
                     unsigned traversal_order ATTRIBUTE_UNUSED,
                     void *data ATTRIBUTE_UNUSED)
{
  struct scos_context *scos_ctx = (struct scos_context *) ctx->data;
  scos_ctx->in_expression = false;
}

/* Helper for the tree traversal: update if needed the traversal context
   information when inspecting an expression or a statement context NODE and
   add a deferred action for this node to restore it when leaving it.  Write a
   statement if we were in the context of a statement and if AUTO_STMT.  */
static void
enter_expr (tree node, struct walk_context *ctx, bool auto_stmt)
{
  struct scos_context *scos_ctx = (struct scos_context *) ctx->data;
  sloc_range sr = INITIAL_SLOC_RANGE;

  if (auto_stmt && !scos_ctx->in_expression)
    {
      add_tree_sloc_range (&sr, node);
      if (sr.start != UNKNOWN_LOCATION)
        write_stmt (' ', &sr, scos_ctx);
    }

  if (!scos_ctx->in_expression)
    {
      scos_ctx->in_expression = true;
      scos_walk_register_deferred_action (ctx,
					  POSTFIX_ORDER,
					  deferred_leave_expr,
					  NULL);
    }
}

/* Helper for generate_stmts_scos: collect information about statement
   sequences for NODE given the walking context CTX.  */
static bool
generate_stmts_scos_helper (tree node, struct walk_context *ctx)
{
  struct scos_context *scos_ctx = (struct scos_context *) ctx->data;
  sloc_range sr = INITIAL_SLOC_RANGE;
  char cs_char;

  /* Skip null nodes.  Also skip "synthetic" return statements: the C frontend
     insert them at the end of the "main" functions in ISO C99 mode; they do
     not come from sources, so we must not generate statement SCOs for
     them.  */
  if (node == NULL
      || (TREE_CODE (node) == RETURN_EXPR
	  && EXPR_LOCATION (node) == BUILTINS_LOCATION))
    return true;

  /* In most case, we do not have precise location information for tree nodes.
     Thus, almost every sloc range generated have the same start/end bounds.
     That's even worse for conditions in decisions: simple variable references
     have the location information of the variable declaration...  */

  switch (TREE_CODE (node))
  {
    case COND_EXPR:
      {
	const bool inside_expr = scos_ctx->in_expression;
	const bool returns_void = TREE_CODE (TREE_TYPE (node)) == VOID_TYPE;
	const bool is_expr = inside_expr || !returns_void;
	const tree cond = TREE_OPERAND (node, 0);
	const tree then = TREE_OPERAND (node, 1);
	const tree else_ = TREE_OPERAND (node, 2);
	sloc_range sr_then = INITIAL_SLOC_RANGE;
	sloc_range sr_else = INITIAL_SLOC_RANGE;

        if (scos_ctx->while_cond.contains (node))
          /* This COND_EXPR is considered as being part of an expanded WHILE
             statement: it has already been processed: skip it.  */
          {
            pop_dom_marker (scos_ctx);
            return true;
          }

        /* Consider the COND_EXPR node as a statement if its TREE_TYPE is void,
           otherwise consider it as an expression.  */
        if (is_expr)
          enter_expr (node, ctx, false);

	/* The sloc of COND_EXPR nodes for ternary expressions is the one of
	   the ":" token.  If we consider then/else parts of them as
	   independant statements, their sloc must not collide with the sloc of
	   this COND_EXPR node.  Note that we look also at the sloc of the else
	   part since the C frontend can switch then/else parts.  */
	add_tree_sloc_range (&sr_then, then);
	add_tree_sloc_range (&sr_else, else_);
	if (EXPR_LOCATION (node)
	    && (sr_then.end == UNKNOWN_LOCATION
		|| EXPR_LOCATION (node) <= sr_then.end)
	    && (sr_else.end == UNKNOWN_LOCATION
		|| EXPR_LOCATION (node) <= sr_else.end))
	    add_to_sloc_range (&sr, node);

        if (is_expr)
          /* Nested expressions belong to the IF statement.  */
          add_tree_sloc_range (&sr, node);
        else
          /* Nested statements do not belong to the IF statement: only add the
             controlling expression.  */
          add_tree_sloc_range (&sr, cond);

        /* This node is a statement itself if it is not inside another
           expression.  */
        if (!inside_expr)
          write_stmt ('I', &sr, scos_ctx);

        /* Defer the handling of the decision to later (not to break the
           current statement line).  */
        defer_decision ('I', sr.start, cond, scos_ctx);
        /* Continue condition expression sub-trees traversal skipping the
           current decision.  Do not forget to walk through them in an
           expression context, not to output statements for them.  */
	{
	  const bool saved_expr_state = scos_ctx->in_expression;

	  scos_ctx->in_expression = true;
	  skip_decision (cond, node, ctx);
	  scos_ctx->in_expression = saved_expr_state;
	}

        /* If this COND_EXPR is a C IF statement, we must break the statement
           lines before and after each sub-tree traversal.  */
        if (!is_expr)
          flush_stmt_line (scos_ctx);

        /* Nested statements must rely on their specific dominance marker.  */
        push_dom_marker (scos_ctx);

        /* We already processed one sub-tree (the condition expression), thus
           we have to walk the other ones (the true and the false branches)
           manually.  */
        for (int i = 1; i <= 2; ++i)
          {
            set_dom_marker ((i == 1) ? 'T' : 'F', sr.start, scos_ctx);
            scos_resume_walk_tree (TREE_OPERAND (node, i), node, ctx);
            if (!is_expr)
              flush_stmt_line (scos_ctx);
          }

        pop_dom_marker (scos_ctx);

        return true;
      }

    /* ??? How can we handle VEC_COND_EXPR nodes?  And when do they appear?  */

    case LOOP_EXPR:
      /* Every loop break statement sequences.  */
      flush_stmt_line (scos_ctx);
      break;

    case SWITCH_EXPR:
      {
	const bool in_expr = scos_ctx->in_expression;
        tree stmt_list, stmt;

        add_to_sloc_range (&sr, node);
        add_tree_sloc_range (&sr, TREE_OPERAND (node, 0));
	if (!in_expr)
	  write_stmt ('C', &sr, scos_ctx);
	flush_stmt_line (scos_ctx);

        /* The first child is the expression we switch on: browse it in an
           expression context.  */
	scos_ctx->in_expression = true;
        scos_resume_walk_tree (TREE_OPERAND (node, 0), node, ctx);
	scos_ctx->in_expression = in_expr;

        /* The second (and last) one is the statement sequence that contain
           case labels and statements for the switch block.  */
        push_dom_marker (scos_ctx);
        *(switch_dominator (scos_ctx)) = node;

        stmt_list = TREE_OPERAND (node, 1);
	if (TREE_CODE (stmt_list) == BIND_EXPR)
	  stmt_list = BIND_EXPR_BODY (stmt_list);
        if (TREE_CODE (stmt_list) != STATEMENT_LIST)
          internal_error ("Found a non-statement list under a switch"
                          " statement.");

        for (tree_stmt_iterator ts = tsi_start (stmt_list);
	     !tsi_end_p (ts);
	     tsi_next (&ts))
          {
            stmt = tsi_stmt (ts);

            if (TREE_CODE (stmt) == CASE_LABEL_EXPR
                && *(switch_dominator (scos_ctx)) != NULL)
              /* If no label invalidated dominance markers, let the statements
                 that follow CASE_LABEL_EXPR nodes be dominated by the switch
                 statement.  */
              {
                flush_stmt_line (scos_ctx);
                set_dom_marker ('S', sr.start, scos_ctx);
              }
            else
              /* Otherwise, just continue the SCOs generation.  */
              scos_resume_walk_tree (stmt, stmt_list, ctx);
          }
        pop_dom_marker (scos_ctx);
        return true;
      }

    case GOTO_EXPR:
      /* The C parser expands loops as it parses them.  WHILE loops thus end up
         here roughly as (label names are added for clarity):

         -  a GOTO_EXPR to "while_cond"
         -  a LABEL_EXPR "while_body"
         -  statements for the body of the loop
         -  a LABEL_EXPR "while_cond"
         -  a COND_EXPR that GOTO "while_body" if condition is true, or that
            GOTO "while_end" otherwise
         -  a LABEL_EXPR "while_end"

         The problem here is that the first GOTO_EXPR and the COND_EXPR nodes
         have the same source location, and outputting two SCO statements with
         the same sloc is invalid.  */
      if (TREE_CODE (scos_walk_get_parent (ctx)) == STATEMENT_LIST)
      {
        location_t while_loc = EXPR_LOCATION (node);
        tree_stmt_iterator ts;
        tree stmt_list = scos_walk_get_parent (ctx);
        tree cond_expr = NULL;
        tree cond_cond = NULL;
	const bool in_expr = scos_ctx->in_expression;

        char loop_kind = 'W';
        sloc_range init_sr = INITIAL_SLOC_RANGE;
        tree init_expr = NULL;

        /* First, look for this node in the parent statement list.  */
        for (ts = tsi_start (stmt_list);
	     !tsi_end_p (ts);
	     tsi_next (&ts))
          {
            if (tsi_stmt (ts) == node)
              break;
            init_expr = tsi_stmt (ts);
          }
        if (tsi_end_p (ts) || tsi_stmt (ts) != node)
          internal_error ("Cannot find a GOTO_EXPR node below its parent node."
                          " Is the tree traversal bugged?");

        /* The next node must be a sloc-less LABEL_EXPR.  */
        tsi_next (&ts);
        if (!tsi_end_p (ts) && TREE_CODE (tsi_stmt (ts)) == LABEL_EXPR
            && EXPR_LOCATION (tsi_stmt (ts)) == UNKNOWN_LOCATION)
        {
          /* And then look for a COND_EXPR with the same location.  */
          for (; !tsi_end_p (ts); tsi_next (&ts))
            {
	      cond_expr = tsi_stmt (ts);
              if (TREE_CODE (cond_expr) == COND_EXPR
                  && EXPR_LOCATION (cond_expr) == while_loc)
              {
                cond_cond = TREE_OPERAND (cond_expr, 0);
                break;
              }
            }

          /* We found both!  This should be an expanded WHILE statement.  */
          if (!tsi_end_p (ts))
            {
              /* Tag it accordingly so that we do not process it again
                 later.  */
              scos_ctx->while_cond.add (cond_expr);

              add_tree_sloc_range (&sr, cond_expr);
              add_tree_sloc_range (&init_sr, init_expr);
              if (sloc_in_range (init_sr.start, &sr))
                {
                  /* INIT_EXPR is an initialisation expression, thus the loop
                     is a FOR one: discard the SCO statement for INIT_EXPR.  */
                  discard_buffered_stmt (scos_ctx);
                  loop_kind = 'F';
                }

              /* Add the proper SCO statement, defer a decision for it and
                 browse the conditionnal expression for nested decisions.  We
                 have to invalidate the dominance marker for the SCO statement
                 because it may be reachable through a label inside the
                 loop.  */
              invalidate_dom_marker (false, scos_ctx);
	      if (!in_expr)
		write_stmt (loop_kind, &sr, scos_ctx);
              defer_decision ('W', sr.start, cond_cond, scos_ctx);
	      scos_ctx->in_expression = true;
              skip_decision (cond_cond, cond_expr, ctx);
	      scos_ctx->in_expression = in_expr;

              flush_stmt_line (scos_ctx);

              /* Nested statements must rely on their specific dominance
                 marker.  We'll POP it when visiting the last COND_EXPR.  */
              push_dom_marker (scos_ctx);

              /* Set a flag not to clear the dominance markers after the label
                 that comes next.  */
              scos_ctx->while_first_label_expected = true;
              set_dom_marker ('T', sr.start, scos_ctx);
              return true;
            }
        }

        /* If this is a "regular" GOTO_EXPR, process it like a RETURN_EXPR.  */
      }
      gcc_fallthrough();
    case RETURN_EXPR:
      add_to_sloc_range (&sr, node);
      if (sr.start != UNKNOWN_LOCATION)
        {
          add_tree_sloc_range (&sr, TREE_OPERAND (node, 0));
	  if (!scos_ctx->in_expression)
	    write_stmt (' ', &sr, scos_ctx);
        }
      enter_expr (node, ctx, false);
      break;

    /* Defining a (case) label break the current statement line, but the label
       itself does not produce any statement.  */
    case LABEL_EXPR:
      /* A mere label invalidates *all* stacked dominance markers, unless it is
         part of an expanded WHILE statement.  */
      if (scos_ctx->while_first_label_expected)
        {
          scos_ctx->while_first_label_expected = false;
          break;
        }
      else
        invalidate_dom_marker (true, scos_ctx);
      gcc_fallthrough();
    case CASE_LABEL_EXPR:
      /* ... but a case label does not, since arbitrary code cannot GOTO
         it.  */
      invalidate_dom_marker (false, scos_ctx);
      break;

    /* ??? Determine if EXIT_EXPR is sometimes generated by the C frontend.  */
    case EXIT_EXPR:
      /* ??? Look for a nested decision in the condition operand.  */
      if (!scos_ctx->in_expression)
	{
	  make_sloc_range (&sr, node);
	  write_stmt ('E', &sr, scos_ctx);
	}
      return true;

    /* Note: We do not consider statements embedded in expressions (this is a
       GNU extension to C).  */

    /* Consider some nodes as statements.  */
    case TYPE_DECL:
      if (!scos_ctx->in_expression)
	{
	  make_sloc_range (&sr, node);
	  if (TREE_TYPE (TREE_TYPE (node)))
	    cs_char = 's';
	  else
	    cs_char = 't';
	  write_stmt (cs_char, &sr, scos_ctx);
	}
      break;

    case DECL_EXPR:
      /* Variable declarations end up in two parts: one VAR_DECL node under a
         BIND_EXPR one, and the same node under a DECL_EXPR node.  In order to
         process VAR_DECL nodes in the same order as they appear in the source
         code, we ignore VAR_DECL that do not belong to a DECL_EXPR (this
         happens in two cases: in BIND_EXPR and when some expression uses the
         declared variable).  */

      if (scos_ctx->in_expression)
        break;

      if (!DECL_ARTIFICIAL (TREE_OPERAND (node, 0)))
        {
          tree var_decl = TREE_OPERAND (node, 0);
          tree initializer = DECL_INITIAL (var_decl);

          add_to_sloc_range (&sr, node);
          add_tree_sloc_range (&sr, initializer);
          write_stmt (' ', &sr, scos_ctx);
          enter_expr (node, ctx, false);

          /* The nested VAR_DECL node was skipped during the BIND_EXPR walk.
             The initializer it may contain must be walked when executed, so
             walk it here.  */
          scos_resume_walk_tree (initializer, var_decl, ctx);
        }
      return true;
    case VAR_DECL:
      return true;

    /* And consider some others as expressions.  Every expression can be used
       in a statement context.  In this case, a statement must be added for
       them to the current CS line.  */
    case ADDR_EXPR:
    case ARRAY_REF:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_NOT_EXPR:
    case BIT_XOR_EXPR:
    case CALL_EXPR:
    case CONVERT_EXPR:
    case COMPONENT_REF:
    case COMPOUND_LITERAL_EXPR:
    case COMPOUND_EXPR:
    case EQ_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case INDIRECT_REF:
    case LE_EXPR:
    case LSHIFT_EXPR:
    case LT_EXPR:
    case MINUS_EXPR:
    case MODIFY_EXPR:
    case MULT_EXPR:
    case NEGATE_EXPR:
    case NE_EXPR:
    case NOP_EXPR:
    case PLUS_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case RSHIFT_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      /* ??? Handle *all* expression nodes.  */
      enter_expr (node, ctx, true);
      break;

    case TRUTH_NOT_EXPR:
      enter_expr (node, ctx, true);
      /* If this node is part of a decision, defer its handling to later (not
         to break the current statement line) and continue sub-trees
         exploration below this decision.  */
      if (is_decision (TREE_OPERAND (node, 0)))
        {
          defer_decision ('X', UNKNOWN_LOCATION, node, scos_ctx);
          skip_decision (node, NULL, ctx);
          return true;
        }
      /* Otherwise, continue as usual.  */
      else
        break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      enter_expr (node, ctx, true);
      /* Defer the handling of this decision to later (not to break the current
         statement line) and continue sub-trees exploration below this
         decision.  */
      defer_decision ('X', UNKNOWN_LOCATION, node, scos_ctx);
      skip_decision (node, NULL, ctx);
      return true;

    /* Default case: do not emit any single thing.  */
    default:
      break;
  }
  /* By default, consider that sub-trees must be visited.  */
  return false;
}

/* For the map structure with index MAP_INDEX in line_table, search for a
   src_table entry already allocated for the corresponding source file name,
   create one if none is found and associate the src_table index with the
   the input MAP_INDEX.  */

static src_entry *
bind_src_entry_for (const unsigned map_index)
{
  const line_map_ordinary *map
    = LINEMAPS_ORDINARY_MAP_AT (line_table, map_index);
  const char *filename = ORDINARY_MAP_FILE_NAME (map);

  unsigned src_index = 0;
  src_entry *sentry;

  do {
    sentry = src_table[src_index];
    if ((sentry == NULL) || FILENAME_EQ (sentry->filename, filename))
      break;
    src_index ++;
  } while (sentry != NULL);

  if (sentry == NULL)
    {
      sentry = (src_entry *) xmalloc (sizeof (src_entry));

      sentry->filename = filename;

      src_table [src_index] = sentry;
    }

  linemap_to_src_index[map_index] = src_index;

  return sentry;
}

/* Initialize the sloc tracking datastructures.  */

static void
init_sloc_tracking (void)
{
  const unsigned count = LINEMAPS_USED (line_table, false);

  bitmap_obstack_initialize (NULL);

  src_table = (src_entry **) xcalloc (count, sizeof (src_entry *));
  linemap_to_src_index = (unsigned *) xmalloc (count * sizeof (unsigned));

  /* For each map index in the current line_table, find or add the
     src_table entry for the associated filename.  */

  for (unsigned map_index = 0; map_index < count; map_index ++)
    bind_src_entry_for (map_index);
}

/* Generate SCOs for BODY, the body of a function.  */

void
generate_scos (tree body)
{
  struct scos_context context;

  context.file = gli_open (main_input_filename);
  if (!context.file)
    return;

  context.current_file = NULL;

  /* Initially, the "current" CS line has no sloc_range (ie. we are'n in a CS
     line) and is not a continuation of a previous CS line.  Moreover, there is
     initially no buffered statement.  */
  context.stmt_count = 0;
  context.stmt_continuation_line = false;
  context.buffered_stmt.present = false;

  context.deferred_decisions = new vec<scos_decision_p>();

  /* The first statement of a function is not dominated by any other
     statement.  */
  context.dom_markers = new vec<scos_dom_marker_p>();
  push_dom_marker (&context);

  context.while_first_label_expected = false;

  context.in_expression = false;

  init_sloc_tracking ();

  scos_walk_tree (body, generate_stmts_scos_helper, &context);

  flush_stmt_line (&context);
  vec_free (context.deferred_decisions);
  vec_free (context.dom_markers);
  gli_close (context.file);

  if (context.in_expression)
    internal_error
      ("still in expression context at the end of the SCOs generation pass.");
}
