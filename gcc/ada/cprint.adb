------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C P R I N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;          use Atree;
with Checks;         use Checks;
with Csets;          use Csets;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Tss;        use Exp_Tss;
with Exp_Unst;       use Exp_Unst;
with Exp_Util;       use Exp_Util;
with Lib;            use Lib;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Osint;          use Osint;
with Osint.C;        use Osint.C;
with Output;         use Output;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Eval;       use Sem_Eval;
with Sem_Mech;       use Sem_Mech;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Table;
with Ttypes;         use Ttypes;
with Types;          use Types;
with Uintp;          use Uintp;
with Urealp;         use Urealp;
with System.HTable;  use System.HTable;
with System.OS_Lib;  use System.OS_Lib;

package body Cprint is

   type Stage_Value is (Searching_Decls, Removing_Decls, Generating_Output);
   Back_End_Stage : Stage_Value;
   --  By default this backend performs several tree traversals associated with
   --  three stages: in the first one the backend searches for decls referenced
   --  by the generated C code; in the second one it traverses the tree marking
   --  declarations of unreferenced types; in the third stage, it traverses the
   --  tree generating the C output. This variable indicates the current stage
   --  (to disable output and tree modifications in the tree traversals of the
   --  first two stages). The switch -gnatd.6 disables this functionality and
   --  generates the full C output in a single tree traversal.

   Anon_Struct_Prefix  : constant String := "_s";
   Anon_Union_Prefix   : constant String := "_u";
   --  Prefix of the internal name given to anonymous unions and structs (since
   --  anonymous unions and structs are not supported by C90).

   Current_Source_File : Source_File_Index;
   --  Index of source file whose generated code is being dumped

   Full_Code_Generation : Boolean := False;
   --  True if we should generate C code for all constructs. If False, only
   --  generate a C header for Ada specs.

   Dump_Node : Node_Id := Empty;
   --  This is set to the current node, used for printing line numbers

   FLCache_N  : Node_Id := Empty;
   FLCache_FL : Physical_Line_Number;
   FLCache_LL : Physical_Line_Number;
   --  Cache for First_Line and Last_Line (N records last node for which any
   --  of these subprograms were called, FL and LL record the corresponding
   --  First and Last physical line numbers for this node).

   Freeze_Level : Int := 0;
   --  Keep track of freeze level (incremented on entry to freeze actions and
   --  decremented on exit). Used to know if we are within freeze actions.

   Indent : Nat := 0;
   --  Number of columns for current line output indentation

   Next_Line_To_Print : Physical_Line_Number := 1;
   --  This keeps track of the next physical line number to process in
   --  Write_Source_Lines.

   No_Physical_Line_Number : constant Physical_Line_Number :=
                               Physical_Line_Number'Last;
   --  Used internally to indicate no line number available

   Tree_Traversals_Counter : Nat := 0;
   --  Number of tree traversals performed by the backend

   In_Comment : Boolean := False;
   --  Indicates whether the current output is a comment added to the output.
   --  Used to disable registering references to entities when Cprint_Node()
   --  is called to output an entity reference in a C comment since otherwise
   --  such references would be erroneously registered in the Types Table.

   In_Header_File : Boolean := False;
   --  Indicates whether the output of the current node is generated for a
   --  C header file.

   In_Library_Unit_Decl : Boolean := False;
   --  Indicates whether the current node is part of a library unit package
   --  or subprogram declaration.

   In_Library_Unit_Pkg_Decl : Boolean := False;
   --  Indicates whether the current node is part of a library unit package
   --  declaration

   In_Main_Unit : Boolean := False;
   --  Indicates whether the current unit being processed is part of the
   --  main unit. If this is the case, output all code; otherwise, output
   --  only external declarations and types.

   In_Object_Decl_Init_Expr : Boolean := False;
   --  Indicates whether the current output corresponds with the initialization
   --  expression of an object declaration.

   Library_Level : Boolean := True;
   --  Indicates whether the current node is at library level

   In_Package_Body_Init : Boolean := False;
   --  Indicates whether the current node is located in the initialization of a
   --  package body.

   In_Search_Type_Ref : Boolean := False;
   --  Indicates whether we are unnesting types of nested subprograms

   Special_Elaboration_Code : Boolean := False;
   --  Indicates whether we are generating code for statements part of the
   --  elaboration code (outside an explicit 'begin ... end').

   Current_Elab_Entity : Node_Id := Empty;
   --  Current entity which needs to be elaborated. Only set when
   --  Special_Elaboration_Code is True.

   Current_Subp_Entity : Entity_Id := Empty;
   --  Current subprogram for which Output_One_Body is generating code

   In_Compound_Statement : Boolean := False;
   --  Indicates whether we are processing a compound statement and, if so,
   --  will generate different code if needed. This is used in particular to
   --  emit an if-statement as an if-expression.

   Unrolling_Full_Type_Decl : Boolean := False;
   --  True if the backend is unrolling a full type declaration.

   Unrolling_Instance_Subp_Id : Entity_Id := Empty;
   --  Set to the unique identifier of a subprogram instantiation when the
   --  backend is unrolling declarations of its wrapper package.

   --  The following constants are used by Write_Uint. They are initialized as
   --  shown when Source_Dump is called:

   ints  : Nat renames Standard_Integer_Size;
   longs : Nat renames Standard_Long_Integer_Size;
   lls   : Nat renames Standard_Long_Long_Integer_Size;
   --  Length in bits of int, long, long long

   LNegInt  : Uint; --  -(Uint_2 ** (ints - 1));
   LPosInt  : Uint; --  abs (LNegInt + 1);
   LNegLong : Uint; --  -(Uint_2 ** (longs - 1));
   LPosLong : Uint; --  abs (LNegLong + 1);
   LNegLL   : Uint; --  -(Uint_2 ** (lls - 1));
   LPosLL   : Uint; --  abs (LNegLL + 1);
   --  Bounds of int, long, long long

   LPosU    : Uint; --  (Uint_2 ** ints) - 1;
   LNegU    : Uint; --  -LPosU;
   LPosUL   : Uint; --  (Uint_2 ** longs) - 1;
   LNegUL   : Uint; --  -LPosUL;
   LPosULL  : Uint; --  (Uint_2 ** lls) - 1;
   LNegULL  : Uint; --  -LPosULL;
   --  Bounds of unsigned, long unsigned, long long unsigned

   ---------------------------------------
   -- Back_End Stage Dependant Services --
   ---------------------------------------

   --  These subprograms hide from direct visibility routines defined in other
   --  packages disabling their functionality while traversing the tree looking
   --  for types.

   --    Warning: The first tree traversal performed by this backend must
   --             not modify tree contents; otherwise the C code generated
   --             in the second tree traversal may be wrong or incomplete.
   --             So, if other routines that modify tree contents are needed
   --             in the future, they must be defined here disabling their
   --             functionality during the first tree traversal.

   procedure Error_Msg_N (Msg : String; N : Node_Or_Entity_Id);
   --  In Generating_Output stage output a message at the Sloc of the given
   --  node; otherwise perform no action.

   procedure Remove (Node : Node_Or_Entity_Id);
   --  In Generating_Output stage remove Node from its list; otherwise perform
   --  no action.

   procedure Set_Error_Posted (N : Node_Id; Value : Boolean := True);
   --  In Generating_Output stage set the Error_Posted flag on N; otherwise do
   --  nothing.

   procedure Set_Has_No_Elaboration_Code
     (N     : Node_Id;
      Value : Boolean := True);
   --  In Generating_Output stage set the elaboration code value on N; perform
   --  no action in other stages.

   procedure Set_Has_Non_Standard_Rep
     (E     : Entity_Id;
      Value : Boolean := True);
   --  In Generating_Output stage set the Has_Non_Standard_Rep value on E; no
   --  action performed in other stages.

   ------------------------------------------
   -- Procedures for printing C constructs --
   ------------------------------------------

   procedure Cprint_Bar_List (List : List_Id);
   --  Print the given list with items separated by vertical bars

   procedure Cprint_Call (Node : Node_Id);
   --  Outputs a function or procedure call, with its parameters, dealing
   --  with the case of passing addresses for OUT or IN OUT parameters

   function Cprint_Comma_List (List : List_Id) return Integer;
   procedure Cprint_Comma_List (List : List_Id);
   --  Prints the nodes in a list, with separating commas. If the list is empty
   --  then no output is generated.
   --  The function version returns the number of nodes printed.

   procedure Cprint_Copy
     (Target     : Node_Id;
      Source     : Node_Id;
      Use_Memcpy : Boolean);
   --  Print code which copies the contents of Source into Target. If
   --  Use_Memcpy is True, the use of memcpy() is safe. Otherwise
   --  memmove() will be used.

   procedure Cprint_Declare
     (Ent        : Entity_Id;
      Add_Access : Boolean := False;
      Virtual_OK : Boolean := False;
      Semicolon  : Boolean := True);
   --  Wrapper of Cprint_Type_Reference and Cprint_Object_Reference which
   --  provides the following extra functionality:
   --    * Declare each entity just once
   --    * If Semicolon is True then emit the closing semicolon and prepend
   --      indentation if needed.

   function Cprint_Object_Reference
     (Ent        : Entity_Id;
      Add_Access : Boolean := False) return Boolean;
   --  Print a normal C declaration for an object. The output does not include
   --  the terminating semicolon. If Add_Access is set to true, then the object
   --  has an extra access, i.e. if we have A of type B then a declaration for
   --  A of type *B is output. Note that there is no indent call, the caller
   --  should call Indent if a new line is needed.

   function Cprint_Type_Reference
     (Ent        : Entity_Id;
      Add_Access : Boolean := False;
      Virtual_OK : Boolean := False) return Boolean;
   --  Print a typedef declaration for a type. The output does not include the
   --  terminating semicolon. If Add_Access is set to true, then the type has
   --  an extra access, i.e. if we have A of type B then a declaration for A of
   --  type *B is output. Note that there is no indent call, the caller should
   --  call Indent if a new line is needed. Virtual_OK deals with the case of
   --  unconstrained array types. When a normal variable of such a type is
   --  declared, the bounds are present in the type, and are the bounds to be
   --  output (case of Virtual_OK = False). But in e.g. the formal of a call,
   --  the bounds come from the caller, and if the type is unconstrained are to
   --  be output simply as []. In this case Virtual_OK is set True. Bounds are
   --  also output as [] if the array is variable length and Add_Access is True

   procedure Cprint_Difference (Val1 : Node_Id; Val2 : Uint; B : Boolean);
   --  Outputs the value of Val1 - Val2, using a single integer value if the
   --  value is known at compile time and otherwise prints val1 - val2. B
   --  is True if parens should be used in the compound case, false otherwise.
   --  Overflows are ignored.

   procedure Cprint_Difference
     (Val1          : Node_Id;
      Val2          : Node_Id;
      Minus_One_Min : Boolean);
   --  Same as above.
   --  In addition, if Minus_One_Min is True, generate Max (Val2 - Val1, -1)
   --  to ensure that we never generate a value below -1. In other words,
   --  assume that this procedure is called to generate array bounds which
   --  should never be negative (case of 'Last < 'First).
   --  Overflows are ignored.

   procedure Cprint_Indented_List (List : List_Id);
   --  Like Cprint_Line_List, except that the indentation level is increased
   --  before outputting the list of items, and then decremented (back to its
   --  original level) before returning to the caller.

   procedure Cprint_Left_Opnd (N : Node_Id);
   --  Print left operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.

   procedure Cprint_Node (Node : Node_Id; Declaration : Boolean := False);
   --  Prints a single node. No new lines are output, except as required for
   --  splitting lines that are too long to fit on a single physical line.
   --  No output is generated at all if Node is Empty. No trailing or leading
   --  blank characters are generated.
   --  If Declaration is True then use the symbolic name associated with Node,
   --  otherwise this subprogram is allowed to replace Node by its value in
   --  case of e.g. a constant.

   procedure Cprint_Node_List (List : List_Id; New_Lines : Boolean := False);
   --  Prints the nodes in a list with no separating characters. This is used
   --  in the case of lists of items which are printed on separate lines using
   --  the current indentation amount. New_Lines controls the generation of
   --  New_Line calls. If False, no New_Line calls are generated. If True,
   --  then New_Line calls are generated as needed to ensure that each list
   --  item starts at the beginning of a line.

   procedure Cprint_Node_Paren (N : Node_Id);
   --  Prints node, adding parentheses if N is an operator, or short circuit
   --  operation or other subexpression which needs parenthesizing as an
   --  operand (we always fully parenthesize expression trees in the C output).

   procedure Cprint_Opt_Node_List (List : List_Id);
   --  Like Cprint_Node_List, but prints nothing if List = No_List

   procedure Cprint_Right_Opnd (N : Node_Id);
   --  Print right operand of operator, parenthesizing if necessary. Note that
   --  we fully parenthesize operator trees in the C output.

   procedure Cprint_Subprogram_Body (N : Node_Id);
   --  Output subprogram body, including dealing with unnesting any subprograms
   --  nested within this body for an outer level subprogram.

   procedure Cprint_Sum (Val1 : Node_Id; Val2 : Uint; B : Boolean);
   --  Outputs the value of Val1 + Val2, using a single integer value if the
   --  value is known at compile time and otherwise prints (val1 + val2). B
   --  is True if parens should be used in the compound case, false otherwise

   procedure Cprint_Type_Name (Typ : Entity_Id; Use_Typedef : Boolean := True);
   --  Output C representation of Ada type Typ.
   --  If Use_Typedef is True, the Typ name is just printed since it is assumed
   --  to be a typedef name. You can set it to False to avoid this behavior.
   --  This is used when Cprint_Type_Name is called from typedef circuitry, to
   --  avoid a typedef pointing to itself!

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Append_Subprogram_Prefix (Spec : Node_Id);
   --  Append "_ada_" to the name if this is a library-level subprogram,
   --  so it can be invoked as a main subprogram from the bind module.

   procedure Check_Attributes (E : Entity_Id);
   --  Check whether we must emit __attribute__ clauses, and emit them if so.

   procedure Check_Definition (N : Node_Id; Error_Node : Node_Id := Empty);
   --  Verify that N is previously defined and report an error on Error_Node
   --  otherwise. If Error_Node is Empty the error is reported on N.

   function Check_Sloc (S : Source_Ptr) return Boolean;
   --  Return False if we are not in the current source file (e.g.
   --  instantiation, inlining).

   procedure Check_Volatile_Atomic (N : Node_Id);
   --  Check if the C volatile specifier is needed to declare N and add it if
   --  so. Also generate a warning when Volatile_Full_Access or Atomic is set.

   procedure Col_Check (N : Nat);
   --  Check that at least N characters remain on current line, and if not,
   --  then start an extra line with two characters extra indentation for
   --  continuing text on the next line.

   function Compound_Statement_Compatible (L : List_Id) return Boolean;
   --  Return True if L contains only expressions or statements compatible
   --  with compound statements.

   procedure Debug_Write_Chars (N : Node_Id);
   --  Output the characters associated with the given node, its internal id,
   --  and its location.

   procedure Debug_Write_Cprint_Declare_Stack;
   pragma Export (Ada, Debug_Write_Cprint_Declare_Stack);
   --  Output the contents of the Cprint_Declare_Stack table

   procedure Debug_Write_Cprint_Node_Stack;
   pragma Export (Ada, Debug_Write_Cprint_Node_Stack);
   --  Output the contents of the Cprint_Node_Stack table

   procedure Debug_Write_Type_References;
   --  Output the contents of Types_Table

   procedure Declare_Enumeration_Type (Typ : Entity_Id);
   --  Output the declaration of the enumeration Typ

   procedure Declare_Subprogram_Types (N : Node_Id);
   --  Force the declaration of the types of the subprogram formals
   --  (including the return type of functions).

   procedure Dump_Type (Typ : Entity_Id);
   --  Dump type and indentation if Typ has not been dumped yet and it is
   --  not defined in the Standard package. For private types dump their
   --  full view and only when the names of the full_view and the partial
   --  view differ dump also the partial view.

   procedure Ensure_New_Line;
   --  Ensure that we are the start of a newline with current indentation

   function Exceeds_Maximum_Array_Length (Length : Uint) return Boolean;
   --  Return True if Length exceeds the maximum C array length

   function First_Line (N : Node_Id) return Physical_Line_Number;
   --  Given a subtree, determines the first physical line number for any node
   --  in the subtree. Returns No_Physical_Line_Number if no value found.

   function Get_Full_View (Id : Entity_Id) return Entity_Id;
   --  Return the full view of Id, or Id itself

   function Last_Line (N : Node_Id) return Physical_Line_Number;
   --  Given a subtree, determines the last physical line number for any node
   --  in the subtree. Returns No_Physical_Line_Number if no value found.

   procedure Get_First_Last_Line (N : Node_Id);
   --  Determines first and last physical line number for subtree N, placing
   --  the result in FLCache. Result is No_Physical_Line_Number if node N does
   --  not come from current source file.

   function Has_AREC_Itype (E : Entity_Id) return Boolean;
   --  Return True if E has been registered in the Types_Table and it is know
   --  to have an AREC itype.

   function Has_Backend_Itype_Entity (E : Entity_Id) return Boolean;
   --  Return True if E has been registered in the Types_Table and it is know
   --  to have a backend itype entity.

   function Has_Object_References (E : Entity_Id) return Boolean;
   --  Return True if the tree traversal collecting types and their references
   --  found some reference to constant or variable object E.

   function Has_Type_References (E : Entity_Id) return Boolean;
   --  Return True if the tree traversal collecting types and their references
   --  found some reference to type E.

   function Has_Or_Inherits_Enum_Rep_Clause (E : Entity_Id) return Boolean;
   --  Return True if the enumeration type E or some of its parents has an
   --  enumeration representation clause.

   function Has_Same_Int_Value (Val1 : Node_Id; Val2 : Node_Id) return Boolean;
   --  Return True if Val1 and Val2 represent the same integer value

   procedure Handle_Attribute (N : Node_Id);
   --  Handle C generation of an attribute reference

   procedure Handle_Raise (N : Node_Id);
   --  Handle the C generation of N_Raise_Statement, N_Raise_Expression
   --  and N_Raise_xxx_Error nodes.

   function In_Instantiation (S : Source_Ptr) return Boolean;
   --  Returns True if the source location corresponds with an instantiation

   function Is_Access_Attribute_Reference (N : Node_Id) return Boolean;
   --  Return True if the attribute reference N corresponds with an
   --  access or address attribute.

   function Is_Constant_Folded
     (E       : Entity_Id;
      In_Decl : Boolean := False) return Boolean;
   --  Return True if E is a constant that is folded by the C backend. In_Decl
   --  indicates that the context of the call to this subprogram is precisely
   --  the object declaration of E.

   function Is_Enum_Literal_Of_Enclosing_Subprogram
     (E : Entity_Id) return Boolean;
   --  Returns True if E is an enumeration literal whose enumeration type is
   --  defined in an enclosing subprogram.

   function Is_Fully_Declared (E : Entity_Id) return Boolean;
   --  Return True if E is fully declared (ie. Cprint_Declare has completed its
   --  declaration).

   function Is_Out_Mode_Access_Formal (E : Node_Id) return Boolean;
   --  Returns True if E is an OUT or IN-OUT access formal

   function Is_Packed_Array (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ is a packed array

   function Is_Raise_Statement (N : Node_Id) return Boolean;
   --  Returns True if N is a raise statement or a raise expression

   function Is_Simple_Unchecked_Union (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ is an Unchecked_Union record type declaration with
   --  all its components declared in its variant parts.

   function Is_Supported_Variable_Size_Record (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ is a record with discriminants whose last field is
   --  an array which depends on its discriminants.

   function Is_Unsigned_Or_Modular_Type (Typ : Entity_Id) return Boolean;
   pragma Inline (Is_Unsigned_Or_Modular_Type);
   --  Return True if Typ is an unsigned or a modular type

   procedure Indent_Begin;
   --  Increase indentation level

   procedure Indent_End;
   --  Decrease indentation level

   function Last_Chance return String;
   --  Return call to last chance handler

   function Last_Field (Typ : Node_Id) return Node_Id;
   --  Return the last field of a given record type

   function Non_Standard_Modular_Type (Typ : Node_Id) return Boolean;
   --  Return whether Typ represents a nonstandard (either Non_Binary_Modulus
   --  or a modulus not in 2**8/16/32/64) modular type.

   procedure Output_Anon_Struct_Name (N : Node_Id);
   --  Output the internal name given to the anonymous struct generated for
   --  a variant part of a record type that has several components.

   procedure Output_Packed_Array_Type (Typ : Node_Id);
   --  Output corresponding packed array type if Typ is a packed array

   procedure Output_Sizeof (Target : Node_Id; Source : Node_Id := Empty);
   --  Output call to sizeof() taking the size of Target or Source, whichever
   --  can be computed.

   function Parens_Needed (N : Node_Id) return Boolean;
   --  Returns True if N is in a context where it is not known to be safe to
   --  leave an expression unparenthesized. This is conservative. False means
   --  is definitely safe to leave out parens, True means that parens may be
   --  needed so they will be put in. Right now, the test is limited to being
   --  the right side of an assignment.

   function Pass_Pointer (Ent : Entity_Id) return Boolean;
   --  Ent is the entity for a formal parameter. This function returns True if
   --  the corresponding object must be passed by using a pointer in C (i.e. by
   --  adding * in the definition of the formal, and & for calls). This is True
   --  for OUT and IN OUT parameters and for by-ref types.
   --  Note that it is never True for arrays, since in C, arrays are always
   --  passed in pointer form in any case.

   function Requires_Address (Typ : Node_Id) return Boolean;
   --  Return True if an object of type Typ should have its address taken when
   --  referencing it (to e.g. call memcmp() or memcmp()).

   procedure Unimplemented_Attribute
     (N       : Node_Id;
      Attr    : Name_Id;
      Context : String := "");
   --  Called to output error string for given unimplemented attribute Attr,
   --  and post error message on node N. Append Context to the error message.

   type Bound_Kind is (Low, High);
   --  Used to specify the bound value writen by Write_Array_Bound

   procedure Write_Array_Bound
     (Expr      : Node_Id;
      Bound     : Bound_Kind;
      Dimension : Pos);
   --  Output the low bound or high bound of the given dimension of the fat
   --  pointer or array available through Expr.

   procedure Write_C_Char_Code (CC : Char_Code);
   --  Write a given character in a suitable form for the C language.

   procedure Write_Id (N : Node_Id);
   --  N is a node with a Chars field. This procedure writes the name that
   --  will be used in the generated code associated with the name. For a
   --  node with no associated entity, this is simply the Chars field. For
   --  the case where there is an entity associated with the node, we print
   --  the name associated with the entity (since it may have been encoded).
   --  One other special case is that an entity has an active external name
   --  (i.e. an external name present with no address clause), then this
   --  external name is output. This procedure also deals with outputting
   --  declarations of referenced types, if not output earlier.

   procedure Write_Integer_Type
     (Siz : Int; Signed : Boolean; Typ : Entity_Id := Empty);
   --  Output an integer type given the size Siz in bits, rounded to the next
   --  power of two (8, 16, 32, 64). If Signed, reference integer_xx, otherwise
   --  reference unsigned_xx.

   procedure Write_Indent;
   --  Start a new line and write indentation spacing

   procedure Write_Indent_Str (S : String);
   --  Start a new line and write indent spacing followed by given string

   procedure Write_Name_Col_Check (N : Name_Id);
   --  Write name (using Write_Name) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Binary_Op (Node : Node_Id; Op : String);
   --  Write binary operation Op on a given Node, in particular taking into
   --  account nonbinary modular operations.

   procedure Write_Param_Specs (N : Node_Id);
   --  Output parameter specifications for node (which is either a function or
   --  procedure specification with a Parameter_Specifications field)

   procedure Write_Source_Lines
     (From : Physical_Line_Number;
      To   : Physical_Line_Number);
   --  From, To are the start/end physical line numbers for the construct
   --  whose C translation is about to be printed. This routine takes care of
   --  generating required #line directives, and also in Dump_Source_Text mode,
   --  prints non-comment source Ada lines as C comments.

   procedure Write_Source_Lines (N : Node_Id);
   --  Same, but From, To are First_Line, Last_Line of node N

   procedure Write_Source_Lines (S : Source_Ptr);
   --  Same, but From and To both correspond to the given Source_Ptr value

   procedure Write_Source_Lines (From : Source_Ptr; To : Physical_Line_Number);
   --  Same, but From is line corresponding to given source_Ptr value.

   procedure Write_Str_Col_Check (S : String);
   --  Write string (using Write_Str) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Uint
     (U            : Uint;
      Column_Check : Boolean := True;
      Modular      : Boolean := False);
   --  Write Uint.
   --  If Column_Check is True, perform initial column check and possible
   --  initial Write_Indent (to get new line) if current line is too full.
   --  The output is always in decimal. Takes care of special cases of the
   --  largest negative number, and possible long integer output.
   --  If Modular is True, output Uint as an unsigned C integer.

   procedure Write_Unconstrained_Array_Prefix (N : Node_Id);
   --  Given an unconstrained array expression N, write a reference to this
   --  array, ready to be used as part of indexing or slicing this array.

   procedure Write_Ureal_Col_Check (Node : Node_Id);
   --  Write Ureal with column checks and a possible initial Write_Indent (to
   --  get new line) if current line is too full. If the number is too big to
   --  be generated then report an error.

   procedure db (S : String; N : Int);
   pragma Warnings (Off, db);
   --  Debugging output, given string and integer value

   type Header_Num is range 1 .. 4096;

   function Hash (N : Node_Id) return Header_Num;
   --  Simple Hash function for Node_Ids

   package Cprint_Declare_Stack is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 32,
     Table_Increment      => 100,
     Table_Name           => "Cprint_Declare_Stack");
   --  Table of entities under declaration (ie. entities which Cprint_Declare
   --  is processing but are not yet fully declared). Used to ensure that we
   --  only register in the Types_Table references to types that are fully
   --  declared.

   package Cprint_Node_Stack is new Table.Table (
     Table_Component_Type => Node_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 32,
     Table_Increment      => 100,
     Table_Name           => "Cprint_Node_Stack");
   --  Table of enclosing nodes of the Cprint_Node tree traversal. Used to
   --  identify references to types.

   package Enclosing_Subp_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Entity_Id,
      No_Element => Empty,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table of entities, to record the enclosing function on which the
   --  backend declares each entity.

   package Entity_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table of entities, to record which entity has been dumped already

   package Elaboration_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Elaboration_Table");
   --  Table of statements part of the current elaboration procedure

   type Objects_Table_Element is record
      References : Elist_Id := No_Elist;
      --  Field shared by all the tree traversals
   end record;

   type Objects_Table_Element_Access is access Objects_Table_Element;

   package Objects_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Objects_Table_Element_Access,
      No_Element => null,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table that records variables, constants and their references

   type Types_Table_Element is record

      --  Fields shared by all the tree traversals

      Has_AREC_Itype              : Boolean  := False;
      Has_Backend_Itype_Entity    : Boolean  := False;
      Has_Backend_Itype_Reference : Boolean  := False;
      References                  : Elist_Id := No_Elist;

      --  Fields associated with stage Removing_Decls

      Declaration_Skipped         : Boolean  := False;
      Enum_Literals_Declared      : Boolean  := False;
      References_Count            : Natural  := 0;
   end record;

   type Types_Table_Element_Access is access Types_Table_Element;

   package Types_Table is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Types_Table_Element_Access,
      No_Element => null,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table that records types and their references

   package Macro_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 512,
      Table_Increment      => 100,
      Table_Name           => "Macro_Table");
   --  Table of macros part of the current scope

   procedure Register_Entity (E : Entity_Id);
   --  Register E in Enclosing_Subp_Table and Entity_Table

   procedure Register_Object_Reference (E : Entity_Id);
   --  Register the reference to the variable or constant object E

   procedure Register_Objects_Table_Entity (E : Entity_Id);
   --  Register E in Objects_Table

   procedure Register_Type_Reference (E : Entity_Id);
   --  Register the type reference to E from the given node

   procedure Register_Types_Table_Entity (E : Entity_Id);
   --  Register E in Types_Table

   -------------------------------
   -- Activation Record Support --
   -------------------------------

   --  Routines which facilitate handling the activation record of unnested
   --  subprograms.

   package AREC_Support is
      function ARECnU (Subp_Id : Entity_Id) return Node_Id;
      --  Return the uplink component of the given subprogram

      function ARECnF (Subp_Id : Entity_Id) return Node_Id;
      --  Return the extra formal that contains the pointer to the activation
      --  record for uplevel references of the given subprogram.

      function AREC_Entity (N : Node_Id) return Entity_Id;
      --  Given an N_Identifier node N which references a field of an
      --  activation record, return the entity of the corresponding formal.

      function AREC_Subprogram (Formal : Entity_Id) return Entity_Id;
      --  Return the subprogram that has a field in its activation record to
      --  pass Formal to its nested subprograms.

      function Get_AREC_Field (N : Node_Id) return Node_Id;
      --  Given the AREC reference N return the AREC field

      function Is_AREC_Reference (N : Node_Id) return Boolean;
      --  Return True if N is a reference to an AREC field

      procedure Write_Up_Level_Formal_Reference
        (Subp   : Entity_Id;
         Formal : Entity_Id);
      --  Write code that climbs through the activation record of the enclosing
      --  subprograms and references the pointer to the fat pointer Formal
      --  parameter of Subp.
   end AREC_Support;
   use AREC_Support;

   ---------------------------------------
   -- Back_End Stage Dependant Services --
   ---------------------------------------

   --  These routines are logically associated because they provide similar
   --  functionality (ie. disabling services). They cannot be defined in a
   --  local package to ensure that they hide from direct visibility their
   --  counterpart routine (defined in other package).

   -----------------
   -- Error_Msg_N --
   -----------------

   procedure Error_Msg_N (Msg : String; N : Node_Or_Entity_Id) is
   begin
      if Back_End_Stage = Generating_Output then
         Errout.Error_Msg_N (Msg, N);
      end if;
   end Error_Msg_N;

   ------------
   -- Remove --
   ------------

   procedure Remove (Node : Node_Or_Entity_Id) is
   begin
      if Back_End_Stage = Generating_Output then
         Nlists.Remove (Node);
      end if;
   end Remove;

   ----------------------
   -- Set_Error_Posted --
   ----------------------

   procedure Set_Error_Posted
     (N     : Node_Id;
      Value : Boolean := True) is
   begin
      if Back_End_Stage = Generating_Output then
         Sinfo.Nodes.Set_Error_Posted (N, Value);
      end if;
   end Set_Error_Posted;

   ---------------------------------
   -- Set_Has_No_Elaboration_Code --
   ---------------------------------

   procedure Set_Has_No_Elaboration_Code
     (N     : Node_Id;
      Value : Boolean := True) is
   begin
      if Back_End_Stage = Generating_Output then
         Sinfo.Nodes.Set_Has_No_Elaboration_Code (N, Value);
      end if;
   end Set_Has_No_Elaboration_Code;

   ------------------------------
   -- Set_Has_Non_Standard_Rep --
   ------------------------------

   procedure Set_Has_Non_Standard_Rep
     (E     : Entity_Id;
      Value : Boolean := True) is
   begin
      if Back_End_Stage = Generating_Output then
         Einfo.Entities.Set_Has_Non_Standard_Rep (E, Value);
      end if;
   end Set_Has_Non_Standard_Rep;

   ---------------------------
   -- Back_End_Scopes_Stack --
   ---------------------------

   --  Stack associated with the generated code. Used to identify declarations
   --  that requires the generation of extra scopes in order to generate C90
   --  compliant code, since the front-end routine Insert_Actions may insert
   --  temporaries in statement lists and C90 does not accept mixing
   --  declarations and statements.

   package Back_End_Scopes_Stack is
      Extra_Scopes_Allowed : Boolean := True;
      --  Enable/disable the ability to create extra scopes

      procedure Open_Scope (With_Block : Boolean := True);
      --  Make new scope stack entry in the top of the scopes stack and output
      --  character '{' if With_Block is True. The new scope is enabled to
      --  start processing declarations; it must be disabled by the caller
      --  invoking the routine Set_In_Statements when it starts generating
      --  code for the statements of this scope.

      procedure Open_Extra_Scope;
      --  Check if an extra scope is needed, and if true then output '{',
      --  push a new scope stack entry, and mark it as extra scope.

      procedure Close_Scope;
      --  Remove from the top of the stack all the entries of inner extra
      --  scopes (if any) and the first non-extra scope. Output '}' for
      --  each closed scope that was opened with With_Block set to True.

      procedure Close_Scope (Scop_Id : Nat);
      --  Remove from the top of the stack all the entries of inner extra
      --  scopes (if any) until the scope Scop_Id is removed from the stack.
      --  Output '}' for each closed scope that was opened with With_Blocks
      --  set to True.

      function Current_Scope_Id return Nat;
      --  Return the id of the current scope

      function In_Declarations return Boolean;
      --  Return True if we are processing the declarations of the scope in
      --  the top of the stack.

      procedure Set_In_Statements;
      --  Remember in the top of the stack entry that we are processing its
      --  declarations.

      procedure Reset;
      --  Clear the contents of the stack

   private
      procedure Write_Scope_Stack;
      --  For debugging purposes

      procedure wss renames Write_Scope_Stack;
      pragma Export (Ada, wss);
   end Back_End_Scopes_Stack;
   use Back_End_Scopes_Stack;

   -----------------------------
   -- Back_End_Itypes_Support --
   -----------------------------

   --  This package provides support to the back end to define extra itypes
   --  not available in the tree. Currently it is used to generate an extra
   --  itype associated with subprogram formals whose type is an access to
   --  an unconstrained multidimensional array type (for unidimensional array
   --  types this extra itype is not needed because the formal is defined as
   --  a pointer to the component type).

   package Back_End_Itypes_Support is
      procedure Declare_Back_End_Itypes (Subp_Id : Entity_Id);
      --  Declare back-end itypes associated with the formals of a subprogram
      --  whose type is an access to a multidimensional unconstrained array

      function Has_Back_End_AREC_Itype (E : Entity_Id) return Boolean;
      --  Return True if E has an extra back-end AREC itype

      function Has_Back_End_Itype (E : Entity_Id) return Boolean;
      --  Return True if E has an extra back-end itype

      function Has_Local_References
        (Subp : Entity_Id;
         Typ  : Entity_Id) return Boolean;
      --  Return True if the subprogram Subp has some reference to Typ. Typ is
      --  a subtype of a multidimensional unconstrained array formal for which
      --  the backend has declared an AREC itype.

      procedure Write_Back_End_Itype_Id (E : Entity_Id);
      --  Output the identifier of the back-end itype of E
   end Back_End_Itypes_Support;
   use Back_End_Itypes_Support;

   -----------------------------
   -- Back_End_Labels_Support --
   -----------------------------

   --  This package provides support to the back end to register loop labels
   --  referenced by the C code. It is currently used to generate the code of
   --  the N_Exit_Statement when it is located inside a case statement (since
   --  in such case we cannot generate a 'break' statement to leave the inner
   --  loop; we generate an explicit 'goto' statement).

   package Back_End_Labels_Support is
      procedure Register_Back_End_Label (Label_Id : Node_Id);
      --  Register the label identifier as a label referenced by the C code

      function Is_Back_End_Label (Label_Id : Node_Id) return Boolean;
      --  Return True if Label_Id is a label referenced by the C code
   end Back_End_Labels_Support;
   use Back_End_Labels_Support;

   --------------------------
   -- Fat_Pointers_Support --
   --------------------------

   package Fat_Pointers_Support is
      In_Fatptr_Constructor_Call : Boolean := False;
      --  True if we are generating code invoking a fatptr constructor

      function Has_Fat_Pointer (Typ : Entity_Id) return Boolean;
      --  Return True if Typ is an unconstrained array type or an access to an
      --  unconstrained array type.

      function Is_Array_Formal (N : Node_Id) return Boolean;
      function Is_Constrained_Array_Type (E : Entity_Id) return Boolean;
      function Is_Unconstrained_Array_Formal (N : Node_Id) return Boolean;
      function Is_Unconstrained_Array_Type (E : Entity_Id) return Boolean;
      function Is_Unidimensional_Array_Type (E : Entity_Id) return Boolean;

      procedure Write_Fatptr_Bounds (Expr : Node_Id; Typ : Entity_Id);
      --  Output the low and high bounds of all the dimensions of the array
      --  Expr separated by commas: {low-bound-N ,high-bound-N}

      procedure Write_Fatptr_Compare (Lhs : Node_Id; Rhs : Node_Id);
      --  Output code which compares the fat pointers associated with Lhs and
      --  Rhs expressions. The comparison of fat pointers with constrained
      --  arrays is supported.

      procedure Write_Fatptr_Declare (Array_Type : Entity_Id);
      --  Output the typedef declaration of a multidimensional unconstrained
      --  array types.

      procedure Write_Fatptr_Dereference;
      --  Output a dereference of the fat pointer contents (i.e. ".all")

      procedure Write_Fatptr_Indexed_Component (N : Node_Id);
      --  N is an explicit dereference of a multidimensional unconstrained
      --  array type. Output code which displaces the pointer to reference the
      --  array element.

      procedure Write_Fatptr_First (Array_Type : Entity_Id; Dimension : Pos);
      procedure Write_Fatptr_Last (Array_Type : Entity_Id; Dimension : Pos);
      --  Output a reference to the fat pointer field holding the value of the
      --  First/Last Dimension of Array_Type.

      procedure Write_Number_Of_Components
        (Fatptr     : Node_Id;
         Array_Type : Entity_Id;
         Dimension  : Nat := 0);
      --  Output code which computes the number of components of Array_Type
      --  in the given Dimension. This routine is commonly used to generate
      --  code which displaces the pointer to the base of an array to point
      --  to a given indexed component. For example, for an array of 3x4x2,
      --  the output generated for dimension 1 computes 4x2=8, for dimension
      --  2 computes 2, and for dimension 3 generates no output. Therefore,
      --  it can be used to compute the total number of elements of an array
      --  passing the value Dimension = 0.

      procedure Write_Fatptr_Init
        (Expr          : Node_Id;
         Typ           : Entity_Id;
         Use_Aggregate : Boolean := False);
      --  Output code which initializes the fat pointer associated with Typ
      --  using Expr. For unidimensional unconstrained arrays a call to the
      --  constructor function is generated (unless Use_Aggregate is True);
      --  for multidimensional unconstrained arrays an aggregate is generated.

      procedure Write_Fatptr_Name (Array_Type : Entity_Id);
      --  Output the name of the fatptr typedef associated with the given
      --  unconstrained array type.
   end Fat_Pointers_Support;
   use Fat_Pointers_Support;

   -------------------------
   -- In_Ada_Body_Support --
   -------------------------

   package In_Ada_Body_Support is
      procedure Define_In_Ada_Body;
      --  Define the macro IN_ADA_BODY

      procedure Enter_In_Ada_Body;
      --  Start generating code inside the macro IN_ADA_BODY

      procedure Exit_In_Ada_Body;
      --  Finish generating code inside the macro IN_ADA_BODY

   end In_Ada_Body_Support;
   use In_Ada_Body_Support;

   --------------------
   -- Itypes_Support --
   --------------------

   package Itypes_Support is
      procedure Check_No_Delayed_Itype_Decls;
      --  Check that there are no pending itypes to output

      procedure Dump_Delayed_Itype_Decls;
      --  Output delayed derived itype declarations

      procedure Register_Delayed_Itype_Decl (E : Entity_Id);
      --  Register derived itypes whose declaration cannot be output because
      --  their parent type has not been declared.

      procedure Write_Itypes_In_Subtree (N : Entity_Id);
      --  Write all the itypes defined in the subtree N which have not been
      --  written yet.
   end Itypes_Support;
   use Itypes_Support;

   -------------------------------
   -- Activation Record Support --
   -------------------------------

   package body AREC_Support is

      ------------
      -- ARECnF --
      ------------

      function ARECnF (Subp_Id : Entity_Id) return Node_Id is
      begin
         return Subps.Table (Subp_Index (Subp_Id)).ARECnF;
      end ARECnF;

      ------------
      -- ARECnU --
      ------------

      function ARECnU (Subp_Id : Entity_Id) return Node_Id is
      begin
         return Subps.Table (Subp_Index (Subp_Id)).ARECnU;
      end ARECnU;

      -----------------
      -- AREC_Entity --
      -----------------

      function AREC_Entity (N : Node_Id) return Entity_Id is
         Subp : Entity_Id := Current_Subp_Entity;

      begin
         pragma Assert (Nkind (N) = N_Identifier);
         loop
            declare
               J    : constant SI_Type := UI_To_Int (Subps_Index (Subp));
               Elmt : Elmt_Id;
               STJ  : Subp_Entry renames Subps.Table (J);

            begin
               if Present (STJ.Uents) then
                  Elmt := First_Elmt (STJ.Uents);

                  while Present (Elmt) loop
                     if Entity (N) = Activation_Record_Component (Node (Elmt))
                     then
                        return Node (Elmt);
                     end if;

                     Next_Elmt (Elmt);
                  end loop;
               end if;
            end;

            exit when No (Enclosing_Subprogram (Subp));
            Subp := Enclosing_Subprogram (Subp);
         end loop;

         return Empty;
      end AREC_Entity;

      ---------------------
      -- AREC_Subprogram --
      ---------------------

      function AREC_Subprogram (Formal : Entity_Id) return Entity_Id is
         Subp : Entity_Id := Current_Subp_Entity;

      begin
         pragma Assert (Is_Formal (Formal));
         loop
            declare
               J    : constant SI_Type := UI_To_Int (Subps_Index (Subp));
               Elmt : Elmt_Id;
               STJ  : Subp_Entry renames Subps.Table (J);

            begin
               if Present (STJ.Uents) then
                  Elmt := First_Elmt (STJ.Uents);

                  while Present (Elmt) loop
                     if Node (Elmt) = Formal then
                        return Subp;
                     end if;

                     Next_Elmt (Elmt);
                  end loop;
               end if;
            end;

            exit when No (Enclosing_Subprogram (Subp));
            Subp := Enclosing_Subprogram (Subp);
         end loop;

         return Empty;
      end AREC_Subprogram;

      --------------------
      -- Get_AREC_Field --
      --------------------

      function Get_AREC_Field (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Is_AREC_Reference (N));
         return First (Expressions (N));
      end Get_AREC_Field;

      -----------------------
      -- Is_AREC_Reference --
      -----------------------

      function Is_AREC_Reference (N : Node_Id) return Boolean is
         function Is_Current_AREC_Entity (E : Entity_Id) return Boolean;
         --  Return True if E is the AREC entity of the enclosing subprogram

         ----------------------------
         -- Is_Current_AREC_Entity --
         ----------------------------

         function Is_Current_AREC_Entity (E : Entity_Id) return Boolean is
         begin
            return
              Present (Current_Subp_Entity)
                and then Subps_Index (Current_Subp_Entity) > 0
                and then ARECnF (Current_Subp_Entity) = E;
         end Is_Current_AREC_Entity;

         --  Local variables

         Typ      : constant Entity_Id := Etype (N);
         Expr     : Node_Id;
         Full_Typ : Entity_Id;
         Pref     : Node_Id;

      --  Start of processing for Is_AREC_Reference

      begin
         if not Has_Prefix (N) then
            return False;
         end if;

         Pref := Ultimate_Prefix (Prefix (N));

         if Is_Access_Type (Typ) then
            Full_Typ := Get_Full_View (Designated_Type (Typ));
         else
            Full_Typ := Get_Full_View (Typ);
         end if;

         if Nkind (Pref) = N_Identifier
           and then Is_Current_AREC_Entity (Entity (Pref))
         then
            return True;

         elsif Nkind (N) = N_Attribute_Reference
           and then Get_Attribute_Id (Attribute_Name (N)) = Attribute_Deref
           and then Is_Array_Type (Full_Typ)
           and then Nkind (First (Expressions (N))) = N_Selected_Component
         then
            Expr := First (Expressions (N));
            Pref := Ultimate_Prefix (Prefix (Expr));

            if Nkind (Pref) = N_Identifier
              and then Is_Current_AREC_Entity (Entity (Pref))
              and then Present (AREC_Entity (Selector_Name (Expr)))
            then
               return True;
            end if;
         end if;

         return False;
      end Is_AREC_Reference;

      -------------------------------------
      -- Write_Up_Level_Formal_Reference --
      -------------------------------------

      procedure Write_Up_Level_Formal_Reference
        (Subp   : Entity_Id;
         Formal : Entity_Id)
      is
         procedure Write_Up_Level_AREC_Access
           (Current_Subp   : Entity_Id;
            Enclosing_Subp : Entity_Id);
         --  Output code that climbs through the activation records from
         --  Current_Subp to Enclosing_Subp.

         --------------------------------
         -- Write_Up_Level_AREC_Access --
         --------------------------------

         procedure Write_Up_Level_AREC_Access
           (Current_Subp   : Entity_Id;
            Enclosing_Subp : Entity_Id)
         is
         begin
            if Get_Level (Enclosing_Subp, Current_Subp) > 1 then
               declare
                  Subp_Id : Entity_Id := Enclosing_Subprogram (Current_Subp);

               begin
                  while Subp_Id /= Enclosing_Subp loop
                     Write_Id (ARECnU (Subp_Id));
                     Write_Str ("->");

                     Subp_Id := Enclosing_Subprogram (Subp_Id);
                  end loop;
               end;
            end if;
         end Write_Up_Level_AREC_Access;

      --  Start of processing for Write_Up_Level_Formal_Reference

      begin
         --  Generate
         --    (*((_fatptr_UNCarray *) ARECnF->{ARECnU->})).

         Write_Str ("(*((");
         Write_Fatptr_Name (Get_Full_View (Etype (Formal)));
         Write_Str (" *) ");

         Write_Id (ARECnF (Subp));
         Write_Str ("->");

         Write_Up_Level_AREC_Access
           (Current_Subp => Current_Subp_Entity,
            Enclosing_Subp => AREC_Subprogram (Formal));

         Write_Id (Formal);
         Write_Str ("))");
      end Write_Up_Level_Formal_Reference;
   end AREC_Support;

   ---------------------------
   -- Back_End_Scopes_Stack --
   ---------------------------

   package body Back_End_Scopes_Stack is
      Debug_Extra_Scope_Id : Int := -1;
      --  Initialized to 0 to associate an Id to the extra scopes and output
      --  C comments which facilitate seeing the locations in which the extra
      --  scopes are opened/closed in the generated C file. Initialized to -1
      --  to disable such extra output.

      type Scope_Stack_Entry is record
         Extra_Scope_Id : Nat;
         --  For extra output

         In_Declarations : Boolean;
         --  True when we are processing declarations of this scope

         Is_Extra_Scope : Boolean;
         --  True when this scope was not generated by the front end

         Last_Macro_Index : Nat;
         --  Value of Macro_Table.Last when the scope is opened. Used to
         --  undefine the macros defined in this scope and restore this
         --  value when the scope is closed.

         With_Block     : Boolean;
         --  True if opening this scope forces the output of '{' and closing
         --  it forces the output of '}'
      end record;

      package Scope_Stack is new Table.Table (
        Table_Component_Type => Scope_Stack_Entry,
        Table_Index_Type     => Nat,
        Table_Low_Bound      => 1,
        Table_Initial        => 128,
        Table_Increment      => 100,
        Table_Name           => "Cprint.Scope_Stack");

      -----------------
      -- Close_Scope --
      -----------------

      procedure Close_Scope is
      begin
         --  Exit from all the extra scopes

         while Scope_Stack.Table (Scope_Stack.Last).Is_Extra_Scope loop
            Write_Indent;
            Write_Char ('}');
            Indent_End;

            if Debug_Extra_Scope_Id >= 0 then
               Write_Str (" /* Extra scope ");
               Write_Int (Scope_Stack.Table (Scope_Stack.Last).Extra_Scope_Id);
               Write_Str (" */");
               Write_Indent;
            end if;

            Scope_Stack.Decrement_Last;
         end loop;

         declare
            SST : Scope_Stack_Entry renames
                    Scope_Stack.Table (Scope_Stack.Last);
            Indent_Needed : Boolean := False;
         begin
            if SST.With_Block then
               Write_Char ('}');

               for J in reverse SST.Last_Macro_Index + 1 .. Macro_Table.Last
               loop
                  Write_Indent_Str ("#undef ");
                  Write_Id (Macro_Table.Table (J));
                  Indent_Needed := True;
               end loop;

               if Indent_Needed then
                  Write_Indent;
               end if;

               Macro_Table.Set_Last (SST.Last_Macro_Index);
            end if;
         end;

         --  And finally exit from the current scope

         Scope_Stack.Decrement_Last;
      end Close_Scope;

      -----------------
      -- Close_Scope --
      -----------------

      procedure Close_Scope (Scop_Id : Nat) is
      begin
         loop
            Close_Scope;
            exit when Scope_Stack.Last < Scop_Id;
         end loop;
      end Close_Scope;

      ----------------------
      -- Current_Scope_Id --
      ----------------------

      function Current_Scope_Id return Nat is
      begin
         return Scope_Stack.Last;
      end Current_Scope_Id;

      ---------------------
      -- In_Declarations --
      ---------------------

      function In_Declarations return Boolean is
      begin
         return Scope_Stack.Table (Scope_Stack.Last).In_Declarations;
      end In_Declarations;

      ----------------
      -- Open_Scope --
      ----------------

      procedure Open_Scope (With_Block : Boolean := True) is
         C : constant Character := Last_Char;
      begin
         Scope_Stack.Increment_Last;

         declare
            SST : Scope_Stack_Entry renames
                    Scope_Stack.Table (Scope_Stack.Last);

         begin
            SST.Extra_Scope_Id   := 0;
            SST.In_Declarations  := True;
            SST.Is_Extra_Scope   := False;
            SST.Last_Macro_Index := Macro_Table.Last;
            SST.With_Block       := With_Block;

            if With_Block then
               if C = ';' or C = '}' or C = ASCII.NUL then
                  Write_Indent;
               end if;

               Write_Char ('{');
            end if;
         end;
      end Open_Scope;

      ----------------------
      -- Open_Extra_Scope --
      ----------------------

      procedure Open_Extra_Scope is
      begin
         --  Check cases in which there is no need to create the extra scope

         if not Extra_Scopes_Allowed
           or else Library_Level
           or else Last_Char = '{'
         then
            return;
         end if;

         Open_Scope;
         Scope_Stack.Table (Scope_Stack.Last).Is_Extra_Scope := True;

         if Debug_Extra_Scope_Id >= 0 then
            Debug_Extra_Scope_Id := Debug_Extra_Scope_Id + 1;

            Write_Str (" /* Extra scope ");
            Write_Int (Debug_Extra_Scope_Id);
            Write_Str (" */");

            Scope_Stack.Table (Scope_Stack.Last).Extra_Scope_Id :=
              Debug_Extra_Scope_Id;
         end if;

         Indent_Begin;
      end Open_Extra_Scope;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Scope_Stack.Init;
      end Reset;

      -----------------------
      -- Set_In_Statements --
      ------------------------

      procedure Set_In_Statements is
      begin
         Scope_Stack.Table (Scope_Stack.Last).In_Declarations := False;
      end Set_In_Statements;

      -----------------------
      -- Write_Scope_Stack --
      -----------------------

      procedure Write_Scope_Stack is
      begin
         Write_Eol;
         Write_Str ("---------- Scope_Stack");
         Write_Eol;

         for J in 1 .. Scope_Stack.Last loop
            if Scope_Stack.Table (J).Is_Extra_Scope then
               Write_Char ('*');
            else
               Write_Char (' ');
            end if;

            Write_Int (J);
            Write_Char (':');

            if Scope_Stack.Table (J).In_Declarations then
               Write_Str ("In_Decl");
            else
               Write_Str ("In_Stmts");
            end if;

            if Scope_Stack.Table (J).With_Block then
               Write_Str (" with block");
            end if;

            if Scope_Stack.Table (J).Is_Extra_Scope then
               Write_Str (" (Extra_Scope_Id = ");
               Write_Int (Scope_Stack.Table (J).Extra_Scope_Id);
               Write_Char (')');
            end if;

            Write_Eol;
         end loop;
      end Write_Scope_Stack;
   end Back_End_Scopes_Stack;

   -----------------------------
   -- Back_End_Itypes_Support --
   -----------------------------

   package body Back_End_Itypes_Support is
      Entities_With_Back_End_Itype      : Elist_Id := No_Elist;
      Entities_With_Back_End_AREC_Itype : Elist_Id := No_Elist;

      procedure Register_Entity_With_Back_End_AREC_Itype (E : Entity_Id);
      --  Register E in the list of entities with extra AREC back-end itype

      procedure Register_Entity_With_Back_End_Itype (E : Entity_Id);
      --  Register E in the list of entities with extra back-end itype

      -----------------------------
      -- Declare_Back_End_Itypes --
      -----------------------------

      procedure Declare_Back_End_Itypes (Subp_Id : Entity_Id) is
         function Back_End_Itypes_Needed return Boolean;
         --  Return True if Subp_Id needs back-end itypes

         function Back_End_Itype_Needed (Formal : Entity_Id) return Boolean;
         --  Return True if Formal requires a back-end itype

         procedure Declare_Itype (Formal : Node_Id; Typ : Entity_Id);
         --  Output the typedef which would correspond with the itype of an
         --  access to an unconstrained multidimensional array type.

         procedure Declare_AREC_Itype (Subp : Entity_Id; Formal : Entity_Id);
         --  Output the typedef which would correspond with the itype of the
         --  unconstrained multidimensional array type Formal of the enclosing
         --  subprogram Subp.

         ---------------------------
         -- Back_End_Itype_Needed --
         ---------------------------

         function Back_End_Itype_Needed (Formal : Entity_Id) return Boolean is
         begin
            return
              Is_Access_Type (Etype (Formal))
                and then
                  Is_Unconstrained_Array_Type
                    (Get_Full_View (Designated_Type (Etype (Formal))))
                and then not
                  Is_Unidimensional_Array_Type
                    (Get_Full_View (Designated_Type (Etype (Formal))));
         end Back_End_Itype_Needed;

         ----------------------------
         -- Back_End_Itypes_Needed --
         ----------------------------

         function Back_End_Itypes_Needed return Boolean is
            Formal : Node_Id;

         begin
            Formal := First_Formal_With_Extras (Subp_Id);
            while Present (Formal) loop
               if Back_End_Itype_Needed (Formal) then
                  return True;
               end if;

               Next_Formal_With_Extras (Formal);
            end loop;

            --  For nested procedures check if the enclosing subprograms need
            --  back-end itypes for unconstrained array types.

            declare
               E        : Entity_Id;
               Elmt     : Elmt_Id;
               Subp     : Entity_Id;
               Subp_Idx : SI_Type;

            begin
               Subp := Enclosing_Subprogram (Current_Subp_Entity);
               while Present (Subp) loop
                  Subp_Idx := UI_To_Int (Subps_Index (Subp));

                  if Subp_Idx > 0
                    and then Present (Subps.Table (Subp_Idx).Uents)
                  then
                     Elmt := First_Elmt (Subps.Table (Subp_Idx).Uents);
                     while Present (Elmt) loop
                        E := Node (Elmt);

                        if Is_Unconstrained_Array_Type
                             (Get_Full_View (Etype (E)))
                        then
                           return True;
                        end if;

                        Next_Elmt (Elmt);
                     end loop;
                  end if;

                  Subp := Enclosing_Subprogram (Subp);
               end loop;
            end;

            return False;
         end Back_End_Itypes_Needed;

         ------------------------
         -- Declare_AREC_Itype --
         ------------------------

         procedure Declare_AREC_Itype (Subp : Entity_Id; Formal : Entity_Id) is
            Typ : constant Entity_Id := Get_Full_View (Etype (Formal));

         begin
            --  Do not declare this itype if it has no references

            if Back_End_Stage >= Removing_Decls
              and then not Has_Local_References
                             (Subp => Subp,
                              Typ  => Get_Full_View (Actual_Subtype (Formal)))
            then
               return;
            end if;

            Write_Indent;

            --  Generate
            --    typedef <Component_Type> itypeId
            --      [(last[1]-first[1]) + 1]
            --      [(last[2]-first[2]) + 1]
            --      ...

            Write_Indent;
            Write_Str ("typedef ");
            Write_Id (Component_Type (Typ));
            Write_Char (' ');
            Write_Id (Actual_Subtype (Formal));

            declare
               Idx : Pos     := 1;
               Ind : Node_Id := First_Index (Typ);

            begin
               while Present (Ind) loop
                  Write_Str_Col_Check ("[(");
                  Write_Up_Level_Formal_Reference (Subp, Formal);
                  Write_Char ('.');
                  Write_Fatptr_Last (Typ, Idx);

                  Write_Str_Col_Check (" - ");

                  Write_Up_Level_Formal_Reference (Subp, Formal);
                  Write_Char ('.');
                  Write_Fatptr_First (Typ, Idx);

                  Write_Str_Col_Check (") + 1]");

                  Idx := Idx + 1;
                  Next_Index (Ind);
               end loop;

               Write_Char (';');
            end;

            --  Remember that this entity is defined

            Register_Entity_With_Back_End_AREC_Itype (Actual_Subtype (Formal));

            --  At this stage we cannot invoke Register_Entity() since several
            --  nested subprograms may need to call Declare_AREC_Itype to have
            --  this itype available, and invoking Register_Entity would avoid
            --  declaring this subtype declaration in the outermost subprogram.

            Register_Types_Table_Entity (Actual_Subtype (Formal));

            --  When the machinery that avoids declaring unreferenced types is
            --  enabled we remember that the actual subtype of this formal may
            --  be referenced from the body of nested subprograms; done because
            --  for these entities the common detection of referenced entities
            --  is not enough and we rely on Has_Local_References (see the body
            --  of Cprint_Declare).

            if not Debug_Flag_Dot_6 then
               Types_Table.Get
                 (Get_Full_View
                   (Actual_Subtype (Formal))).Has_AREC_Itype := True;
            end if;
         end Declare_AREC_Itype;

         -------------------
         -- Declare_Itype --
         -------------------

         procedure Declare_Itype (Formal : Node_Id; Typ : Entity_Id) is
            Table_Elmt : Types_Table_Element_Access :=
                           Types_Table.Get (Formal);

         begin
            --  Do not declare this itype if it has no references

            if Back_End_Stage >= Removing_Decls
              and then Table_Elmt /= null
              and then not Table_Elmt.Has_Backend_Itype_Reference
            then
               return;
            end if;

            --  Generate
            --    typedef <Component_Type> itypeId
            --      [(last[1]-first[1]) + 1]
            --      [(last[2]-first[2]) + 1]
            --      ...

            Write_Indent;
            Write_Str ("typedef ");
            Write_Id (Component_Type (Typ));
            Write_Char (' ');
            Write_Back_End_Itype_Id (Formal);

            declare
               Idx : Pos     := 1;
               Ind : Node_Id := First_Index (Typ);

            begin
               while Present (Ind) loop
                  Write_Str_Col_Check ("[(");
                  Write_Id (Formal);

                  if Pass_Pointer (Formal) then
                     Write_Str ("->");
                  else
                     Write_Char ('.');
                  end if;

                  Write_Fatptr_Last (Typ, Idx);
                  Write_Str_Col_Check (" - ");
                  Write_Id (Formal);

                  if Pass_Pointer (Formal) then
                     Write_Str ("->");
                  else
                     Write_Char ('.');
                  end if;

                  Write_Fatptr_First (Typ, Idx);
                  Write_Str_Col_Check (") + 1]");

                  Idx := Idx + 1;
                  Next_Index (Ind);
               end loop;

               Write_Char (';');
            end;

            if Back_End_Stage = Searching_Decls then
               if Table_Elmt = null then
                  Table_Elmt := new Types_Table_Element;
                  Types_Table.Set (Get_Full_View (Formal), Table_Elmt);
               end if;

               Table_Elmt.Has_Backend_Itype_Entity := True;
            end if;
         end Declare_Itype;

         --  Local variables

         Formal : Node_Id;

      --  Start of processing for Declare_Back_End_Itypes

      begin
         if not Back_End_Itypes_Needed then
            return;
         end if;

         Indent_Begin;

         --  Declare itypes associated with the formals of Subp_Id

         Formal := First_Formal_With_Extras (Subp_Id);
         while Present (Formal) loop
            if Back_End_Itype_Needed (Formal) then
               Register_Entity_With_Back_End_Itype (Formal);
               Declare_Itype (Formal,
                 Get_Full_View (Designated_Type (Etype (Formal))));
            end if;

            Next_Formal_With_Extras (Formal);
         end loop;

         --  Declare itypes of unconstrained array type formals of enclosing
         --  subprograms.

         declare
            E        : Entity_Id;
            Elmt     : Elmt_Id;
            Subp     : Entity_Id;
            Subp_Idx : SI_Type;

         begin
            Subp := Enclosing_Subprogram (Current_Subp_Entity);
            while Present (Subp) loop
               Subp_Idx := UI_To_Int (Subps_Index (Subp));

               if Subp_Idx > 0
                 and then Present (Subps.Table (Subp_Idx).Uents)
               then
                  Elmt := First_Elmt (Subps.Table (Subp_Idx).Uents);
                  while Present (Elmt) loop
                     E := Node (Elmt);

                     if Is_Unconstrained_Array_Type
                          (Get_Full_View (Etype (E)))
                     then
                        Declare_AREC_Itype
                          (Subp   => Subp_Id,
                           Formal => E);
                     end if;

                     Next_Elmt (Elmt);
                  end loop;
               end if;

               Subp := Enclosing_Subprogram (Subp);
            end loop;
         end;

         Indent_End;
      end Declare_Back_End_Itypes;

      -----------------------------
      -- Has_Back_End_AREC_Itype --
      -----------------------------

      function Has_Back_End_AREC_Itype (E : Entity_Id) return Boolean is
      begin
         return Contains (Entities_With_Back_End_AREC_Itype, E);
      end Has_Back_End_AREC_Itype;

      ------------------------
      -- Has_Back_End_Itype --
      ------------------------

      function Has_Back_End_Itype (E : Entity_Id) return Boolean is
      begin
         return Contains (Entities_With_Back_End_Itype, E);
      end Has_Back_End_Itype;

      --------------------------
      -- Has_Local_References --
      --------------------------

      function Has_Local_References
        (Subp : Entity_Id;
         Typ  : Entity_Id) return Boolean
      is
         Found_Local_References : Boolean := False;

         function Search_References (N : Node_Id) return Traverse_Result;
         --  Subtree visitor which looks for local references of Typ or its
         --  related expression in Subp.

         -----------------------
         -- Search_References --
         -----------------------

         function Search_References (N : Node_Id) return Traverse_Result is
         begin
            --  Exclude from the search the declaration of the array subtype

            if Nkind (N) = N_Subtype_Declaration
              and then Defining_Identifier (N) = Typ
            then
               return Skip;

            elsif Nkind (N) = N_Identifier then

               --  Direct reference of Typ

               if Entity (N) = Typ then
                  Found_Local_References := True;
                  return Abandon;

               --  Indexed component reference of the related formal

               elsif Entity (N) = Related_Expression (Typ)
                 and then Nkind (Parent (N)) = N_Indexed_Component
               then
                  Found_Local_References := True;
                  return Abandon;

               --  'First/'Last reference of the related formal

               elsif Entity (N) = Related_Expression (Typ)
                 and then Nkind (Parent (N)) = N_Attribute_Reference
                 and then
                   (Get_Attribute_Id (Attribute_Name (Parent (N))) =
                      Attribute_First
                   or else
                    Get_Attribute_Id (Attribute_Name (Parent (N))) =
                      Attribute_Last)
               then
                  Found_Local_References := True;
                  return Abandon;
               end if;
            end if;

            return OK;
         end Search_References;

         procedure Search_Local_Refs is new Traverse_Proc (Search_References);
         --  Subtree visitor instantiation

         --  Local variables

         Current_Subp_Body : Node_Id;

      --  Start of processing for Has_Local_References

      begin
         pragma Assert
           (Ekind (Typ) = E_Array_Subtype
             and then Present (Related_Expression (Typ))
             and then Is_Formal (Related_Expression (Typ)));

         --  No need to traverse the body of the subprogram searching for
         --  references if during the first tree traversal we found no
         --  references in the whole tree.

         if not Has_Type_References (Typ) then
            return False;

         --  If we found references during the first tree traversal we need
         --  to verify that this nested subprogram references it (since the
         --  references may be located in other nested subprograms).

         else
            Current_Subp_Body := Subprogram_Body (Subp);
            Search_Local_Refs (Current_Subp_Body);
            return Found_Local_References;
         end if;
      end Has_Local_References;

      ----------------------------------------------
      -- Register_Entity_With_Back_End_AREC_Itype --
      ----------------------------------------------

      procedure Register_Entity_With_Back_End_AREC_Itype (E : Entity_Id) is
      begin
         Append_New_Elmt (E, Entities_With_Back_End_AREC_Itype);
      end Register_Entity_With_Back_End_AREC_Itype;

      -----------------------------------------
      -- Register_Entity_With_Back_End_Itype --
      -----------------------------------------

      procedure Register_Entity_With_Back_End_Itype (E : Entity_Id) is
      begin
         Append_New_Elmt (E, Entities_With_Back_End_Itype);
      end Register_Entity_With_Back_End_Itype;

      -----------------------------
      -- Write_Back_End_Itype_Id --
      -----------------------------

      procedure Write_Back_End_Itype_Id (E : Entity_Id) is
         Table_Elmt : Types_Table_Element_Access;
      begin
         pragma Assert (Has_Back_End_Itype (E));

         Write_Id (E);
         Write_Str ("_Ib");

         --  Do not take into account calls to this routine performed before
         --  Has_Backend_Itype_Entity is set. Needed because when generating
         --  the typedef declaration of this itype, Declare_Itype() invokes
         --  this routine to output the name of the itype; when the flag
         --  Has_Backend_Itype_Entity is set to True then all calls to this
         --  routine are references to this itype.

         if Back_End_Stage = Searching_Decls
           and then Has_Backend_Itype_Entity (E)
         then
            Table_Elmt := Types_Table.Get (Get_Full_View (E));
            Table_Elmt.Has_Backend_Itype_Reference := True;
         end if;
      end Write_Back_End_Itype_Id;
   end Back_End_Itypes_Support;

   -----------------------------
   -- Back_End_Labels_Support --
   -----------------------------

   package body Back_End_Labels_Support is
      Back_End_Labels : Elist_Id := No_Elist;

      -----------------------------
      -- Register_Back_End_Label --
      -----------------------------

      procedure Register_Back_End_Label (Label_Id : Node_Id) is
      begin
         pragma Assert (Nkind (Label_Id) = N_Defining_Identifier);
         Append_New_Elmt (Label_Id, Back_End_Labels);
      end Register_Back_End_Label;

      -----------------------
      -- Is_Back_End_Label --
      -----------------------

      function Is_Back_End_Label (Label_Id : Node_Id) return Boolean is
      begin
         return Contains (Back_End_Labels, Label_Id);
      end Is_Back_End_Label;

   end Back_End_Labels_Support;

   --------------------------
   -- Fat_Pointers_Support --
   --------------------------

   package body Fat_Pointers_Support is
      procedure Write_Attr_Index (Array_Type : Entity_Id; Dimension : Pos);
      --  Output the reference the Nth attribute of the fat pointer of a
      --  multidimensional array type.

      procedure Write_Name_All;
      --  Output "all"

      procedure Write_Name_First;
      --  Output "first"

      procedure Write_Name_Last;
      --  Output "last"

      ---------------------
      -- Has_Fat_Pointer --
      ---------------------

      function Has_Fat_Pointer (Typ : Entity_Id) return Boolean is
         E : constant Entity_Id := Get_Full_View (Typ);

      begin
         return Is_Unconstrained_Array_Type (E)
           or else
             (Is_Access_Type (E)
                and then Is_Array_Type (Get_Full_View (Designated_Type (E)))
                and then not
                  Is_Constrained (Get_Full_View (Designated_Type (E))));
      end Has_Fat_Pointer;

      ---------------------
      -- Is_Array_Formal --
      ---------------------

      function Is_Array_Formal (N : Node_Id) return Boolean is
         Nod : Node_Id := N;

      begin
         loop
            while Nkind (Nod) in N_Attribute_Reference | N_Explicit_Dereference
            loop
               Nod := Prefix (Nod);
            end loop;

            if Nkind (Nod) in N_Has_Entity
              and then Present (Entity (Nod))
              and then Present (Renamed_Object (Get_Full_View (Entity (Nod))))
            then
               Nod := Renamed_Object (Get_Full_View (Entity (Nod)));
            end if;

            exit when Nkind (Nod) not in
                        N_Attribute_Reference | N_Explicit_Dereference;
         end loop;

         if Nkind (Nod) in N_Has_Entity
           and then Present (Entity (Nod))
           and then Is_Formal (Entity (Nod))
         then
            declare
               Typ : Entity_Id;
            begin
               Typ := Get_Full_View (Etype (Entity (Nod)));

               if Is_Access_Type (Typ) then
                  Typ := Get_Full_View (Designated_Type (Typ));
               end if;

               return Is_Array_Type (Typ);
            end;
         else
            return False;
         end if;
      end Is_Array_Formal;

      -------------------------------
      -- Is_Constrained_Array_Type --
      -------------------------------

      function Is_Constrained_Array_Type (E : Entity_Id) return Boolean is
      begin
         return Is_Array_Type (E) and then Is_Constrained (E);
      end Is_Constrained_Array_Type;

      -----------------------------------
      -- Is_Unconstrained_Array_Formal --
      -----------------------------------

      function Is_Unconstrained_Array_Formal (N : Node_Id) return Boolean is
      begin
         return Is_Array_Formal (N) and then not Is_Constrained (Etype (N));
      end Is_Unconstrained_Array_Formal;

      ---------------------------------
      -- Is_Unconstrained_Array_Type --
      ---------------------------------

      function Is_Unconstrained_Array_Type (E : Entity_Id) return Boolean is
      begin
         return Is_Array_Type (E) and then not Is_Constrained (E);
      end Is_Unconstrained_Array_Type;

      ----------------------------------
      -- Is_Unidimensional_Array_Type --
      ----------------------------------

      function Is_Unidimensional_Array_Type (E : Entity_Id) return Boolean is
         Full_E : constant Entity_Id := Get_Full_View (E);
      begin
         return
           Is_Array_Type (Full_E)
             and then (No (First_Index (Full_E))
                        or else No (Next_Index (First_Index (Full_E))));
      end Is_Unidimensional_Array_Type;

      ----------------------
      -- Write_Attr_Index --
      ----------------------

      procedure Write_Attr_Index (Array_Type : Entity_Id; Dimension : Pos) is
      begin
         if not Is_Unidimensional_Array_Type (Array_Type) then
            Write_Char ('[');
            Write_Int (Dimension - 1);
            Write_Char (']');
         end if;
      end Write_Attr_Index;

      -------------------------
      -- Write_Fatptr_Bounds --
      -------------------------

      procedure Write_Fatptr_Bounds (Expr : Node_Id; Typ : Entity_Id) is
      begin
         if Ekind (Typ) = E_String_Literal_Subtype then
            Write_Array_Bound (Expr, Low, 1);
            Write_Str (", ");
            Write_Array_Bound (Expr, High, 1);

         else
            declare
               Idx : Nat     := 1;
               Ind : Node_Id := First_Index (Typ);

            begin
               while Present (Ind) loop
                  Write_Array_Bound (Expr, Low, Idx);
                  Write_Str (", ");
                  Write_Array_Bound (Expr, High, Idx);

                  Idx := Idx + 1;
                  Next_Index (Ind);

                  if Present (Ind) then
                     Write_Str (", ");
                  end if;
               end loop;
            end;
         end if;
      end Write_Fatptr_Bounds;

      --------------------------
      -- Write_Fatptr_Compare --
      --------------------------

      procedure Write_Fatptr_Compare (Lhs : Node_Id; Rhs : Node_Id) is
         Is_Access : Boolean := False;

         procedure Write_Reference (N : Node_Id; Typ : Node_Id);
         --  Output a reference to N plus a dereference for fat pointers

         ---------------------
         -- Write_Reference --
         ---------------------

         procedure Write_Reference (N : Node_Id; Typ : Node_Id) is
         begin
            if Has_Fat_Pointer (Typ) then
               if Is_Access then
                  Cprint_Node_Paren (N);
               else
                  Cprint_Node (N);
               end if;

               Write_Fatptr_Dereference;
            else
               Cprint_Node (N);
            end if;
         end Write_Reference;

         --  Local variables

         Lhs_Typ : Node_Id := Get_Full_View (Etype (Lhs));
         Rhs_Typ : Node_Id := Get_Full_View (Etype (Rhs));

      --  Start of processing for Write_Fatptr_Compare

      begin
         pragma Assert
           (Has_Fat_Pointer (Lhs_Typ) or else Has_Fat_Pointer (Rhs_Typ));

         if Is_Access_Type (Lhs_Typ) then
            Lhs_Typ   := Get_Full_View (Designated_Type (Lhs_Typ));
            Is_Access := True;
         end if;

         if Is_Access_Type (Rhs_Typ) then
            Rhs_Typ   := Get_Full_View (Designated_Type (Rhs_Typ));
            Is_Access := True;
         end if;

         Write_Str_Col_Check ("(");

         if Nkind (Rhs) = N_Null then
            Write_Reference (Lhs, Lhs_Typ);
            Write_Str (" == ");
            Write_Str ("NULL");

         else
            --  Generate for access types:
            --    Lhs.all == Rhs.all
            --    && Lhs.first == Rhs.first
            --    && Lhs.last == Rhs.last
            --
            --  and for arrays:
            --    sizeof (Lhs) == sizeof(Rhs)
            --    && !memcmp(Lhs.all, Rhs.all, sizeof(...))

            if Is_Access then
               Write_Reference (Lhs, Lhs_Typ);
               Write_Str (" == ");
               Write_Reference (Rhs, Rhs_Typ);

               --  Handle simple case where both operands are fat pointers

               if Has_Fat_Pointer (Lhs_Typ)
                 and then Has_Fat_Pointer (Rhs_Typ)
               then
                  for Idx in 1 .. Number_Dimensions (Lhs_Typ) loop
                     Write_Str_Col_Check (" && ");
                     Cprint_Node (Lhs);
                     Write_Str (".");
                     Write_Fatptr_First (Lhs_Typ, Idx);
                     Write_Str (" == ");
                     Cprint_Node (Rhs);
                     Write_Str (".");
                     Write_Fatptr_First (Rhs_Typ, Idx);
                     Write_Str_Col_Check (" && ");
                     Cprint_Node (Lhs);
                     Write_Str (".");
                     Write_Fatptr_Last (Lhs_Typ, Idx);
                     Write_Str (" == ");
                     Cprint_Node (Rhs);
                     Write_Str (".");
                     Write_Fatptr_Last (Rhs_Typ, Idx);
                  end loop;

               --  Handle case where one operand is a constrained array

               else
                  declare
                     Typ_Idx : Entity_Id;

                  begin
                     --  Initialize Typ_Idx with the first index of the operand
                     --  that is NOT a fat pointer.

                     if Has_Fat_Pointer (Lhs_Typ) then
                        pragma Assert (not Has_Fat_Pointer (Rhs_Typ));
                        Typ_Idx := First_Index (Rhs_Typ);
                     else
                        Typ_Idx := First_Index (Lhs_Typ);
                     end if;

                     for Idx in 1 .. Number_Dimensions (Lhs_Typ) loop
                        Write_Str_Col_Check (" && ");

                        if Has_Fat_Pointer (Lhs_Typ) then
                           Cprint_Node (Lhs);
                           Write_Str (".");
                           Write_Fatptr_First (Lhs_Typ, Idx);

                        elsif Nkind (Typ_Idx) = N_Range then
                           Cprint_Node (Low_Bound (Typ_Idx));

                        else
                           Error_Msg_N ("unsupported array comparison", Lhs);
                        end if;

                        Write_Str (" == ");

                        if Has_Fat_Pointer (Rhs_Typ) then
                           Cprint_Node (Rhs);
                           Write_Str (".");
                           Write_Fatptr_First (Rhs_Typ, Idx);

                        elsif Nkind (Typ_Idx) = N_Range then
                           Cprint_Node (Low_Bound (Typ_Idx));

                        else
                           Error_Msg_N ("unsupported array comparison", Rhs);
                        end if;

                        Write_Str_Col_Check (" && ");

                        if Has_Fat_Pointer (Lhs_Typ) then
                           Cprint_Node (Lhs);
                           Write_Str (".");
                           Write_Fatptr_Last (Lhs_Typ, Idx);

                        elsif Nkind (Typ_Idx) = N_Range then
                           Cprint_Node (High_Bound (Typ_Idx));

                        else
                           Error_Msg_N ("unsupported array comparison", Lhs);
                        end if;

                        Write_Str (" == ");

                        if Has_Fat_Pointer (Rhs_Typ) then
                           Cprint_Node (Rhs);
                           Write_Str (".");
                           Write_Fatptr_Last (Rhs_Typ, Idx);

                        elsif Nkind (Typ_Idx) = N_Range then
                           Cprint_Node (High_Bound (Typ_Idx));

                        else
                           Error_Msg_N ("unsupported array comparison", Rhs);
                        end if;

                        Next_Index (Typ_Idx);
                     end loop;
                  end;
               end if;

            else
               Output_Sizeof (Lhs);
               Write_Str_Col_Check (" == ");
               Output_Sizeof (Rhs);
               Write_Str_Col_Check (" && ");

               Write_Str ("!memcmp(");
               Write_Reference (Lhs, Lhs_Typ);
               Write_Str (", ");
               Write_Reference (Rhs, Rhs_Typ);
               Write_Str (", ");
               Output_Sizeof (Lhs, Rhs);
               Write_Char (')');
            end if;
         end if;

         Write_Char (')');
      end Write_Fatptr_Compare;

      --------------------------
      -- Write_Fatptr_Declare --
      --------------------------

      procedure Write_Fatptr_Declare (Array_Type : Entity_Id) is
         procedure Write_Array_Length (Length : Pos);
         --  Output the length of the array declaration

         ------------------------
         -- Write_Array_Length --
         ------------------------

         procedure Write_Array_Length (Length : Pos) is
         begin
            Write_Char ('[');
            Write_Int (Length);
            Write_Char (']');
         end Write_Array_Length;

      --  Start of processing for Write_Fatptr_Declare

      begin
         pragma Assert (Is_Array_Type (Array_Type)
           and then not Is_Unidimensional_Array_Type (Array_Type));

         Write_Indent;

         --  Generate:

         --    typedef struct _<typeName> {
         --      <typeName> *all;
         --      integer_ptr_t first[N];
         --      integer_ptr_t last[N];
         --    } _fatptr_<typeName>;

         Write_Str ("typedef struct _");
         Write_Id (Array_Type);
         Write_Str (" {");

         Indent_Begin;
         Write_Indent;

         Write_Id (Component_Type (Array_Type));
         Write_Str (" *");
         Write_Name_All;
         Write_Str (";");

         Write_Indent;
         Write_Str ("integer_ptr_t ");
         Write_Name_First;
         Write_Array_Length (Number_Dimensions (Array_Type));
         Write_Char (';');

         Write_Indent;
         Write_Str ("integer_ptr_t ");
         Write_Name_Last;
         Write_Array_Length (Number_Dimensions (Array_Type));
         Write_Char (';');

         Indent_End;
         Write_Indent;

         Write_Str ("} ");
         Write_Fatptr_Name (Array_Type);
         Write_Str (";");
         Write_Indent;
      end Write_Fatptr_Declare;

      ------------------------------
      -- Write_Fatptr_Dereference --
      ------------------------------

      procedure Write_Fatptr_Dereference is
      begin
         Write_Char ('.');
         Write_Name_All;
      end Write_Fatptr_Dereference;

      ------------------------------------
      -- Write_Fatptr_Indexed_Component --
      ------------------------------------

      procedure Write_Fatptr_Indexed_Component (N : Node_Id) is
         Pref      : constant Node_Id   := Unqual_Conv (Prefix (N));
         Pref_Type : constant Entity_Id := Get_Full_View (Etype (Pref));
         Fatptr    : constant Node_Id   := Prefix (Pref);

      begin
         pragma Assert
           (Nkind (N) = N_Indexed_Component
             and then Nkind (Pref) = N_Explicit_Dereference
             and then Is_Unconstrained_Array_Type (Pref_Type)
             and then not Is_Unidimensional_Array_Type (Pref_Type));

         --  Generate code to dereference the resulting computed address

         Write_Str ("(*("); --  Open parenthesis 1 & 2

         --  In practice the following cast is currently not needed since the
         --  type of the pointer defined in the fat pointer struct associated
         --  with multidimensional arrays is a pointer to the component type,
         --  and the first component of the expression generated to compute
         --  the address of the indexed array component is precisely such fat
         --  pointer component (implicitly meaning in C that the arithmetic of
         --  C pointers will use such size to displace the pointer). However,
         --  we generate it to leave the code clear but also to facilitate the
         --  early detection of problems in case of changes in this area since
         --  the correct type of the pointer is essential to ensure that the
         --  resulting values computed by this routine are correct.

         Write_Char ('(');
         Cprint_Node (Component_Type (Pref_Type));
         Write_Str ("*)");

         --  The needed computation is simple: for each dimension generate code
         --  which displaces the pointer as many components as the number of
         --  components of each dimension multiplied by the index. As usual,
         --  given that in C arrays start at 0, the actual value of the index
         --  requires computing its distance to 'first.

         Write_Char ('(');  --  Open parenthesis 3

         Cprint_Node (Fatptr);
         Write_Fatptr_Dereference;

         declare
            Expr : Node_Id := First (Expressions (N));
            Idx  : Pos     := 1;

         begin
            while Idx < Number_Dimensions (Pref_Type) loop

               --  Generate:
               --    + (Expr - fatptr.first[idx]) * Number_Of_Components(Idx)

               Write_Str_Col_Check (" + ");

               Write_Char ('(');
               Cprint_Node (Expr);
               Write_Str_Col_Check (" - ");
               Cprint_Node (Fatptr);
               Write_Str (".");
               Write_Fatptr_First (Pref_Type, Idx);
               Write_Char (')');

               Write_Str_Col_Check (" * ");
               Write_Number_Of_Components (Fatptr, Pref_Type, Idx);

               Next (Expr);
               Idx := Idx + 1;
            end loop;

            --  For the last index generate:
            --    + Expr - fatptr.first[n]

            Write_Str_Col_Check (" + ");
            Cprint_Node (Expr);
            Write_Str_Col_Check (" - ");
            Cprint_Node (Fatptr);
            Write_Str (".");
            Write_Fatptr_First (Pref_Type, Idx);
         end;

         Write_Str (")))"); --  Closing parenthesis 1, 2 & 3
      end Write_Fatptr_Indexed_Component;

      ------------------------
      -- Write_Fatptr_First --
      ------------------------

      procedure Write_Fatptr_First (Array_Type : Entity_Id; Dimension : Pos) is
         pragma Assert (Is_Unconstrained_Array_Type (Array_Type));
      begin
         Write_Name_First;
         Write_Attr_Index (Array_Type, Dimension);
      end Write_Fatptr_First;

      -----------------------
      -- Write_Fatptr_Last --
      -----------------------

      procedure Write_Fatptr_Last (Array_Type : Entity_Id; Dimension : Pos) is
         pragma Assert (Is_Unconstrained_Array_Type (Array_Type));
      begin
         Write_Name_Last;
         Write_Attr_Index (Array_Type, Dimension);
      end Write_Fatptr_Last;

      --------------------------------
      -- Write_Number_Of_Components --
      --------------------------------

      procedure Write_Number_Of_Components
        (Fatptr     : Node_Id;
         Array_Type : Entity_Id;
         Dimension  : Nat := 0)
      is
         procedure Write_Fatptr_Length
           (Fatptr     : Node_Id;
            Array_Type : Entity_Id;
            Dimension  : Pos);
         --  Output code which computes the length of the array in the given
         --  dimension: Fatptr.last[dimension] - Fatptr.first[dimension] + 1

         -------------------------
         -- Write_Fatptr_Length --
         -------------------------

         procedure Write_Fatptr_Length
           (Fatptr     : Node_Id;
            Array_Type : Entity_Id;
            Dimension  : Pos)
         is
         begin
            Cprint_Node (Fatptr);
            Write_Str (".");
            Write_Fatptr_Last (Array_Type, Dimension);

            Write_Str_Col_Check (" - ");

            Cprint_Node (Fatptr);
            Write_Str (".");
            Write_Fatptr_First (Array_Type, Dimension);

            Write_Str_Col_Check (" + 1");
         end Write_Fatptr_Length;

         --  Local variables

         Idx : Nat     := 1;
         Ind : Node_Id := First_Index (Array_Type);

      --  Start of processing for Write_Number_Of_Components

      begin
         --  Locate the index of the given Dimension

         while Idx <= Dimension loop
            Next_Index (Ind);
            Idx := Idx + 1;
         end loop;

         --  Generate code which computes its number of components

         while Idx <= Number_Dimensions (Array_Type) loop
            Write_Char ('(');
            Write_Fatptr_Length (Fatptr, Array_Type, Idx);
            Write_Char (')');

            if Idx < Number_Dimensions (Array_Type) then
               Write_Str_Col_Check (" * ");
            end if;

            Next_Index (Ind);
            Idx := Idx + 1;
         end loop;
      end Write_Number_Of_Components;

      -----------------------
      -- Write_Fatptr_Init --
      -----------------------

      procedure Write_Fatptr_Init
        (Expr          : Node_Id;
         Typ           : Entity_Id;
         Use_Aggregate : Boolean := False)
      is
         procedure Write_Array_Aggregate_Bounds (Expr : Node_Id);
         --  Output the low bound and high bound of all the dimensions of the
         --  type of Expr separated by commas:
         --    low-bound-1 {,low-bound-N} high-bound-1 {,high-bound-N}

         procedure Write_Call_Fatptr_Constructor
           (Expr       : Node_Id;
            Array_Type : Entity_Id);
         --  Generate a call to the constructor of Typ to initialize Expr

         procedure Write_Fatptr_Aggregate
           (Expr       : Node_Id;
            Array_Type : Entity_Id);
         --  Generate an aggregate of Typ to initialize Expr

         ----------------------------------
         -- Write_Array_Aggregate_Bounds --
         ----------------------------------

         procedure Write_Array_Aggregate_Bounds (Expr : Node_Id) is
            Typ : Node_Id;

         begin
            Typ := Get_Full_View (Etype (Expr));

            if Is_Access_Type (Typ) then
               Typ := Get_Full_View (Designated_Type (Typ));
            end if;

            --  Initialize all the components of first[]

            declare
               Idx : Nat := 1;
               Ind : Node_Id := First_Index (Typ);

            begin
               while Present (Ind) loop
                  Write_Array_Bound (Expr, Low, Idx);
                  Write_Str (", ");

                  Idx := Idx + 1;
                  Next_Index (Ind);
               end loop;
            end;

            --  Initialize all the components of last[]

            declare
               Idx : Nat := 1;
               Ind : Node_Id := First_Index (Typ);

            begin
               while Present (Ind) loop
                  Write_Array_Bound (Expr, High, Idx);

                  Idx := Idx + 1;
                  Next_Index (Ind);

                  if Present (Ind) then
                     Write_Str (", ");
                  end if;
               end loop;
            end;
         end Write_Array_Aggregate_Bounds;

         -----------------------------------
         -- Write_Call_Fatptr_Constructor --
         -----------------------------------

         procedure Write_Call_Fatptr_Constructor
           (Expr       : Node_Id;
            Array_Type : Entity_Id)
         is
            Close_Paren : Boolean := True;
            Expr_Typ    : Entity_Id := Get_Full_View (Etype (Expr));
            Saved_Value : constant Boolean := In_Fatptr_Constructor_Call;
            U_Expr      : constant Node_Id := Unqual_Conv (Expr);
            U_Etyp      : constant Entity_Id := Get_Full_View (Etype (U_Expr));

         begin
            if Is_Access_Type (Expr_Typ) then
               Expr_Typ := Get_Full_View (Designated_Type (Expr_Typ));
            end if;

            In_Fatptr_Constructor_Call := True;

            Write_Str ("_fatptr_UNCarray_CONS ");
            Write_Str ("((void*)");

            --  Null fat pointers are initialized with .all = NULL and all its
            --  bounds set to 0.

            if Nkind (U_Expr) = N_Null then
               Write_Str ("NULL, ");

               declare
                  Ind : Node_Id := First_Index (Array_Type);

               begin
                  while Present (Ind) loop
                     Write_Str ("0, 0");
                     Next_Index (Ind);

                     if Present (Ind) then
                        Write_Str (", ");
                     end if;
                  end loop;
               end;

            elsif Nkind (U_Expr) = N_Allocator then
               Cprint_Node (U_Expr);
               Close_Paren := False;

            elsif Nkind (Expr) in
                    N_Type_Conversion | N_Unchecked_Type_Conversion
            then
               Cprint_Node (U_Expr);

               if Has_Fat_Pointer (U_Etyp) then
                  Write_Fatptr_Dereference;
               end if;

               --  The bounds must be computed using the target type of the
               --  type conversion.

               Write_Str (", ");
               Write_Fatptr_Bounds (Expr, Expr_Typ);

            --  Common case

            else
               Cprint_Node (Expr);

               if Has_Fat_Pointer (Expr_Typ) then
                  Write_Fatptr_Dereference;
               end if;

               Write_Str (", ");
               Write_Fatptr_Bounds (Expr, Array_Type);
            end if;

            if Close_Paren then
               Write_Str (")");
            end if;

            In_Fatptr_Constructor_Call := Saved_Value;
         end Write_Call_Fatptr_Constructor;

         ----------------------------
         -- Write_Fatptr_Aggregate --
         ----------------------------

         procedure Write_Fatptr_Aggregate
           (Expr       : Node_Id;
            Array_Type : Entity_Id)
         is
            U_Expr : constant Node_Id := Unqual_Conv (Expr);
            U_Etyp : constant Entity_Id := Get_Full_View (Etype (U_Expr));

         begin
            Write_Char ('(');
            Write_Fatptr_Name (Array_Type);
            Write_Char (')');

            Write_Char ('{');

            Write_Str ("(");
            Write_Id (Component_Type (Array_Type));
            Write_Str ("*) ");

            if Nkind (U_Expr) = N_Null then
               Write_Str ("NULL");

            elsif Nkind (U_Expr) = N_Allocator then
               Cprint_Node (U_Expr);

            else
               --  Do not generate code to compute the address of the entity
               --  if we are propagating a fat pointer.

               if Nkind (U_Expr) in N_Has_Entity
                 and then Is_Formal (Entity (U_Expr))
                 and then Is_Unconstrained_Array_Type (Etype (Entity (U_Expr)))
               then
                  null;
               else
                  Write_Str ("&");
               end if;

               Cprint_Node (U_Expr);

               if Has_Fat_Pointer (U_Etyp) then
                  Write_Fatptr_Dereference;
               end if;
            end if;

            Write_Str (", ");

            --  The bounds must be computed using the type of the original
            --  expression.

            Write_Array_Aggregate_Bounds (Expr);
            Write_Char ('}');
         end Write_Fatptr_Aggregate;

         --  Local variable

         Array_Type : Entity_Id;

      --  Start of processing for Write_Fatptr_Init

      begin
         if Is_Access_Type (Typ) then
            Array_Type := Designated_Type (Typ);
         else
            Array_Type := Typ;
         end if;

         --  This routine must not be invoked with an attribute reference.
         --  Attribute_Reference() must be invoked by the caller (routine
         --  that takes care of invoking this one). The exception of this
         --  rule is attribute 'Deref since the use of this attribute in
         --  constrained array actuals may involve building a fat pointer
         --  using the type of the formal (cf. Cprint_Call).

         pragma Assert (Nkind (Expr) /= N_Attribute_Reference
           or else
             Get_Attribute_Id (Attribute_Name (Expr)) = Attribute_Deref);

         --  Ensure that it is correct to generate the code initializing a fat
         --  pointer.

         pragma Assert (Is_Unconstrained_Array_Type (Array_Type));

         --  Fat pointers of unidimensional arrays are initialized by means of
         --  the constructor to generate code compliant with C90.

         if Is_Unidimensional_Array_Type (Array_Type)
           and then not Use_Aggregate
         then
            Write_Call_Fatptr_Constructor (Expr, Array_Type);

         --  Fat pointers of multidimensional arrays are initialized by means
         --  of an aggregate.

         else
            Write_Fatptr_Aggregate (Expr, Array_Type);
         end if;
      end Write_Fatptr_Init;

      -----------------------
      -- Write_Fatptr_Name --
      -----------------------

      procedure Write_Fatptr_Name (Array_Type : Entity_Id) is
      begin
         pragma Assert (Is_Unconstrained_Array_Type (Array_Type));

         if Is_Unidimensional_Array_Type (Array_Type) then
            Write_Str ("_fatptr_UNCarray");
         else
            Write_Str ("_fatptr_");
            Cprint_Node (Array_Type, Declaration => True);
         end if;
      end Write_Fatptr_Name;

      --------------------
      -- Write_Name_All --
      --------------------

      procedure Write_Name_All is
      begin
         Write_Str ("all");
      end Write_Name_All;

      ----------------------
      -- Write_Name_First --
      ----------------------

      procedure Write_Name_First is
      begin
         Write_Str ("first");
      end Write_Name_First;

      ---------------------
      -- Write_Name_Last --
      ---------------------

      procedure Write_Name_Last is
      begin
         Write_Str ("last");
      end Write_Name_Last;

   end Fat_Pointers_Support;

   -------------------------
   -- In_Ada_Body_Support --
   -------------------------

   package body In_Ada_Body_Support is
      In_Ada_Body : Boolean := False;
      --  Indicates whether we are generating code under control of the macro
      --  definition IN_ADA_BODY

      ------------------------
      -- Define_In_Ada_Body --
      ------------------------

      procedure Define_In_Ada_Body is
      begin
         Write_Str ("#define IN_ADA_BODY");
         Write_Eol;
      end Define_In_Ada_Body;

      -----------------------
      -- Enter_In_Ada_Body --
      -----------------------

      procedure Enter_In_Ada_Body is
      begin
         pragma Assert (not In_Ada_Body);
         Write_Indent_Str ("#ifdef IN_ADA_BODY");
         In_Ada_Body := True;
      end Enter_In_Ada_Body;

      ----------------------
      -- Exit_In_Ada_Body --
      ----------------------

      procedure Exit_In_Ada_Body is
      begin
         pragma Assert (In_Ada_Body);
         Write_Indent_Str ("#endif /* IN_ADA_BODY */");
         In_Ada_Body := False;
      end Exit_In_Ada_Body;

   end In_Ada_Body_Support;

   --------------------
   -- Itypes_Support --
   --------------------

   package body Itypes_Support is
      Delayed_Itype_Decls : Elist_Id := No_Elist;

      ----------------------------------
      -- Check_No_Delayed_Itype_Decls --
      ----------------------------------

      procedure Check_No_Delayed_Itype_Decls is
         Elmt : Elmt_Id;

      begin
         if Delayed_Itype_Decls /= No_Elist then
            Elmt := First_Elmt (Delayed_Itype_Decls);
            while Present (Elmt) loop
               Error_Msg_N ("unsupported type reference", Node (Elmt));
               Next_Elmt (Elmt);
            end loop;
         end if;
      end Check_No_Delayed_Itype_Decls;

      ------------------------------
      -- Dump_Delayed_Itype_Decls --
      ------------------------------

      procedure Dump_Delayed_Itype_Decls is
         Elmt  : Elmt_Id;
         Itype : Entity_Id;

      begin
         if No (Delayed_Itype_Decls) then
            return;
         end if;

         Elmt := First_Elmt (Delayed_Itype_Decls);
         while Present (Elmt) loop
            Itype := Node (Elmt);

            --  Ensure that its parent type has been output before generating
            --  the declaration of the Itype.

            Dump_Type (Etype (Itype));

            --  Cannot invoke here Dump_Type since it would append again Itype
            --  to the list of pending record subtypes thus entering into a
            --  never-ending loop. Hence we invoke directly Cprint_Declare().

            Cprint_Declare (Itype);

            Next_Elmt (Elmt);
         end loop;

         Delayed_Itype_Decls := No_Elist;
      end Dump_Delayed_Itype_Decls;

      ---------------------------------
      -- Register_Delayed_Itype_Decl --
      ---------------------------------

      procedure Register_Delayed_Itype_Decl (E : Entity_Id) is
      begin
         Append_New_Elmt (E, Delayed_Itype_Decls);
      end Register_Delayed_Itype_Decl;

      -----------------------------
      -- Write_Itypes_In_Subtree --
      -----------------------------

      procedure Write_Itypes_In_Subtree (N : Node_Id) is
         function Search_Entities (N : Node_Id) return Traverse_Result;
         --  Subtree visitor which invokes Write_Itype with all the found
         --  entities.

         procedure Write_Itype (Typ : Node_Id);
         --  Subsidiary of Search_Entities. If Typ is an Itype that has not
         --  been written yet, write it. If Typ is any other kind of entity
         --  or tree node, the call is ignored.

         ---------------------
         -- Search_Entities --
         ---------------------

         function Search_Entities (N : Node_Id) return Traverse_Result is
            Def_Id : constant Entity_Id := Defining_Entity_Or_Empty (N);
         begin
            if Present (Def_Id) then
               return Search_Entities (Def_Id);
            end if;

            if Nkind (N) in N_Entity then
               Write_Itype (N);
            end if;

            if Nkind (N) in N_Has_Etype then
               Write_Itype (Etype (N));
            end if;

            return OK;
         end Search_Entities;

         -----------------
         -- Write_Itype --
         -----------------

         procedure Write_Itype (Typ : Node_Id) is
         begin
            if No (Typ)
              or else not Is_Itype (Typ)
              or else Entity_Table.Get (Typ)
            then
               return;
            end if;

            --  Skip types depending on discriminants

            if Size_Depends_On_Discriminant (Typ)
              or else (Is_Array_Type (Typ)
                        and then Depends_On_Discriminant (First_Index (Typ)))
            then
               Register_Entity (Typ);
               return;
            end if;

            pragma Assert (Nkind (Typ) in N_Entity);
            Cprint_Declare (Typ);
         end Write_Itype;

         ------------------
         -- Write_Itypes --
         ------------------

         procedure Write_Itypes is new Traverse_Proc (Search_Entities);
         --  Subtree visitor instantiation

      --  Start of processing for Write_Itypes_In_Subtree

      begin
         Write_Itypes (N);
      end Write_Itypes_In_Subtree;
   end Itypes_Support;

   ----------------------
   -- Check_Attributes --
   ----------------------

   procedure Check_Attributes (E : Entity_Id) is
      procedure Check_Alignment (Ent : Entity_Id);
      --  Check whether an explicit alignment clause is specified, and emit
      --  corresponding C clause if so.

      procedure Check_Linker_Section (Ent : Entity_Id);
      --  Check whether an explicit Linker_Section pragma or aspect is
      --  specified, and emit corresponding C code if so.

      procedure Check_Machine_Attributes (Ent : Entity_Id);
      --  Check whether some explicit Machine_Attribute pragma is specified,
      --  and emit corresponding C code if so.

      function Has_Alignment (E : Entity_Id) return Boolean;
      --  Check whether an explicit alignment clause is specified

      function Has_Linker_Section (E : Entity_Id) return Boolean;
      --  Check whether an explicit Linker_Section pragma or aspect is
      --  specified.

      function Has_Machine_Attributes (E : Entity_Id) return Boolean;
      --  Check whether explicit Machine_Attribute pragmas are specified

      ---------------------
      -- Check_Alignment --
      ---------------------

      procedure Check_Alignment (Ent : Entity_Id) is
      begin
         if Present (Alignment_Clause (Ent)) then
            Write_Indent_Str ("GNAT_ALIGN(");
            Write_Uint (Alignment (Ent));
            Write_Str  (")");
         end if;
      end Check_Alignment;

      --------------------------
      -- Check_Linker_Section --
      --------------------------

      procedure Check_Linker_Section (Ent : Entity_Id) is
         Args : List_Id;
         Sect : Node_Id;

      begin
         --  The front end allows applying pragma/aspect Linker_Section to
         --  types, variables, and subprograms, but the GCC C compiler
         --  restricts the use of attribute 'section' to variables and
         --  subprograms.

         if Present (Linker_Section_Pragma (Ent)) then
            Args := Pragma_Argument_Associations (Linker_Section_Pragma (Ent));
            Sect := Expr_Value_S (Get_Pragma_Arg (Last (Args)));

            if Is_Object (Ent) then
               Write_Char (' ');
            end if;

            Write_Indent_Str ("GNAT_LINKER_SECTION(");
            Write_Str ("""");
            pragma Assert (Nkind (Sect) = N_String_Literal);
            String_To_Name_Buffer (Strval (Sect));
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Str ("""");

            Write_Str  (")");
         end if;
      end Check_Linker_Section;

      ------------------------------
      -- Check_Machine_Attributes --
      ------------------------------

      procedure Check_Machine_Attributes (Ent : Entity_Id) is
         Arg1     : Node_Id;
         Arg2     : Node_Id;
         Arg3     : Node_Id;
         Num_Args : Nat;
         Rep_Item : Node_Id;

      begin
         if No (First_Rep_Item (Ent)) then
            return;
         end if;

         Rep_Item := First_Rep_Item (Ent);

         while Present (Rep_Item) loop
            if Nkind (Rep_Item) = N_Pragma
              and then Pragma_Name (Rep_Item) = Name_Machine_Attribute
            then
               Num_Args :=
                 List_Length (Pragma_Argument_Associations (Rep_Item));

               Arg1 := First (Pragma_Argument_Associations (Rep_Item));
               Arg2 := Next (Arg1);

               Write_Indent_Str ("GNAT_ATTRIBUTE(");
               String_To_Name_Buffer (Strval (Expression (Arg2)));
               Write_Str (Name_Buffer (1 .. Name_Len));

               if Num_Args > 2 then
                  Arg3 := Next (Arg2);
                  Write_Char ('(');

                  if Nkind (Expression (Arg3)) = N_Integer_Literal then
                     Write_Int (UI_To_Int (Intval (Expression (Arg3))));

                  elsif Nkind (Expression (Arg3)) = N_String_Literal then
                     String_To_Name_Buffer (Strval (Expression (Arg3)));
                     Write_Str (Name_Buffer (1 .. Name_Len));

                  else
                     Error_Msg_N ("unsupported argument", Rep_Item);
                  end if;

                  Write_Char (')');
               end if;

               if Num_Args > 3 then
                  Error_Msg_N ("unsupported number of arguments", Rep_Item);
               end if;

               Write_Char (')');
            end if;

            Next_Rep_Item (Rep_Item);
         end loop;
      end Check_Machine_Attributes;

      -------------------
      -- Has_Alignment --
      -------------------

      function Has_Alignment (E : Entity_Id) return Boolean is
      begin
         return Present (Alignment_Clause (E));
      end Has_Alignment;

      ------------------------
      -- Has_Linker_Section --
      ------------------------

      function Has_Linker_Section (E : Entity_Id) return Boolean is
      begin
         return Present (Linker_Section_Pragma (E));
      end Has_Linker_Section;

      ----------------------------
      -- Has_Machine_Attributes --
      ----------------------------

      function Has_Machine_Attributes (E : Entity_Id) return Boolean is
         Rep_Item : Node_Id;

      begin
         if Present (First_Rep_Item (E)) then
            Rep_Item := First_Rep_Item (E);

            while Present (Rep_Item) loop
               if Nkind (Rep_Item) = N_Pragma
                 and then Pragma_Name (Rep_Item) = Name_Machine_Attribute
               then
                  return True;
               end if;

               Next_Rep_Item (Rep_Item);
            end loop;
         end if;

         return False;
      end Has_Machine_Attributes;

   begin
      if Is_Subprogram (E) then
         if Has_Linker_Section (E)
           or else Has_Machine_Attributes (E)
         then
            Indent_Begin;
            Check_Linker_Section (E);
            Check_Machine_Attributes (E);
            Indent_End;
            Write_Indent;
         end if;

      elsif Is_Object (E)
        and then Ekind (E) not in Record_Field_Kind
      then
         if Has_Alignment (E)
           or else Has_Linker_Section (E)
           or else Has_Machine_Attributes (E)
         then
            Indent_Begin;
            Check_Alignment (E);
            Check_Linker_Section (E);
            Check_Machine_Attributes (E);
            Indent_End;
         end if;

      --  Types, record components, and discriminants

      else
         if Has_Alignment (E)
           or else Has_Machine_Attributes (E)
         then
            Indent_Begin;
            Check_Alignment (E);
            Check_Machine_Attributes (E);
            Indent_End;
         end if;
      end if;
   end Check_Attributes;

   ----------------------
   -- Check_Definition --
   ----------------------

   procedure Check_Definition (N : Node_Id; Error_Node : Node_Id := Empty) is
      procedure Check_Entity (E : Entity_Id);
      --  Check that entity E is already defined

      procedure Check_Identifier (N : Node_Id);
      --  Check that the entity associated with this identifier is already
      --  defined.

      function Is_BE_Visible_Type (E : Entity_Id) return Boolean;
      --  Return True if E is a type defined by the backend at library level or
      --  in the current subprogram.

      procedure Report_Error (E : Entity_Id);
      --  Report the error associated with E. If Error_Node is not present the
      --  error is reported on N; otherwise it is reported on Error_Node.

      ------------------
      -- Check_Entity --
      ------------------

      procedure Check_Entity (E : Entity_Id) is
      begin
         --  No need to generate many errors on the same node

         if Error_Posted (E) then
            return;

         elsif (Is_Type (E) or else Ekind (E) = E_Constant)
           and then Error_Posted (Get_Full_View (E))
         then
            return;
         end if;

         if Sloc (E) <= Standard_Location then
            null;

         elsif not Is_Type (E) and then Entity_Table.Get (E) then
            null;

         elsif Is_Type (E) and then Is_BE_Visible_Type (E) then
            null;

         elsif Is_Type (E)
           and then Present (Full_View (E))
           and then Sloc (Get_Full_View (E)) <= Standard_Location
         then
            null;

         elsif Is_Type (E)
           and then Present (Full_View (E))
           and then Is_BE_Visible_Type (Get_Full_View (E))
         then
            null;

         elsif Ekind (E) = E_Constant
           and then
             (Sloc (Get_Full_View (E)) <= Standard_Location
               or else Entity_Table.Get (Get_Full_View (E)))
         then
            null;

         elsif Is_Formal (E) and then Scope (E) = Current_Subp_Entity then
            null;

         --  No check for enumeration literals defined in enclosing subprograms
         --  since in such a case we directly generate their value.

         elsif Is_Enum_Literal_Of_Enclosing_Subprogram (E) then
            null;

         elsif Ekind (E) = E_Enumeration_Literal then
            Check_Entity (Etype (E));

         --  No check needed on the iterator defining identifier since it is
         --  safe.

         elsif Nkind (Parent (E)) = N_Iterator_Specification then
            null;

         --  No check needed on extra labels generated by the back end.

         elsif Ekind (E) = E_Loop and then Is_Back_End_Label (E) then
            null;

         --  When we are unrolling the declarations of the wrapper package of
         --  a subprogram instantiation, instead of reporting an error, we
         --  output this missing declaration.

         elsif Present (Unrolling_Instance_Subp_Id) then
            Cprint_Node (Parent (E));

         else
            Report_Error (E);

            if Is_Private_Type (E) then
               Set_Error_Posted (Get_Full_View (E));
            end if;
         end if;
      end Check_Entity;

      ----------------------
      -- Check_Identifier --
      ----------------------

      procedure Check_Identifier (N : Node_Id) is
         function In_Object_Declaration (N : Node_Id) return Boolean;
         --  Return True if N is part of an object declaration (excluding it
         --  initializing expression or renamed object).

         ---------------------------
         -- In_Object_Declaration --
         ---------------------------

         function In_Object_Declaration (N : Node_Id) return Boolean is
            Par  : Node_Id := N;
            Prev : Node_Id := Empty;

         begin
            while Present (Par) loop
               if Nkind (Par) = N_Object_Declaration then
                  return No (Prev) or else Expression (Par) /= Prev;

               elsif Nkind (Par) = N_Object_Renaming_Declaration then
                  return No (Prev) or else Name (Par) /= Prev;
               end if;

               Prev := Par;
               Par  := Parent (Par);
            end loop;

            return False;
         end In_Object_Declaration;

         --  Local variables

         E : constant Entity_Id := Entity (N);

      --  Start of processing for Check_Identifier

      begin
         --  Skip formals since they are safe if they correspond with the
         --  current subprogram, and they cannot be easily checked if we are
         --  in a nested subprogram.

         if Is_Formal (E) then
            null;

         --  Loop parameters are safe

         elsif Ekind (E) = E_Loop_Parameter then
            null;

         --  The identifier of an exit statement is safe

         elsif Nkind (Parent (N)) = N_Exit_Statement
           and then Name (Parent (N)) = N
         then
            null;

         --  The identifier of a goto statement is safe

         elsif Nkind (Parent (N)) = N_Goto_Statement
           and then Name (Parent (N)) = N
         then
            null;

         --  Skip object declarations and object renamings since the entity is
         --  still undefined.

         elsif In_Object_Declaration (N) then
            null;

         --  Skip references to AREC entities internally built by the back end

         elsif Has_Back_End_AREC_Itype (E) then
            null;

         elsif Nkind (Parent (N)) = N_Selected_Component then
            if N = Prefix (Parent (N)) then
               Check_Entity (E);

            --  N is the selector name; locate the enclosing variable

            else
               declare
                  Pref : Node_Id := Prefix (Parent (N));

               begin
                  while Nkind (Pref) = N_Selected_Component loop
                     Pref := Prefix (Pref);
                  end loop;

                  --  For now we just check identifier prefixes

                  if Nkind (Pref) = N_Identifier then
                     Check_Identifier (Pref);
                  end if;
               end;
            end if;
         else
            Check_Entity (E);
         end if;
      end Check_Identifier;

      ------------------------
      -- Is_BE_Visible_Type --
      ------------------------

      function Is_BE_Visible_Type (E : Entity_Id) return Boolean is
         Enclosing_Subp : constant Entity_Id := Enclosing_Subp_Table.Get (E);

      begin
         return
           Entity_Table.Get (E)
             and then
               (No (Enclosing_Subp)
                 or else Enclosing_Subp = Current_Subp_Entity);
      end Is_BE_Visible_Type;

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (E : Entity_Id) is
         E_Node : Node_Id;

      begin
         if Present (Error_Node) then
            E_Node := Error_Node;
         else
            E_Node := N;
         end if;

         if Is_Type (E) and then not Is_BE_Visible_Type (E) then
            Error_Msg_N ("unsupported type reference", E);

         elsif Is_Type (E)
           and then Present (Full_View (E))
           and then not Is_BE_Visible_Type (Get_Full_View (E))
         then
            Error_Msg_N ("unsupported type reference", E);

         elsif Present (Current_Subp_Entity)
           and then not Is_Library_Level_Entity (Current_Subp_Entity)
         then
            if Is_Type (E) then
               Error_Msg_N
                 ("unsupported reference to type defined in enclosing scope",
                  E_Node);

            elsif Comes_From_Source (N) then
               Error_Msg_N
                 ("unsupported reference to entity defined in enclosing scope",
                  E_Node);
            else
               Error_Msg_N
                 ("unsupported reference to internal entity defined in " &
                  "enclosing scope", E_Node);
            end if;

         elsif Is_Itype (E) then
            Error_Msg_N ("unsupported type reference", E_Node);
         else
            Error_Msg_N ("unsupported entity reference", E_Node);
         end if;

         Set_Error_Posted (N);
      end Report_Error;

   --  Start of processing for Check_Definition

   begin
      --  No need to perform these checks in the tree traversal that locates
      --  types since any output is disabled.

      if Back_End_Stage /= Generating_Output then
         return;
      end if;

      if Nkind (N) = N_Defining_Identifier then
         Check_Entity (N);

      elsif Nkind (N) = N_Identifier then
         Check_Identifier (N);

      elsif Nkind (N) in N_Type_Conversion | N_Unchecked_Type_Conversion
        and then Nkind (Unqual_Conv (N)) = N_Identifier
      then
         Check_Identifier (Unqual_Conv (N));
      end if;
   end Check_Definition;

   ----------------
   -- Check_Sloc --
   ----------------

   function Check_Sloc (S : Source_Ptr) return Boolean is
   begin
      return
        not In_Instantiation (S)
          and then Get_Source_File_Index (S) = Current_Source_File;
   end Check_Sloc;

   ---------------------------
   -- Check_Volatile_Atomic --
   ---------------------------

   procedure Check_Volatile_Atomic (N : Node_Id) is
      Volatile : Boolean := False;

   begin
      if Is_Atomic (N) or else Has_Atomic_Components (N) then
         Error_Msg_N ("??atomic treated as volatile", N);
         Volatile := True;

      elsif Is_Volatile_Full_Access (N) then
         Error_Msg_N ("??volatile_full_access treated as volatile", N);
         Volatile := True;

      else
         Volatile := Is_Volatile (N) or else Has_Volatile_Components (N);
      end if;

      if Volatile then
         Write_Str ("volatile ");
      end if;
   end Check_Volatile_Atomic;

   ---------------
   -- Col_Check --
   ---------------

   procedure Col_Check (N : Nat) is
   begin
      if N + Column > Sprint_Line_Limit then
         Write_Indent_Str ("  ");
      end if;
   end Col_Check;

   -----------------------------------
   -- Compound_Statement_Compatible --
   -----------------------------------

   function Compound_Statement_Compatible (L : List_Id) return Boolean is
      Result : Boolean := True;

      function Search_Complex_Node (Node : Node_Id) return Traverse_Result;
      --  Subtree visitor that looks for nodes incompatible with compound
      --  statements.

      -------------------------
      -- Search_Complex_Node --
      -------------------------

      function Search_Complex_Node (Node : Node_Id) return Traverse_Result is
      begin
         case Nkind (Node) is
            when N_Declaration
               | N_Statement_Other_Than_Procedure_Call
            =>
               if Nkind (Node) not in N_Null_Statement | N_If_Statement then
                  Result := False;
                  return Abandon;
               end if;

            when others =>
               return OK;
         end case;

         return OK;
      end Search_Complex_Node;

      procedure Search is new Traverse_Proc (Search_Complex_Node);
      --  Subtree visitor instantiation

      --  Local variables

      N : Node_Id;

   --  Start of processing for Compound_Statement_Compatible

   begin
      if Is_Non_Empty_List (L) then
         N := First (L);

         loop
            Search (N);
            Next (N);
            exit when N = Empty;
         end loop;
      end if;

      return Result;
   end Compound_Statement_Compatible;

   ---------------------
   -- Cprint_Bar_List --
   ---------------------

   procedure Cprint_Bar_List (List : List_Id) is
      Node : Node_Id;
   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            Cprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
            Write_Str (" | ");
         end loop;
      end if;
   end Cprint_Bar_List;

   -----------------
   -- Cprint_Call --
   -----------------

   procedure Cprint_Call (Node : Node_Id) is
      function Array_Cast_Needed
        (Formal : Node_Id;
         Actual : Node_Id) return Boolean;
      --  Return True if passing Actual to Formal requires casting

      procedure Handle_Access_To_Constrained_Array
        (Formal : Node_Id;
         Actual : Node_Id);
      --  Handle C generation of an access-to-constrained-array actual

      -----------------------
      -- Array_Cast_Needed --
      -----------------------

      function Array_Cast_Needed
        (Formal : Node_Id;
         Actual : Node_Id) return Boolean
      is
      begin
         --  Add a cast on const array parameters to address C compiler
         --  warnings (and MISRA C compliance).

         if Is_Entity_Name (Actual)
           and then Ekind (Entity (Actual)) = E_Constant
           and then Is_Unidimensional_Array_Type (Etype (Formal))
         then
            return True;

         elsif Nkind (Actual) = N_Explicit_Dereference
           and then Is_Unidimensional_Array_Type (Etype (Formal))
         then
            return True;

         elsif Nkind (Actual) = N_String_Literal then
            return True;

         else
            return False;
         end if;
      end Array_Cast_Needed;

      ----------------------------------------
      -- Handle_Access_To_Constrained_Array --
      ----------------------------------------

      procedure Handle_Access_To_Constrained_Array
        (Formal : Node_Id;
         Actual : Node_Id)
      is
         Formal_Array_Type : constant Entity_Id :=
                               Get_Full_View
                                 (Designated_Type (Etype (Formal)));

      --  Start of processing for Handle_Access_To_Constrained_Array

      begin
         if Etype (Formal) /= Etype (Actual)
           or else (Ekind (Etype (Formal)) /= E_Anonymous_Access_Type
                      and then Is_Constrained (Formal_Array_Type))
         then
            if Nkind (Original_Node (Actual)) = N_Allocator then
               Write_Char ('(');
               Write_Id (Etype (Formal));
               Write_Str (") ");

            elsif Is_Out_Mode_Access_Formal (Formal) then
               null;

            elsif Is_Unidimensional_Array_Type (Formal_Array_Type) then
               Write_Char ('(');
               Write_Id (Component_Type (Formal_Array_Type));
               Write_Str ("*) ");
            end if;
         end if;

         --  When the prefix of an access/address attribute reference is an
         --  array, the prefix is a pointer to the array contents and hence
         --  there is no need to compute its address.

         if Nkind (Actual) = N_Attribute_Reference
           and then Is_Access_Attribute_Reference (Actual)
         then
            Cprint_Node (Prefix (Actual));

         --  When the actual and the formal are access to a multidimensional
         --  array type, and the formal is not an OUT or IN-OUT access type,
         --  the formal has been declared using the designated type and we
         --  pass the dereference of the actual.

         elsif Etype (Formal) = Etype (Actual)
           and then not Is_Unidimensional_Array_Type (Formal_Array_Type)
           and then not Is_Out_Mode_Access_Formal (Formal)
         then
            Write_Char ('*');
            Cprint_Node (Actual);

         elsif Has_Fat_Pointer (Etype (Actual)) then
            if Is_Unidimensional_Array_Type (Formal_Array_Type) then
               Cprint_Node (Actual);
               Write_Fatptr_Dereference;

            --  Cast needed on access to multidimensional arrays to avoid
            --  warnings on the generated code.

            else
               Write_Str ("*((");
               Write_Id (Formal_Array_Type);
               Write_Str ("*) ");

               Cprint_Node (Actual);
               Write_Fatptr_Dereference;

               Write_Char (')');
            end if;

         --  Common output

         else
            if Present (Formal) and then Pass_Pointer (Formal) then
               Write_Char ('&');
            end if;

            Cprint_Node (Actual);
         end if;
      end Handle_Access_To_Constrained_Array;

      --  Local variables

      Actual : Node_Id;
      Formal : Node_Id := Empty;
      Call   : Node_Id;

   --  Start of processing for Cprint_Call

   begin
      Write_Itypes_In_Subtree (Node);

      if Nkind (Name (Node)) not in N_Has_Entity then

         --  Can happen in case of a rewritten node, e.g. for
         --  unchecked_conversion

         Call := Name (Node);

         if Nkind (Call) = N_Explicit_Dereference then
            Formal := First_Entity (Designated_Type (Etype (Prefix (Call))));

         --  Report an error on unsupported cases

         else
            declare
               S : constant String := Node_Kind'Image (Nkind (Call));
            begin
               Error_Msg_Strlen := S'Length;
               Error_Msg_String (1 .. Error_Msg_Strlen) := S;
               Error_Msg_N ("unsupported call (~)", Node);
            end;
         end if;

      else
         Call := Entity (Name (Node));
         Formal := First_Formal_With_Extras (Call);
      end if;

      Cprint_Node (Call);
      Write_Char ('(');

      Actual := First_Actual (Node);
      while Present (Actual) loop
         if Present (Formal) then
            if Has_Fat_Pointer (Etype (Formal)) then
               if not Has_Fat_Pointer (Etype (Unqual_Conv (Actual)))
               then
                  if Nkind (Actual) = N_Attribute_Reference
                    and then
                      Get_Attribute_Id
                        (Attribute_Name (Actual)) /= Attribute_Deref
                  then
                     Handle_Attribute (Actual);
                  else
                     Write_Fatptr_Init (Actual, Etype (Formal));
                  end if;

               --  The actual parameter is a fat pointer

               else
                  if Pass_Pointer (Formal) then
                     Write_Char ('&');
                  end if;

                  if Nkind (Actual) = N_Explicit_Dereference then
                     Cprint_Node (Prefix (Actual));
                  else
                     Cprint_Node (Actual);
                  end if;
               end if;

            elsif Is_Access_Type (Etype (Formal))
              and then Is_Array_Type
                         (Get_Full_View (Designated_Type (Etype (Formal))))
              and then not Is_Unconstrained_Array_Type
                             (Get_Full_View (Designated_Type (Etype (Formal))))
            then
               Handle_Access_To_Constrained_Array (Formal, Actual);

            elsif Is_Tagged_Type (Etype (Formal)) then
               declare
                  function Full_Root_Type (E : Entity_Id) return Entity_Id;
                  pragma Inline (Full_Root_Type);
                  --  If E is a class-wide type then return the full view of
                  --  the root type of E; otherwise return the full view of E.

                  --------------------
                  -- Full_Root_Type --
                  --------------------

                  function Full_Root_Type (E : Entity_Id) return Entity_Id is
                     Full_View : Entity_Id :=
                                   Get_Full_View (Underlying_Type (E));
                  begin
                     if Is_Class_Wide_Type (Full_View) then
                        Full_View := Get_Full_View (Root_Type (Full_View));
                     end if;

                     if Ekind (Full_View) = E_Record_Subtype then
                        Full_View := Get_Full_View (Etype (Full_View));
                     end if;

                     return Full_View;
                  end Full_Root_Type;

                  --  Local Variables

                  Actual_Param : constant Entity_Id := Unqual_Conv (Actual);
                  Actual_Typ   : constant Entity_Id :=
                                   Get_Full_View (Etype (Actual_Param));
                  Formal_Typ   : constant Entity_Id :=
                                   Get_Full_View (Etype (Formal));
               begin
                  --  Avoid generating &* if we are passing by reference a
                  --  formal that has been passed by reference.

                  if Nkind (Actual_Param) = N_Identifier
                    and then Is_Formal (Entity (Actual_Param))
                    and then Pass_Pointer (Entity (Actual_Param))
                    and then Full_Root_Type (Actual_Typ) =
                               Full_Root_Type (Formal_Typ)
                  then
                     --  Casting needed for in-mode parameters to avoid C
                     --  warning because they were passed as 'const'.

                     if Ekind (Entity (Actual_Param)) = E_In_Parameter then
                        Write_Char ('(');
                        Write_Id (Formal_Typ);
                        Write_Str ("*)");
                     end if;

                     Write_Id (Actual_Param);

                  elsif Nkind (Actual_Param) = N_Explicit_Dereference then
                     if Full_Root_Type (Actual_Typ) =
                        Full_Root_Type (Formal_Typ)
                     then
                        Cprint_Node (Prefix (Actual_Param));
                     else
                        Write_Char ('(');
                        Write_Id (Formal_Typ);
                        Write_Str ("*)");
                        Cprint_Node (Prefix (Actual_Param));
                     end if;

                  else
                     Write_Char ('(');
                     Write_Id (Formal_Typ);
                     Write_Str ("*)");

                     Write_Char ('&');
                     Cprint_Node (Actual_Param);
                  end if;
               end;

            --  We can directly reference the actual parameter when it is a
            --  formal passed by reference whose type and kind matches with
            --  the expected parameter (which is also passed by reference).

            elsif Pass_Pointer (Formal)
              and then Nkind (Actual) = N_Identifier
              and then Is_Formal (Entity (Actual))
              and then Pass_Pointer (Entity (Actual))
              and then Ekind (Formal) = Ekind (Entity (Actual))
              and then Etype (Formal) = Etype (Entity (Actual))
            then
               Cprint_Node (Entity (Actual));

            else
               if Pass_Pointer (Formal) then
                  if Nkind (Actual) = N_Indexed_Component then
                     Write_Char ('(');
                     Write_Id (Etype (Formal));
                     Write_Str ("*) ");
                  end if;

                  Write_Char ('&');
               else
                  if Array_Cast_Needed (Formal, Actual) then
                     Write_Char ('(');
                     Write_Id
                       (Component_Type (Get_Full_View (Etype (Formal))));
                     Write_Str ("*) ");
                  end if;
               end if;

               --  Strip extra type conversion when passing parameters by
               --  pointer.

               if Nkind (Actual) = N_Type_Conversion
                 and then Pass_Pointer (Formal)
               then
                  Cprint_Node (Expression (Actual));
               else
                  Cprint_Node (Actual);
               end if;
            end if;

            Next_Formal_With_Extras (Formal);

         else
            Cprint_Node (Actual);
         end if;

         Next_Actual (Actual);
         exit when No (Actual);

         Write_Str (", ");
      end loop;

      Write_Char (')');
   end Cprint_Call;

   -----------------------
   -- Cprint_Comma_List --
   -----------------------

   function Cprint_Comma_List (List : List_Id) return Integer is
      Node : Node_Id;
      Num  : Integer := 0;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);
         loop
            if Nkind (Node) /= N_Null_Statement then
               Cprint_Node (Node);
               Num := Num + 1;

               if Last_Char = ';' then
                  Delete_Last_Char;
               end if;
            end if;

            Next (Node);
            exit when Node = Empty;

            if Nkind (Node) /= N_Null_Statement then
               Write_Str (", ");
            end if;
         end loop;
      end if;

      return Num;
   end Cprint_Comma_List;

   procedure Cprint_Comma_List (List : List_Id) is
      Ignore : Integer;
   begin
      Ignore := Cprint_Comma_List (List);
   end Cprint_Comma_List;

   -----------------
   -- Cprint_Copy --
   -----------------

   procedure Cprint_Copy
     (Target     : Node_Id;
      Source     : Node_Id;
      Use_Memcpy : Boolean)
   is
      procedure Write_Param (Param : Node_Id; Param_Typ : Entity_Id);
      --  Output a parameter of the call to memcpy/memmove

      -----------------
      -- Write_Param --
      -----------------

      procedure Write_Param (Param : Node_Id; Param_Typ : Entity_Id) is
         Typ : Entity_Id;

      begin
         if Is_Access_Type (Param_Typ) then
            Typ := Designated_Type (Param_Typ);
         else
            Typ := Param_Typ;
         end if;

         if Requires_Address (Typ) then
            Write_Str ("&");
            Cprint_Node (Param, Declaration => True);

         else
            if Is_Unconstrained_Array_Formal (Param)
              or else Is_Unconstrained_Array_Type (Typ)
            then
               Cprint_Node (Param, Declaration => True);
               Write_Fatptr_Dereference;

            elsif Nkind (Param) = N_Slice
              and then Is_Unconstrained_Array_Formal (Prefix (Param))
            then
               Write_Unconstrained_Array_Prefix (Prefix (Param));
               Write_Str ("+");

               if Nkind (Discrete_Range (Param)) = N_Range then
                  Cprint_Node (Low_Bound (Discrete_Range (Param)));
                  Write_Str ("-");
                  Cprint_Node (Prefix (Param));
                  Write_Str (".");
                  Write_Fatptr_First (Etype (Prefix (Param)), 1);

               else
                  declare
                     S : constant String :=
                           Node_Kind'Image (Nkind (Discrete_Range (Param)));

                  begin
                     Error_Msg_Strlen := S'Length;
                     Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                     Error_Msg_N ("unsupported kind of slice (~)", Source);
                  end;
               end if;
            else
               Cprint_Node (Param, Declaration => True);
            end if;
         end if;
      end Write_Param;

      --  Local variables

      Target_Typ : constant Entity_Id := Get_Full_View (Etype (Target));

      Siz                  : Int;
      Src                  : Node_Id := Source;
      Src_Is_UC            : Boolean := False;
      Src_Typ              : Entity_Id;
      Use_Temp             : Boolean := False;
      Use_Memory_Operation : Boolean := False;

   --  Start of processing for Cprint_Copy

   begin
      --  For nested type conversions and/or unchecked type conversions, take
      --  the innermost source.

      if Nkind (Src) in N_Type_Conversion | N_Unchecked_Type_Conversion then
         Src := Unqual_Conv (Src);
         Src_Is_UC := True;
      end if;

      Src_Typ := Get_Full_View (Etype (Src));

      --  Use simple assignment for elementary objects, elementary record
      --  components, and assignment of unchecked conversion if Target_Typ is
      --  elementary.

      if ((Ekind (Src_Typ) not in Composite_Kind or else Src_Is_UC)
           and then Ekind (Target_Typ) not in Composite_Kind)
        or else
          (Is_Packed_Array (Target_Typ)
            and then Is_Integer_Type (Packed_Array_Impl_Type (Target_Typ))
            and then (not Src_Is_UC
                        or else Src_Typ = Target_Typ
                        or else Src_Typ = Universal_Integer))
        or else
          (Is_Record_Type (Target_Typ)
            and then Esize (Target_Typ) > Uint_0
            and then Esize (Target_Typ) <= Standard_Integer_Size)
      then
         Cprint_Node (Target, Declaration => True);

         if Is_Access_Type (Target_Typ)
           and then Has_Fat_Pointer (Target_Typ)
           and then not Has_Fat_Pointer (Src_Typ)
         then
            Write_Fatptr_Dereference;
         end if;

         Write_Str (" = ");

         if Is_Access_Type (Src_Typ)
           and then Has_Fat_Pointer (Src_Typ)
           and then not Has_Fat_Pointer (Target_Typ)
         then
            Write_Char ('(');
            Write_Id (Target_Typ);
            Write_Str (") ");

            Cprint_Node (Source);
            Write_Fatptr_Dereference;
         else
            Cprint_Node (Source);
         end if;

      --  Composite object kinds

      else
         --  Use a simple assignment when the expression is a function
         --  returning a struct or a struct object/component.

         if Is_Record_Type (Src_Typ)
           and then not Src_Is_UC
           and then Nkind (Src) in
                      N_Function_Call | N_Identifier | N_Selected_Component
         then
            Cprint_Node (Target, Declaration => True);
            Write_Str (" = ");
            Cprint_Node (Src, Declaration => True);

         --  Replace composite assignment by a call to memcpy() or memmove()

         else
            Use_Memory_Operation := True;

            --  Handle cases on which memcpy cannot work directly

            if Nkind (Source) = N_Aggregate
              or else (Nkind (Source) = N_Qualified_Expression
                        and then Nkind (Expression (Source)) = N_Aggregate)
              or else (Nkind (Source) = N_Unchecked_Type_Conversion
                        and then Nkind (Expression (Source)) = N_Function_Call)
            then
               Use_Temp := True;

               Open_Scope;
               Write_Char (' ');
               Write_Itypes_In_Subtree (Src);
               Check_Definition (Src_Typ, Error_Node => Src);
               Cprint_Type_Name (Src_Typ);
               Write_Str (" _tmp = ");
               Cprint_Node (Src, Declaration => True);
               Write_Str (";");
               Write_Indent;
               Set_In_Statements;

            else
               --  Packed record, since memcpy doesn't work on bitfields

               if Nkind (Src) = N_Selected_Component
                 and then Has_Non_Standard_Rep
                            (Get_Full_View (Etype (Prefix (Src))))
               then
                  Siz :=
                    UI_To_Int (Esize (Get_Full_View (Etype (Prefix (Src)))));

                  if Siz > Uint_0 then
                     Use_Temp := True;
                  elsif not Is_Packed (Get_Full_View (Etype (Prefix (Src))))
                  then
                     Error_Msg_N
                       ("unsupported record component reference", Src);
                  end if;

               --  Unchecked conversion of scalar type to composite type

               elsif Nkind (Source) = N_Unchecked_Type_Conversion
                 and then Is_Scalar_Type (Etype (Src))
               then
                  Siz := UI_To_Int (Esize (Src_Typ));

                  --  If dest is an object, generate (*(src_type *)&dest) = src
                  --  to avoid using a temporary.

                  if Nkind (Target) in N_Identifier | N_Expanded_Name then
                     Write_Str ("*((");
                     Cprint_Type_Name (Etype (Src));
                     Write_Str (" *)&");
                     Cprint_Node (Target, Declaration => True);
                     Write_Str (") = ");
                     Cprint_Node (Src, Declaration => True);
                     Use_Memory_Operation := False;

                  else
                     Use_Temp := True;
                  end if;
               end if;

               if Use_Temp then
                  Open_Scope;
                  Write_Char (' ');

                  if Is_Discrete_Type (Etype (Src)) then
                     Write_Integer_Type
                       (Siz,
                        Signed => not Is_Modular_Integer_Type (Etype (Src)));

                  else
                     Check_Definition (Etype (Src), Error_Node => Src);
                     Cprint_Type_Name (Etype (Src));
                  end if;

                  Write_Str (" _tmp = ");
                  Cprint_Node (Src, Declaration => True);
                  Write_Str (";");
                  Set_In_Statements;
               end if;
            end if;

            if Use_Memory_Operation then
               if Last_Char = ';' then
                  Write_Indent;
               end if;

               if Use_Memcpy or Use_Temp then
                  Write_Str ("memcpy((void*)(");
               else
                  Write_Str ("memmove((void*)(");
               end if;

               Write_Param (Target, Target_Typ);
               Write_Str ("), (const void*)(");

               if Use_Temp then
                  Write_Str ("&_tmp");
               else
                  Write_Param (Src, Src_Typ);
               end if;

               Write_Str ("), ");
               Output_Sizeof (Target, Source);
               Write_Char (')');

               if Use_Temp then
                  Write_Char (';');
                  Close_Scope;
               end if;
            end if;
         end if;
      end if;

      --  After generating the assignment or the call to memcopy/memmove
      --  remember that we are now processing statements.

      Set_In_Statements;
   end Cprint_Copy;

   --------------------
   -- Cprint_Declare --
   --------------------

   procedure Cprint_Declare
     (Ent        : Entity_Id;
      Add_Access : Boolean := False;
      Virtual_OK : Boolean := False;
      Semicolon  : Boolean := True)
   is
      function Has_References (E : Entity_Id) return Boolean;
      --  Return True if E has references

      function Is_Skipped_Declaration (E : Entity_Id) return Boolean;
      --  Return True if the declaration of E was skipped

      procedure Mark_Skipped_Declaration (E : Entity_Id);
      --  Remember that E has not been declared

      --------------------
      -- Has_References --
      --------------------

      function Has_References (E : Entity_Id) return Boolean is
      begin
         --  If the first tree traversal was disabled then we cannot know if
         --  E has references. We unconditionally return True to enable the
         --  output of all entities.

         if Debug_Flag_Dot_6 then
            return True;

         elsif Is_Skipped_Declaration (E) then
            return False;

         --  If the entity has an AREC_Itype then we know that it is the actual
         --  subtype of a multidimensional array formal and we need to check if
         --  it is locally referenced (since it may be referenced only from its
         --  nested subprograms).

         elsif Has_AREC_Itype (E) then
            return Has_Local_References (Current_Subp_Entity, E);

         --  Common case: rely on the references seen during the first tree
         --  traversal.

         else
            return Has_Type_References (E);
         end if;
      end Has_References;

      ----------------------------
      -- Is_Skipped_Declaration --
      ----------------------------

      function Is_Skipped_Declaration (E : Entity_Id) return Boolean is
         Table_Elmt : constant Types_Table_Element_Access :=
                        Types_Table.Get (Get_Full_View (E));

      begin
         return Table_Elmt /= null and then Table_Elmt.Declaration_Skipped;
      end Is_Skipped_Declaration;

      ------------------------------
      -- Mark_Skipped_Declaration --
      ------------------------------

      procedure Mark_Skipped_Declaration (E : Entity_Id) is
      begin
         Types_Table.Get (Get_Full_View (E)).Declaration_Skipped := True;
      end Mark_Skipped_Declaration;

      --  Local variables

      Debug          : constant Boolean := False;
      Need_Semicolon : Boolean;

   --  Start of processing for Cprint_Declare

   begin
      --  Only declare each entity once

      if Entity_Table.Get (Ent) then
         if Debug then
            In_Comment := True;
            Write_Eol;
            Write_Str ("/* skipped: ");
            Cprint_Node (Ent);
            Write_Str (" */");
            Write_Eol;
            In_Comment := False;
         end if;

         return;

      elsif Back_End_Stage >= Removing_Decls
        and then not (In_Library_Unit_Pkg_Decl
                       and then Comes_From_Source (Ent))
        and then not (In_Header_File
                       and then Is_Library_Level_Entity (Ent)
                       and then not Special_Elaboration_Code
                       and then No (Current_Subp_Entity))
        and then Is_Type (Ent)
        and then not Has_References (Ent)
      then
         if Debug then
            In_Comment := True;
            Write_Eol;

            if Is_Itype (Ent) then
               Write_Str ("/* skipped itype: ");
            else
               Write_Str ("/* skipped type: ");
            end if;

            Write_Int (Int (Ent));
            Write_Str (":");
            Cprint_Node (Ent);
            Write_Str (" */");
            Write_Eol;
            In_Comment := False;
         end if;

         --  For skipped enumeration types output their C enum declaration
         --  since, although we know that the type is not directly referenced,
         --  their literals may be referenced.

         if Is_Enumeration_Type (Ent)
           and then Etype (Ent) = Ent
           and then Sloc (Ent) > Standard_Location
           and then Present (First_Literal (Ent))
           and then not Types_Table.Get (Ent).Enum_Literals_Declared
         then
            Write_Indent;
            Declare_Enumeration_Type (Ent);
            Write_Char (';');

            Types_Table.Get (Ent).Enum_Literals_Declared := True;
         end if;

         Register_Entity (Ent);
         Mark_Skipped_Declaration (Ent);

         return;
      end if;

      Register_Entity (Ent);

      --  No declaration required for itypes associated with procedures

      if Is_Itype (Ent)
        and then Ekind (Ent) = E_Subprogram_Type
        and then Etype (Ent) = Standard_Void_Type
      then
         return;
      end if;

      --  Handle entities that come from the limited view

      if From_Limited_With (Ent)
        and then Present (Non_Limited_View (Ent))
      then
         declare
            NL_View : constant Entity_Id := Non_Limited_View (Ent);
            NL_Typ  : Entity_Id := NL_View;

         begin
            --  Output code if the nonlimited view is available and it is
            --  not still an incomplete type (if at this stage it is still
            --  an incomplete type it means that we have not seen yet its
            --  full declaration, and it is unclear if we can generate any
            --  C code).

            --  We invoke Cprint_Node with the non-incomplete type declaration
            --  to ensure that we properly unroll private types.

            if Is_Class_Wide_Type (NL_View) then
               NL_Typ := Root_Type (NL_View);

               if Ekind (NL_Typ) /= E_Incomplete_Type then
                  Cprint_Node (Parent (NL_Typ));

                  --  And now we output the declaration of this class-wide type
                  --  declaration.

                  Cprint_Declare (NL_View, Add_Access, Virtual_OK, Semicolon);
               end if;

            else
               if Ekind (NL_Typ) /= E_Incomplete_Type then
                  Cprint_Node (NL_View);
               end if;
            end if;

            return;
         end;
      end if;

      Cprint_Declare_Stack.Append (Ent);

      if Semicolon and Last_Char /= ' ' then
         Write_Indent;
      end if;

      if Is_Type (Ent) then
         Need_Semicolon := Cprint_Type_Reference (Ent, Add_Access, Virtual_OK);
      else
         Need_Semicolon := Cprint_Object_Reference (Ent, Add_Access);
      end if;

      if Semicolon and Need_Semicolon then
         Write_Char (';');
      end if;

      Cprint_Declare_Stack.Decrement_Last;

      if Is_Private_Type (Ent) then
         Register_Entity (Get_Full_View (Ent));
      end if;
   end Cprint_Declare;

   -----------------------
   -- Cprint_Difference --
   -----------------------

   procedure Cprint_Difference (Val1 : Node_Id; Val2 : Uint; B : Boolean) is
      Modular : constant Boolean := Is_Modular_Integer_Type (Etype (Val1));
   begin
      if Compile_Time_Known_Value (Val1) then
         Write_Uint (Expr_Value (Val1) - Val2, Modular => Modular);

      elsif Val2 = Uint_0 then
         Cprint_Node (Val1);

      elsif B then
         Write_Str_Col_Check ("(");
         Cprint_Node (Val1);
         Write_Str_Col_Check (" - ");
         Write_Uint (Val2, Modular => Modular);
         Write_Str_Col_Check (")");

      else
         Cprint_Node (Val1);
         Write_Str_Col_Check (" - ");
         Write_Uint (Val2, Modular => Modular);
      end if;
   end Cprint_Difference;

   procedure Cprint_Difference
     (Val1          : Node_Id;
      Val2          : Node_Id;
      Minus_One_Min : Boolean)
   is
   begin
      if Compile_Time_Known_Value (Val2) then
         Cprint_Difference (Val1, Expr_Value (Val2), Minus_One_Min);

      elsif Is_Entity_Name (Val1) and then Is_Entity_Name (Val2)
        and then Entity (Val1) = Entity (Val2)
      then
         Write_Str_Col_Check ("0");

      else
         --  When Minus_One_Min is True, then generate safeguard:

         --  (Val1 < Val2 ? -1 : Val1 - Val2)

         --  Note that we rely on the front end to remove side effects by
         --  stabilizing values into temporaries, so we do not need to worry
         --  about side effects here.

         if Minus_One_Min then
            Write_Str_Col_Check ("(");
            Cprint_Node (Val1);
            Write_Str_Col_Check (" < (");
            Cprint_Node (Val2);
            Write_Str_Col_Check (") ? -1 : ");
         end if;

         Cprint_Node (Val1);
         Write_Str_Col_Check (" - ");

         --  Add parens around expression if needed

         if Nkind (Val2) in N_Identifier | N_Expanded_Name then
            Cprint_Node (Val2);
         else
            Write_Str_Col_Check ("(");
            Cprint_Node (Val2);
            Write_Str_Col_Check (")");
         end if;

         if Minus_One_Min then
            Write_Str_Col_Check (")");
         end if;
      end if;
   end Cprint_Difference;

   --------------------------
   -- Cprint_Indented_List --
   --------------------------

   procedure Cprint_Indented_List (List : List_Id) is
   begin
      Indent_Begin;
      Cprint_Node_List (List);
      Indent_End;
   end Cprint_Indented_List;

   ----------------------
   -- Cprint_Left_Opnd --
   ----------------------

   procedure Cprint_Left_Opnd (N : Node_Id) is
      Opnd : constant Node_Id := Left_Opnd (N);
   begin
      Cprint_Node_Paren (Opnd);
   end Cprint_Left_Opnd;

   -----------------
   -- Cprint_Node --
   -----------------

   --  WARNING: This routine handles a stack of nodes. Return statements must
   --  be replaced by gotos which jump to the end of the routine and update the
   --  contents of the stack.

   procedure Cprint_Node (Node : Node_Id; Declaration : Boolean := False) is
      Save_Dump_Node : constant Node_Id := Dump_Node;

      procedure Convert_Float_To_Integer (Expr : Node_Id);
      --  Convert a float value to an integer value applying the same reasoning
      --  described in gcc-interface/trans.c (convert_with_check, handling of
      --  !truncatep).

      ------------------------------
      -- Convert_Float_To_Integer --
      ------------------------------

      procedure Convert_Float_To_Integer (Expr : Node_Id) is
         Point_5_Pred : constant String := "0.49999999999999994";
         --  Represents Long_Float'Pred (0.5)

      begin
         --  The code below is duplicating the expression which can be
         --  arbitrarily complex.

         Write_Char ('(');
         Cprint_Node_Paren (Expr);
         Write_Str (" >= 0.0 ? ");
         Cprint_Node_Paren (Expr);
         Write_Str (" + " & Point_5_Pred & " : ");
         Cprint_Node_Paren (Expr);
         Write_Str (" - " & Point_5_Pred & ")");
      end Convert_Float_To_Integer;

   --  Start of processing for Cprint_Node

   begin
      if Node = Empty then
         return;
      end if;

      if Library_Level
        and then (Nkind (Node) in N_Statement_Other_Than_Procedure_Call
                   or else Nkind (Node) in N_Subprogram_Call
                   or else Nkind (Node) = N_Handled_Sequence_Of_Statements
                   or else Nkind (Node) in N_Raise_xxx_Error
                   or else Nkind (Node) = N_Raise_Statement)
      then
         --  Append to list of statements to put in the elaboration procedure
         --  if in main unit, otherwise simply ignore the statement.

         if In_Main_Unit and then Nkind (Node) /= N_Null_Statement then
            Elaboration_Table.Append (Node);
         end if;

         return;
      end if;

      --  Remember that we start processing statements. Needed to enable the
      --  generation of extra scopes (if needed).

      if In_Declarations
        and then
          (Nkind (Node) = N_Procedure_Call_Statement
            or else Nkind (Node) in N_Statement_Other_Than_Procedure_Call
            or else Nkind (Node) in N_Raise_xxx_Error)
        and then Nkind (Node) /= N_Null_Statement
        and then Extra_Scopes_Allowed
      then
         Set_In_Statements;
      end if;

      --  Setup current dump node

      Cprint_Node_Stack.Append (Node);
      Dump_Node := Node;

      --  Select print circuit based on node kind

      case Nkind (Node) is
         when N_Abort_Statement
            | N_Abortable_Part
         =>
            raise Program_Error;

         when N_Abstract_Subprogram_Declaration =>
            null; -- not output in C code

         when N_Accept_Alternative
            | N_Accept_Statement
         =>
            raise Program_Error;

         when N_Access_Definition =>
            if Present (Access_To_Subprogram_Definition (Node)) then
               Cprint_Node
                 (Access_To_Subprogram_Definition (Node), Declaration => True);
            else
               Write_Str_Col_Check ("* ");
               Cprint_Node (Subtype_Mark (Node), Declaration => True);
            end if;

         when N_Access_To_Object_Definition
            | N_Access_Function_Definition
            | N_Access_Procedure_Definition
         =>
            --  Processed by Cprint_Declare as part of processing the parent
            --  node (N_Full_Type_Declaration) or the itypes associated with
            --  anonymous access-to-subprogram types.

            raise Program_Error;

         when N_Aggregate =>
            if Null_Record_Present (Node) then
               null;

            else
               Write_Str_Col_Check ("{");

               if Present (Expressions (Node)) then
                  Cprint_Comma_List (Expressions (Node));

                  if Present (Component_Associations (Node))
                    and then not Is_Empty_List (Component_Associations (Node))
                  then
                     Write_Str (", ");
                  end if;
               end if;

               if Present (Component_Associations (Node))
                 and then not Is_Empty_List (Component_Associations (Node))
               then
                  Indent_Begin;

                  declare
                     Nd : Node_Id;
                  begin
                     Nd := First (Component_Associations (Node));

                     loop
                        Write_Indent;
                        Cprint_Node (Nd);
                        Next (Nd);
                        exit when No (Nd);
                        Write_Str (", ");
                     end loop;
                  end;

                  Indent_End;
               end if;

               Write_Char ('}');
            end if;

         when N_Allocator =>

            --  For now, just handle case of identifier or qualified expression
            --  with no storage pool.

            if No (Storage_Pool (Node)) then
               if Nkind (Expression (Node)) in
                    N_Expanded_Name | N_Identifier | N_Qualified_Expression
               then
                  declare
                     function Allocator_Name (N : Node_Id) return Node_Id;
                     --  Return object name corresponding to the current
                     --  allocator, from node N.

                     --------------------
                     -- Allocator_Name --
                     --------------------

                     function Allocator_Name (N : Node_Id) return Node_Id is
                     begin
                        case Nkind (N) is
                           when N_Object_Declaration =>
                              return Defining_Identifier (N);

                           when N_Assignment_Statement =>
                              return Name (N);

                           when others =>
                              return Empty;
                        end case;
                     end Allocator_Name;

                     Expr  : constant Node_Id := Expression (Node);
                     Typ   : constant Node_Id := Get_Full_View (Etype (Expr));
                     Field : Node_Id;
                     N     : Node_Id;
                     Rng   : Node_Id;

                     Extra_Paren : Boolean := False;
                     Skip_N      : Boolean := False;

                  begin
                     Write_Str_Col_Check ("malloc(sizeof(");

                     --  Handle qualified expression with unconstrained array
                     --  type (for example, new String'("...")).

                     if Nkind (Expr) = N_Qualified_Expression
                       and then Is_Unconstrained_Array_Type (Typ)
                     then
                        Check_Definition
                          (Etype (Expression (Expr)),
                           Error_Node => Node);
                        Cprint_Type_Name (Etype (Expression (Expr)));
                     else
                        Check_Definition (Etype (Expr), Error_Node => Node);
                        Cprint_Type_Name (Etype (Expr));
                     end if;

                     Write_Char (')');

                     if Has_Discriminants (Typ) then
                        Field := Last_Field (Typ);

                        if Has_Per_Object_Constraint (Field)
                          and then Ekind (Etype (Field)) = E_Array_Subtype
                        then
                           --  For a record type with discriminants and whose
                           --  last field depends on this discriminant,
                           --  generate:
                           --    malloc(sizeof(<type>+<size of last field>))

                           Write_Str (" + ");
                           Rng := First_Index (Etype (Field));

                           if Nkind (Rng) = N_Range then

                              --  Note: we do not add +1 here since sizeof()
                              --  already accounts for 1 element.

                              Write_Uint
                                (Intval (High_Bound (Rng)) -
                                 Intval (Low_Bound  (Rng)));
                              Write_Str (" * sizeof(");
                              Check_Definition
                                (Component_Type (Etype (Field)),
                                 Error_Node => Field);
                              Cprint_Type_Name
                                (Component_Type (Etype (Field)));
                              Write_Char (')');

                           else
                              Error_Msg_N
                                ("cannot compute size for field", Field);
                              Write_Char ('0');
                           end if;
                        end if;
                     end if;

                     Write_Char (')');

                     --  If we are invoking a fatptr constructor we must now
                     --  provide the bounds.

                     if In_Fatptr_Constructor_Call then
                        Write_Str (", ");

                        if Nkind (Expr) = N_Qualified_Expression then
                           Write_Fatptr_Bounds (Expression (Expr),
                             Get_Full_View (Etype (Expression (Expr))));
                        else
                           Write_Fatptr_Bounds (Expr,
                             Get_Full_View (Etype (Expr)));
                        end if;

                        Write_Char (')');
                     end if;

                     if Nkind (Expr) = N_Qualified_Expression then
                        if Nkind (Parent (Node)) in N_Assignment_Statement
                                                  | N_Object_Declaration
                                                  | N_Qualified_Expression
                                                  | N_Simple_Return_Statement
                        then
                           Write_Char (';');
                           Write_Indent;

                           if Is_Composite_Type (Typ)
                             and then (not Is_Unconstrained_Array_Type (Typ)
                                        or else Number_Dimensions (Typ) > 1)
                           then
                              Error_Msg_N
                                ("unsupported expression (composite type) " &
                                 "in allocator", Node);
                           end if;

                           Set_In_Statements;

                           if not Is_Unconstrained_Array_Type (Typ) then
                              Write_Str ("*(");
                           end if;

                           N := Allocator_Name (Parent (Node));

                           if No (N) then
                              case Nkind (Parent (Node)) is
                                 when N_Simple_Return_Statement =>
                                    if not Is_Unconstrained_Array_Type (Typ)
                                    then
                                       Write_Str ("_tmp");
                                    end if;

                                    Skip_N := True;

                                 when N_Qualified_Expression =>
                                    N :=
                                      Allocator_Name (Parent (Parent (Node)));

                                    if No (N) then
                                       N := Parent (Parent (Node));

                                       if Nkind (N) = N_Allocator then
                                          N := Allocator_Name (Parent (N));

                                          if Present (N) then
                                             Write_Str ("*(");
                                             Extra_Paren := True;
                                          end if;
                                       end if;
                                    end if;

                                 when others =>
                                    raise Program_Error;
                              end case;
                           end if;

                           if not Skip_N and then No (N) then
                              Error_Msg_N
                                ("unsupported context for allocator", Node);

                           elsif Is_Unconstrained_Array_Type (Typ) then
                              if Skip_N or else No (N) then
                                 Error_Msg_N
                                   ("unsupported context for allocator", Node);
                              else
                                 Cprint_Copy
                                   (Target => N,
                                    Source => Expression (Expr),
                                    Use_Memcpy => True);
                              end if;
                           else
                              if not Skip_N then
                                 Cprint_Node (N);
                              end if;

                              if Extra_Paren then
                                 Write_Char (')');
                              end if;

                              Write_Char (')');
                              Write_Str (" = ");
                              Cprint_Node (Expression (Expr));
                           end if;

                        else
                           declare
                              S : constant String :=
                                    Node_Kind'Image (Nkind (Parent (Node)));
                           begin
                              Error_Msg_Strlen := S'Length;
                              Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                              Error_Msg_N
                                ("unsupported context for allocator (~)",
                                 Node);
                           end;
                        end if;
                     end if;
                  end;

               else
                  declare
                     S : constant String :=
                           Node_Kind'Image (Nkind (Expression (Node)));
                  begin
                     Error_Msg_Strlen := S'Length;
                     Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                     Error_Msg_N ("unsupported kind of allocation (~)", Node);
                  end;

                  Write_Str_Col_Check ("NULL /* new ");
                  Cprint_Node (Expression (Node), Declaration => True);
                  Write_Str_Col_Check (" */");
               end if;

            --  Not a case we handle

            else
               Error_Msg_N ("storage pools not supported", Node);
               Write_Str_Col_Check ("NULL /* new (via storage_pool) ");
               Cprint_Node (Expression (Node), Declaration => True);
               Write_Str_Col_Check (" */");
            end if;

         when N_And_Then =>
            Cprint_Left_Opnd (Node);
            Write_Str (" && ");
            Cprint_Right_Opnd (Node);

         --  Note: the following code for N_Aspect_Specification is not used,
         --  since we deal with aspects as part of a declaration.

         when N_Aspect_Specification =>
            raise Program_Error;

         when N_Assignment_Statement =>
            declare
               LHS : constant Node_Id := Name (Node);
               RHS : constant Node_Id := Expression (Node);
               Typ : constant Node_Id := Get_Full_View (Etype (LHS));
               Op  : Character;

            begin
               Write_Source_Lines (Node);
               Write_Indent;
               Write_Itypes_In_Subtree (Node);

               --  Do not output LHS when RHS is a raise statement (to leave
               --  the C output cleaner).

               if Is_Raise_Statement (RHS) then
                  Cprint_Node (RHS);

               elsif Ekind (Typ) in Composite_Kind
                 or else Nkind (RHS) = N_Unchecked_Type_Conversion
               then
                  --  memcpy() is only safe to use when both Forwards_OK and
                  --  Backwards_OK are True.

                  Cprint_Copy
                    (Target     => LHS,
                     Source     => RHS,
                     Use_Memcpy => Forwards_OK (Node)
                                     and then Backwards_OK (Node));

               elsif Is_Access_Type (Typ)
                 and then Has_Fat_Pointer (Typ)
                 and then Nkind (RHS) = N_Allocator
               then
                  Cprint_Node (LHS, Declaration => True);
                  Write_Str (" = ");
                  Write_Fatptr_Init (RHS, Typ);

               --  Handle conversion of access-to-constrained-array type to
               --  access-to-unconstrained array type. The reverse case is
               --  handled when procesing the N_Type_Conversion node.

               elsif Is_Access_Type (Typ)
                 and then Has_Fat_Pointer (Typ)
                 and then Nkind (RHS) = N_Type_Conversion
                 and then not Has_Fat_Pointer (Etype (Expression (RHS)))
               then
                  Cprint_Node (LHS, Declaration => True);
                  Write_Str (" = ");
                  Write_Fatptr_Init (Expression (RHS), Typ);

               elsif Is_Access_Type (Typ)
                 and then
                  ((Is_Array_Formal (LHS) and then not Is_Array_Formal (RHS))
                      or else
                   (not Is_Array_Formal (LHS) and then Is_Array_Formal (RHS)))
                 and then Is_Constrained_Array_Type
                            (Get_Full_View (Designated_Type (Typ)))
               then
                  Cprint_Node (LHS, Declaration => True);
                  Write_Str (" = ");

                  if Is_Array_Formal (LHS) then

                     --  No casting needed for OUT and IN-OUT access formals

                     if Nkind (LHS) in N_Has_Entity
                       and then Is_Out_Mode_Access_Formal (Entity (LHS))
                     then
                        null;

                     --  No casting needed for constrained multidimensional
                     --  array types.

                     elsif Is_Unidimensional_Array_Type (Designated_Type (Typ))
                     then
                        Write_Char ('(');
                        Write_Id
                          (Component_Type
                            (Get_Full_View (Designated_Type (Typ))));
                        Write_Str ("*)");
                     end if;
                  else
                     Write_Char ('(');
                     Write_Id (Typ);
                     Write_Char (')');
                  end if;

                  Cprint_Node (RHS);

               elsif Is_Access_Type (Typ)
                 and then Is_AREC_Reference (LHS)
               then
                  Cprint_Node (LHS, Declaration => True);
                  Write_Str (" = (");
                  Write_Id (Etype (Get_AREC_Field (LHS)));
                  Write_Str (")");
                  Cprint_Node (RHS);

               else
                  --  Use simple assignment

                  Cprint_Node (LHS, Declaration => True);

                  --  A special case, if we have X = X +/- const, convert to
                  --  the more natural ++/-- or +=/-= notation in the C output.

                  if Is_Entity_Name (LHS)
                    and then Nkind (RHS) in N_Op_Add | N_Op_Subtract
                    and then Is_Entity_Name (Left_Opnd (RHS))
                    and then Entity (LHS) = Entity (Left_Opnd (RHS))
                    and then Nkind (Right_Opnd (RHS)) = N_Integer_Literal
                    and then not Non_Standard_Modular_Type (Etype (LHS))
                  then
                     if Nkind (RHS) = N_Op_Add then
                        Op := '+';
                     else
                        Op := '-';
                     end if;

                     if Intval (Right_Opnd (RHS)) = 1 then
                        Write_Char (Op);
                        Write_Char (Op);
                     else
                        Write_Char (' ');
                        Write_Char (Op);
                        Write_Str ("= ");
                        Cprint_Node (Right_Opnd (RHS));
                     end if;

                  elsif Is_Access_Type (Typ)
                    and then Has_Fat_Pointer (Typ)
                    and then Nkind (RHS) = N_Null
                  then
                     Write_Str (" = ");
                     Write_Fatptr_Init (RHS, Typ);

                  elsif Is_Access_Type (Typ)
                    and then not Has_Fat_Pointer (Typ)
                    and then Has_Fat_Pointer (Etype (RHS))
                  then
                     Write_Str (" = ");

                     Write_Char ('(');
                     Write_Id (Typ);
                     Write_Str (") ");

                     Cprint_Node (RHS);
                     Write_Fatptr_Dereference;

                  elsif Is_Access_Type (Typ)
                    and then Typ /= Get_Full_View (Etype (RHS))
                  then
                     Write_Str (" = (");
                     Write_Id (Typ);
                     Write_Str (") ");
                     Cprint_Node (RHS);

                  --  Normal case of C assignment

                  else
                     Write_Str (" = ");
                     Cprint_Node (RHS);
                  end if;
               end if;

               Write_Char (';');
            end;

         when N_Asynchronous_Select
            | N_At_Clause
         =>
            raise Program_Error;

         when N_Attribute_Definition_Clause =>

            --  The only interesting case left after expansion is for Address
            --  clauses. We only deal with 'Address if the object has a Freeze
            --  node.

            if Get_Attribute_Id (Chars (Node)) = Attribute_Address
              and then Present (Freeze_Node (Entity (Name (Node))))
            then
               --  No additional code needed in the elab procedure if the
               --  address could be declared constant.

               if Special_Elaboration_Code then
                  if not Compile_Time_Known_Value
                           (Unqual_Conv (Expression (Node)))
                  then
                     Write_Indent_Str ("_");
                     Write_Id (Name (Node));
                     Write_Str ("_address = ");
                     Cprint_Node (Expression (Node));
                     Write_Str (";");
                  end if;

               else
                  Write_Source_Lines (Node);

                  if Library_Level and then not In_Main_Unit then
                     Write_Indent_Str ("extern void *_");
                  else
                     Write_Indent_Str ("const void *_");
                  end if;

                  Write_Id (Name (Node));

                  --  Library level entity defined in a withed-unit

                  if Library_Level and then not In_Main_Unit then
                     Write_Str ("_address;");

                  --  Library level entity defined in the current unit whose
                  --  address is specified by means of a variable (initialized
                  --  by the elaboration procedure).

                  elsif Library_Level
                    and then not
                      Compile_Time_Known_Value
                        (Unqual_Conv (Expression (Node)))
                  then
                     Write_Str ("_address;");
                     Elaboration_Table.Append (Node);

                  else
                     Write_Str ("_address = ");
                     Cprint_Node (Expression (Node));
                     Write_Str (";");
                  end if;

                  Write_Eol;
                  Write_Str ("#define ");
                  Write_Id (Name (Node));
                  Write_Str (" (*(");
                  Check_Volatile_Atomic (Entity (Node));
                  Cprint_Node (Etype (Entity (Node)), Declaration => True);
                  Write_Str ("* const)_");
                  Write_Id (Name (Node));
                  Write_Str ("_address)");
                  Write_Eol;

                  --  Record this macro so that it will be #undef'ed at the end
                  --  of the current scope.

                  if not Library_Level then
                     Macro_Table.Append (Name (Node));
                  end if;

                  --  Remember that this entity is defined

                  Register_Entity (Entity (Name (Node)));
               end if;
            end if;

         when N_Attribute_Reference =>
            Handle_Attribute (Node);

         when N_Block_Statement =>
            Write_Source_Lines (Sloc (Node));

            declare
               HSS : constant Node_Id := Handled_Statement_Sequence (Node);
            begin
               --  Detect case of dummy block with no declarations and a single
               --  statement. In this case we can omit the block junk.

               if Is_Empty_List (Declarations (Node))
                 and then List_Length (Statements (HSS)) = 1
               then
                  Set_In_Statements;
                  Cprint_Node (First (Statements (HSS)));

               --  Normal case, we need a block

               else
                  Open_Scope;

                  if Present (Declarations (Node)) then
                     Cprint_Indented_List (Declarations (Node));
                     Write_Indent;
                  end if;

                  Set_In_Statements;
                  Cprint_Node (Handled_Statement_Sequence (Node));

                  Write_Indent;
                  Close_Scope;
               end if;

               --  C90 rejects declarations found after the block (therefore,
               --  remember that we will need to create extra blocks for them!)

               Set_In_Statements;
            end;

         when N_Body_Stub =>
            if Nkind (Node) in N_Protected_Body_Stub | N_Task_Body_Stub then
               raise Program_Error;
            end if;

            --  No action if the separate unit is not available

            if No (Library_Unit (Node)) then
               Error_Msg_N ("separate unit not available", Node);
            else
               Cprint_Node (Get_Body_From_Stub (Node));
            end if;

         --  Call markers are internal tree annotations in the form of nodes
         --  and should not be present in the output.

         when N_Call_Marker =>
            null;

         when N_Case_Expression =>

            --  We should not see case expressions in a fully expanded tree,
            --  since they are always replaced by case statements.

            raise Program_Error;

         when N_Case_Expression_Alternative =>
            raise Program_Error;

         when N_Case_Statement =>
            Write_Source_Lines (Sloc (Node), Last_Line (Expression (Node)));

            declare
               Use_If : Boolean := False;
               Alt    : Node_Id;
               Choice : Node_Id;

            begin
               --  First we do a prescan to see if there are any ranges, if
               --  so, we will have to use an if/else translation since the C
               --  switch statement does not accommodate ranges. Note that we
               --  do not have to test the last alternative, since it
               --  translates to a default anyway without any range tests.

               Alt := First (Alternatives (Node));
               Outer : while Present (Next (Alt)) loop
                  Choice := First (Discrete_Choices (Alt));
                  Inner : while Present (Choice) loop
                     if Nkind (Choice) = N_Range
                       or else (Is_Entity_Name (Choice)
                                 and then Is_Type (Entity (Choice)))
                     then
                        Use_If := True;
                        exit Outer;
                     end if;

                     Next (Choice);
                  end loop Inner;

                  Next (Alt);
               end loop Outer;

               --  Case where we have to use if's

               if Use_If then
                  Alt := First (Alternatives (Node));
                  loop
                     Write_Source_Lines
                       (Sloc (Alt), Last_Line (Last (Discrete_Choices (Alt))));

                     --  First alternative, use if

                     if No (Prev (Alt)) then
                        Write_Indent_Str ("if (");

                     --  All but last alternative, use else if

                     elsif Present (Next (Alt)) then
                        Write_Indent_Str ("else if (");

                     --  Last alternative, use else and we are done

                     else
                        Write_Indent_Str ("else ");
                        Open_Scope;
                        Cprint_Indented_List (Statements (Alt));
                        Write_Source_Lines
                          (Sloc (Node) +
                             Text_Ptr (UI_To_Int (End_Span (Node))));
                        Close_Scope;
                        exit;
                     end if;

                     Choice := First (Discrete_Choices (Alt));
                     loop
                        --  Simple expression, equality test

                        if Nkind (Choice) not in N_Range | N_Subtype_Indication
                          and then (not Is_Entity_Name (Choice)
                                     or else not Is_Type (Entity (Choice)))
                        then
                           Cprint_Node (Expression (Node));
                           Write_Str (" == ");
                           Cprint_Node (Choice);

                        --  Range, do range test

                        else
                           declare
                              LBD : Node_Id;
                              HBD : Node_Id;

                           begin
                              case Nkind (Choice) is
                                 when N_Range =>
                                    LBD := Low_Bound  (Choice);
                                    HBD := High_Bound (Choice);

                                 when N_Subtype_Indication =>
                                    pragma Assert
                                      (Nkind (Constraint (Choice)) =
                                        N_Range_Constraint);

                                    LBD :=
                                      Low_Bound (Range_Expression
                                        (Constraint (Choice)));
                                    HBD :=
                                      High_Bound (Range_Expression
                                        (Constraint (Choice)));

                                 when others =>
                                    LBD := Type_Low_Bound  (Entity (Choice));
                                    HBD := Type_High_Bound (Entity (Choice));
                              end case;

                              Write_Char ('(');
                              Cprint_Node (Expression (Node));
                              Write_Str (" >= ");
                              Write_Uint (Expr_Value (LBD));
                              Write_Str (" && ");
                              Cprint_Node (Expression (Node));
                              Write_Str (" <= ");
                              Write_Uint (Expr_Value (HBD));
                              Write_Char (')');
                           end;
                        end if;

                        if Present (Next (Choice)) then
                           Write_Str_Col_Check (" || ");
                           Next (Choice);
                        else
                           exit;
                        end if;
                     end loop;

                     Write_Str (") ");
                     Open_Scope;
                     Cprint_Indented_List (Statements (Alt));
                     Write_Indent;
                     Close_Scope;

                     Next (Alt);
                  end loop;

               --  Case where we can use Switch

               else
                  Write_Indent_Str ("switch (");
                  Cprint_Node (Expression (Node));
                  Write_Str (") ");
                  Open_Scope;
                  Cprint_Indented_List (Alternatives (Node));
                  Write_Source_Lines
                    (Sloc (Node) + Text_Ptr (UI_To_Int (End_Span (Node))));
                  Write_Indent;
                  Close_Scope;
               end if;
            end;

         when N_Case_Statement_Alternative =>
            Write_Source_Lines
              (Sloc (Node), Last_Line (Last (Discrete_Choices (Node))));

            declare
               Choices     : constant List_Id := Discrete_Choices (Node);
               Choice      : Node_Id;
               Default     : Boolean := False;
               Extra_Block : Boolean := False;

            begin
               Choice := First (Choices);
               while Present (Choice) loop
                  if Nkind (Choice) = N_Others_Choice then
                     Write_Indent_Str ("default:");
                     Default := True;
                  else
                     Write_Indent_Str ("case ");
                     Cprint_Node (Choice);
                     Write_Str (":");
                  end if;

                  Next (Choice);
               end loop;

               if Has_Non_Null_Statements (Statements (Node)) then
                  if List_Length (Statements (Node)) > 1
                    or else Nkind (First (Statements (Node))) =
                              N_Object_Declaration
                  then
                     Write_Char (' ');
                     Open_Scope;
                     Extra_Block := True;
                  end if;

                  Cprint_Indented_List (Statements (Node));

               elsif Default then
                  Write_Str (" /* No statement */");
               end if;

               if Extra_Block then
                  Write_Char (' ');
                  Close_Scope;
               end if;

               Indent_Begin;
               Write_Indent_Str ("break;");
               Indent_End;
            end;

         when N_Character_Literal =>
            if Column > Sprint_Line_Limit - 2 then
               Write_Indent_Str ("  ");
            end if;

            --  If an Entity is present, it means that this was one of the
            --  literals in a user-defined character type. In that case, return
            --  the Enumeration_Rep of the entity. Otherwise, use the character
            --  code.

            if Present (Entity (Node)) then
               Write_Uint (Enumeration_Rep (Entity (Node)));
            else
               Write_Char (''');
               Write_C_Char_Code (UI_To_CC (Char_Literal_Value (Node)));
               Write_Char (''');
            end if;

         when N_Code_Statement =>
            Write_Source_Lines (Node);

            Write_Indent;
            Cprint_Node (Expression (Node));
            Write_Char (';');

         when N_Compilation_Unit
            | N_Compilation_Unit_Aux
         =>
            raise Program_Error;

         when N_Component_Association =>
            Cprint_Node (Expression (Node));

         when N_Component_Clause =>
            raise Program_Error;

         when N_Component_Definition =>

            --  Ada 2005 (AI-230): Access definition components

            if Present (Access_Definition (Node)) then
               Cprint_Node (Access_Definition (Node), Declaration => True);
            else
               pragma Assert (Present (Subtype_Indication (Node)));

               --  Ada 2005 (AI-231)

               Cprint_Node (Subtype_Indication (Node), Declaration => True);
            end if;

         when N_Component_Declaration =>
            raise Program_Error;

         when N_Component_List =>
            raise Program_Error;

         when N_Compound_Statement =>
            raise Program_Error;

         when N_Conditional_Entry_Call
            | N_Constrained_Array_Definition
            | N_Contract
            | N_Decimal_Fixed_Point_Definition
         =>
            raise Program_Error;

         when N_Defining_Character_Literal =>

            --  For enumeration literals of enumeration types that have a
            --  representation clause use directly their value.

            if Ekind (Node) = E_Enumeration_Literal
              and then
                Has_Enumeration_Rep_Clause (Get_Full_View (Etype (Node)))
            then
               Write_Uint (Enumeration_Rep (Node));
            else
               Write_Name_Col_Check (Chars (Ultimate_Alias (Node)));
            end if;

         when N_Defining_Identifier =>

            --  Replace constant references by the direct values, to avoid
            --  a level of indirection for e.g. private values, and since
            --  we are not trying to generate human readable code, losing
            --  the reference to the constant object is not a problem. In
            --  addition, this allows generation of static values and static
            --  aggregates.

            if not Declaration
              and then Is_Constant_Folded (Node)
            then
               declare
                  N    : constant Node_Id := Get_Full_View (Node);
                  Decl : constant Node_Id := Declaration_Node (N);
                  Expr : Node_Id := Empty;

               begin
                  if Nkind (Decl) /= N_Object_Renaming_Declaration then
                     Expr := Expression (Decl);
                  end if;

                  if Present (Expr)
                    and then Nkind (Expr) in N_Character_Literal
                                           | N_Expanded_Name
                                           | N_Integer_Literal
                                           | N_Real_Literal
                  then

                     --  Add a cast to System.Address to avoid mismatch between
                     --  integer and pointer.

                     if Is_Descendant_Of_Address (Etype (N)) then
                        Write_Str ("(system__address)");
                     end if;

                     Cprint_Node (Expr);

                  elsif Present (Expr) and then Nkind (Expr) = N_Identifier
                  then
                     if Ekind (Entity (Expr)) = E_Enumeration_Literal then
                        Write_Uint (Enumeration_Rep (Entity (Expr)));
                     elsif Library_Level then
                        Cprint_Node (Expr);
                     else
                        Write_Id (N);
                     end if;
                  else
                     Write_Id (N);
                  end if;
               end;

            elsif Is_Formal (Node)
              and then Is_Unconstrained_Array_Type (Etype (Node))
              and then Present (Activation_Record_Component (Node))
              and then Present (Current_Subp_Entity)
              and then not Within_Scope (Node, Current_Subp_Entity)
            then
               Write_Up_Level_Formal_Reference
                 (Subp   => Current_Subp_Entity,
                  Formal => Node);

            --  For enumeration literals defined in the enclosing scope of a
            --  nested subprogram we directly generate their values. Thus, we
            --  avoid the need to duplicate the declaration of the enum in the
            --  enclosing subprograms.

            elsif Is_Enum_Literal_Of_Enclosing_Subprogram (Node) then
               Write_Uint (Enumeration_Rep (Node));

            --  For enumeration literals of enumeration types that have a
            --  representation clause use directly their value.

            elsif Ekind (Node) = E_Enumeration_Literal
              and then Has_Or_Inherits_Enum_Rep_Clause (Etype (Node))
            then
               declare
                  Typ         : Entity_Id := Etype (Node);
                  Enum_Rep    : Uint;
                  LPosByte    : constant Uint := (Uint_2 ** 8) - 1;
                  LPosWord16  : constant Uint := (Uint_2 ** 16) - 1;
                  LPosWord32  : constant Uint := (Uint_2 ** 32) - 1;

               begin
                  --  Handle derived types

                  while Etype (Typ) /= Typ loop
                     Typ := Etype (Typ);
                  end loop;

                  Enum_Rep := Enumeration_Rep (Node);

                  --  If the enumeration value is negative we cannot output
                  --  its value as a negative number because it is not allowed
                  --  by the C compiler for aggregates.

                  if Enum_Rep < 0
                    and then Is_Unsigned_Or_Modular_Type (Typ)
                  then
                     if Esize (Typ) = 8 then
                        Enum_Rep := LPosByte + Enum_Rep + 1;

                     elsif Esize (Typ) = 16 then
                        Enum_Rep := LPosWord16 + Enum_Rep + 1;

                     elsif Esize (Typ) = 32 then
                        Enum_Rep := LPosWord32 + Enum_Rep + 1;
                     end if;
                  end if;

                  Write_Uint (Enum_Rep,
                    Modular => Is_Unsigned_Or_Modular_Type (Typ));
               end;

            --  For private types whose full-view is an itype use their full
            --  view. Required to ensure generating a consistent name when
            --  its partial or private view entity are referenced since the
            --  output of Write_Id depends on their decoration.

            elsif Is_Private_Type (Node)
              and then Is_Itype (Get_Full_View (Node))
            then
               Write_Id (Get_Full_View (Node));

            else
               Write_Id (Node);
            end if;

         when N_Defining_Operator_Symbol =>
            Write_Name_Col_Check (Chars (Node));

         when N_Defining_Program_Unit_Name =>
            Cprint_Node (Defining_Identifier (Node));

         when N_Delay_Alternative
            | N_Delay_Relative_Statement
            | N_Delay_Until_Statement
         =>
            raise Program_Error; -- should not occur in generated code

         when N_Delta_Constraint =>
            raise Program_Error;

         when N_Derived_Type_Definition =>
            raise Program_Error;

         when N_Designator
            | N_Digits_Constraint
         =>
            raise Program_Error;

         when N_Discriminant_Association =>
            raise Program_Error;

         when N_Discriminant_Specification =>
            raise Program_Error;

         when N_Elsif_Part =>
            Write_Source_Lines (Sloc (Node), Last_Line (Condition (Node)));
            Write_Indent_Str ("else if (");
            Cprint_Node (Condition (Node));
            Write_Char (')');

            Write_Char (' ');
            Open_Scope;
            Cprint_Indented_List (Then_Statements (Node));
            Write_Indent;
            Close_Scope;

         when N_Empty =>
            null;

         when N_Entry_Body
            | N_Entry_Body_Formal_Part
            | N_Entry_Call_Alternative
            | N_Entry_Call_Statement
            | N_Entry_Declaration
            | N_Entry_Index_Specification
         =>
            raise Program_Error; -- should not occur in generated code

         when N_Enumeration_Representation_Clause
            | N_Enumeration_Type_Definition
         =>
            null; -- not output in C code

         when N_Error =>
            Write_Str_Col_Check ("<error>");

         when N_Exception_Handler =>
            null; -- not output in C code

         when N_Exception_Declaration
            | N_Exception_Renaming_Declaration
         =>
            if not In_Declarations then
               Open_Extra_Scope;
            end if;

            Write_Source_Lines (Node);
            Write_Indent;

            if not In_Main_Unit then
               Write_Str ("extern ");
            end if;

            Write_Str ("void* ");
            Cprint_Node (Defining_Identifier (Node));
            Write_Char (';');

            --  Remember that this entity is defined

            Register_Entity (Defining_Identifier (Node));

         when N_Exit_Statement =>
            Write_Source_Lines (Node);

            if Present (Condition (Node)) then
               Write_Indent_Str ("if (");
               Cprint_Node (Condition (Node));
               Write_Str (") ");
               Open_Scope;
               Indent_Begin;
            end if;

            if No (Name (Node)) then
               declare
                  In_Case_Stmt : Boolean := False;
                  Loop_Stmt    : Node_Id;

               begin
                  --  Locate the enclosing loop statement remembering if this
                  --  exit statement is located inside some inner case stmt
                  --  because in such case we cannot exit from the inner loop
                  --  by means of a 'break' C statement (we must generate a
                  --  'goto').

                  for J in reverse 1 .. Cprint_Node_Stack.Last - 1 loop
                     Loop_Stmt := Cprint_Node_Stack.Table (J);
                     exit when Nkind (Loop_Stmt) = N_Loop_Statement;

                     if Nkind (Loop_Stmt) = N_Case_Statement then
                        In_Case_Stmt := True;
                     end if;
                  end loop;

                  if not In_Case_Stmt then
                     Write_Indent_Str ("break;");
                  else
                     Write_Indent_Str ("goto ");
                     Cprint_Node (Identifier (Loop_Stmt), Declaration => True);
                     Write_Char (';');

                     Register_Back_End_Label (Entity (Identifier (Loop_Stmt)));
                  end if;
               end;
            else
               Write_Indent_Str ("goto ");
               Cprint_Node (Name (Node), Declaration => True);
               Write_Char (';');
            end if;

            if Present (Condition (Node)) then
               Indent_End;
               Write_Indent;
               Close_Scope;
            end if;

         when N_Expanded_Name =>

            --  At this stage, all names should have been expanded, so any
            --  remaining expanded names can be handled via their Entity.

            Cprint_Node (Entity (Node), Declaration);

         when N_Explicit_Dereference =>

            --  For subprogram types we generate a typedef and hence the
            --  explicit dereference is not needed.

            if Ekind (Etype (Node)) = E_Subprogram_Type then
               null;

            --  When the prefix of the explicit dereference is a reference to
            --  a multidimensional array formal we must not generate C code to
            --  dereference the pointer, because the formal has been defined in
            --  the profile of the C function as a C array (it is not defined
            --  as a pointer to the component).

            elsif Is_Array_Formal (Prefix (Node))
              and then not
                Is_Unconstrained_Array_Type (Etype (Prefix (Node)))
              and then not
                Is_Unidimensional_Array_Type (Etype (Prefix (Node)))
            then
               null;

            elsif Has_Fat_Pointer (Etype (Node))
              and then not Is_Access_Type (Etype (Node))
            then
               null;

            else
               Write_Char ('*');
            end if;

            Cprint_Node_Paren (Prefix (Node));

         when N_Expression_With_Actions =>
            if Is_Non_Empty_List (Actions (Node)) then

               --  Map N_Expression_With_Actions to a compound statement if it
               --  is simple enough, otherwise use a braced-group.

               if Compound_Statement_Compatible (Actions (Node)) then
                  declare
                     Saved_In_Compound_Statement : constant Boolean :=
                       In_Compound_Statement;

                  begin
                     Write_Char ('(');
                     In_Compound_Statement := True;

                     if Cprint_Comma_List (Actions (Node)) /= 0 then
                        Write_Str (", ");
                     end if;

                     In_Compound_Statement := Saved_In_Compound_Statement;
                     Cprint_Node (Expression (Node));
                     Write_Char (')');
                  end;

               else
                  declare
                     ESA_Value : constant Boolean := Extra_Scopes_Allowed;

                  begin
                     --  Disable the support for generating extra scopes in
                     --  this construct since they cause errors.

                     Extra_Scopes_Allowed := False;

                     --  Emit a warning about the nonportable construct, so
                     --  that users will not be surprised to get an error on
                     --  various non-GCC compilers.

                     Error_Msg_N
                       ("??requires non-portable C construct: " &
                        "braced-groups within expressions", Node);

                     Write_Str ("({");
                     Cprint_Indented_List (Actions (Node));

                     if Last_Char /= ';' then
                        Write_Char (';');
                     end if;

                     Indent_Begin;
                     Write_Indent;
                     Cprint_Node (Expression (Node));
                     Write_Str ("; })");
                     Indent_End;

                     --  Restore the support for generating extra scopes

                     Extra_Scopes_Allowed := ESA_Value;
                  end;
               end if;
            else
               Cprint_Node (Expression (Node));
            end if;

         when N_Expression_Function =>
            Write_Indent;
            Cprint_Node (Specification (Node), Declaration => True);
            Write_Char (' ');
            Open_Scope;
            Indent_Begin;
            Write_Indent;
            Write_Str ("return ");
            Cprint_Node (Expression (Node));
            Write_Char (';');
            Indent_End;
            Close_Scope;

         when N_Extended_Return_Statement =>
            raise Program_Error;

         when N_Delta_Aggregate =>
            raise Program_Error;

         when N_Extension_Aggregate =>
            raise Program_Error;

         when N_Floating_Point_Definition
            | N_Formal_Decimal_Fixed_Point_Definition
            | N_Formal_Derived_Type_Definition
            | N_Formal_Abstract_Subprogram_Declaration
            | N_Formal_Concrete_Subprogram_Declaration
            | N_Formal_Discrete_Type_Definition
            | N_Formal_Floating_Point_Definition
            | N_Formal_Modular_Type_Definition
            | N_Formal_Object_Declaration
            | N_Formal_Ordinary_Fixed_Point_Definition
            | N_Formal_Package_Declaration
            | N_Formal_Private_Type_Definition
            | N_Formal_Incomplete_Type_Definition
            | N_Formal_Signed_Integer_Type_Definition
            | N_Formal_Type_Declaration
         =>
            null; -- not output in C code

         when N_Free_Statement =>
            Write_Source_Lines (Node);
            Write_Indent_Str ("free(");
            Cprint_Node (Expression (Node), Declaration => True);
            Write_Str (");");

         when N_Freeze_Entity =>
            Freeze_Level := Freeze_Level + 1;
            Cprint_Node_List (Actions (Node));
            Freeze_Level := Freeze_Level - 1;

         when N_Freeze_Generic_Entity =>
            null; -- not output in C code

         when N_Full_Type_Declaration =>
            if not In_Declarations then
               Open_Extra_Scope;
            end if;

            Write_Source_Lines (Node);
            Write_Itypes_In_Subtree (Node);

            declare
               procedure Check_Components
                 (Clist            : Node_Id;
                  Allow_Last_Field : Boolean);
               --  Check validity of components in Clist. Emit an error if a
               --  type whose size depends on a discriminant is found, unless
               --  Allow_Last_Field is True and this is the type of the last
               --  field in a record.

               ----------------------
               -- Check_Components --
               ----------------------

               procedure Check_Components
                 (Clist            : Node_Id;
                  Allow_Last_Field : Boolean)
               is
                  Comp  : Node_Id;
                  Comp2 : Node_Id;
                  Disc  : Node_Id;
                  Discs : List_Id;

               begin
                  --  No action needed if no components are available

                  if No (Clist) then
                     return;
                  end if;

                  Comp := First (Component_Items (Clist));
                  Comp_Loop : while Present (Comp) loop
                     if Nkind (Comp) = N_Component_Declaration then

                        --  Check type of component

                        if Size_Depends_On_Discriminant
                             (Get_Full_View
                               (Etype (Defining_Identifier (Comp))))
                        then
                           if Allow_Last_Field then
                              Discs := Discriminant_Specifications (Node);

                              if Present (Discs) then
                                 Disc := First (Discs);
                                 while Present (Disc) loop
                                    if Present (Expression (Disc)) then
                                       Error_Msg_N
                                         ("unsupported type: discriminant " &
                                          "with default value", Disc);
                                       exit;
                                    end if;

                                    Next (Disc);
                                 end loop;
                              end if;

                              Comp2 := Comp;

                              loop
                                 Next (Comp2);

                                 exit when No (Comp2);

                                 if Nkind (Comp2) = N_Component_Declaration
                                 then
                                    Error_Msg_N
                                      ("unsupported type: only the last field "
                                       & "may depend on a discriminant", Comp);
                                    exit Comp_Loop;
                                 end if;
                              end loop;
                           else
                              Error_Msg_N
                                ("unsupported type: field in variant part " &
                                 "cannot depend on a discriminant", Comp);
                           end if;

                           exit Comp_Loop;
                        end if;
                     end if;

                     Next (Comp);
                  end loop Comp_Loop;

                  if Present (Variant_Part (Clist)) then
                     Comp := First (Variants (Variant_Part (Clist)));

                     while Present (Comp) loop
                        Check_Components
                          (Component_List (Comp), Allow_Last_Field => False);
                        Next (Comp);
                     end loop;
                  end if;
               end Check_Components;

               --  Local variables

               Typ : constant Entity_Id := Defining_Identifier (Node);

            begin
               --  If this is a first subtype, and base type is not the same as
               --  the first subtype, output a typedef for that as well.

               if Is_First_Subtype (Typ) and then Base_Type (Typ) /= Typ then
                  Cprint_Declare (Base_Type (Typ));
               end if;

               if Has_Discriminants (Typ) then
                  declare
                     Type_Def : Node_Id := Type_Definition (Node);

                  begin
                     if Nkind (Type_Def) = N_Derived_Type_Definition then
                        Type_Def := Record_Extension_Part (Type_Def);
                     end if;

                     --  Check that this is a supported type, for now:
                     --  only the last field may depend on a discriminant (with
                     --  no default value), so that we can map this type to a C
                     --  type:
                     --     typedef struct _<name> {
                     --       field1;
                     --       ...
                     --       <type> last_field[1];
                     --     } <name>;

                     Check_Components
                       (Component_List (Type_Def), Allow_Last_Field => True);
                  end;
               end if;

               --  Now the typedef for the type itself

               Cprint_Declare (Typ);

               if Is_Packed_Array (Typ) then
                  Cprint_Declare (Packed_Array_Impl_Type (Typ));
               end if;
            end;

         when N_Function_Call =>
            Cprint_Call (Node);

         when N_Function_Instantiation =>
            null; -- not output in C code

         when N_Function_Specification =>
            declare
               Designator : constant Entity_Id :=
                              Unique_Defining_Entity (Node);
               Typ        : constant Entity_Id := Etype (Designator);

            begin
               Append_Subprogram_Prefix (Node);
               Declare_Subprogram_Types (Node);

               if not Is_Public (Designator) then
                  Write_Str_Col_Check ("static ");

                  if Is_Inlined (Designator)
                    or else Has_Pragma_Inline (Designator)
                  then
                     Write_Str_Col_Check ("GNAT_INLINE ");
                  end if;

               elsif Declaration then
                  Write_Str_Col_Check ("extern ");
               end if;

               if Is_Unconstrained_Array_Type (Typ) then
                  Error_Msg_N
                    ("function returning unconstrained arrays not "
                     & "supported!!??", Result_Definition (Node));
                  Write_Fatptr_Name (Typ);
                  Write_Char (' ');

               else
                  Check_Definition (Typ,
                    Error_Node => Result_Definition (Node));
                  Cprint_Type_Name (Typ);
                  Write_Char (' ');
               end if;
            end;

            Check_Attributes (Defining_Entity (Node));
            Cprint_Node (Defining_Unit_Name (Node), Declaration => True);
            Write_Param_Specs (Node);

            --  Remember that this entity is defined

            Register_Entity (Defining_Entity (Node));

         when N_Generic_Association
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration
            | N_Generic_Subprogram_Declaration
         =>
            if Nkind (Parent (Node)) = N_Compilation_Unit then
               Set_Has_No_Elaboration_Code (Parent (Node), True);
            end if;

         when N_Goto_Statement =>
            Write_Source_Lines (Node);
            Write_Indent_Str ("goto ");
            Cprint_Node (Name (Node), Declaration => True);
            Write_Char (';');

            if Nkind (Next (Node)) = N_Label then
               Write_Indent;
            end if;

         when N_Goto_When_Statement =>
            Write_Source_Lines (Node);
            Write_Indent_Str ("goto ");
            Cprint_Node (Name (Node), Declaration => True);
            Write_Str (" when ");
            Cprint_Node (Condition (Node));

            Write_Char (';');

            if Nkind (Next (Node)) = N_Label then
               Write_Indent;
            end if;

         when N_Handled_Sequence_Of_Statements =>
            declare
               Saved_Value : constant Boolean := In_Package_Body_Init;

            begin
               In_Package_Body_Init :=
                 Nkind (Parent (Node)) = N_Package_Body;

               Cprint_Indented_List (Statements (Node));

               if not Is_Empty_List (Exception_Handlers (Node)) then
                  Error_Msg_N
                    ("??exception handlers are ignored",
                     First (Exception_Handlers (Node)));
               end if;

               if Present (At_End_Proc (Node)) then
                  Error_Msg_N
                    ("clean up procedures not supported yet",
                     At_End_Proc (Node));
               end if;

               In_Package_Body_Init := Saved_Value;
            end;

         when N_Identifier =>

            --  If reference to parameter passed by pointer, add deference

            if Is_Formal (Entity (Node))
              and then Pass_Pointer (Entity (Node))
            then
               Write_Str ("(*");
               Write_Id (Node);
               Write_Char (')');

            --  Replace constant identifier by its expression when relevant

            elsif not Declaration
              and then Nkind (Node) in N_Subexpr
              and then Nkind (Parent (Node)) /= N_Attribute_Reference
            then
               if Nkind (Node) in N_Identifier
                                | N_Type_Conversion
                                | N_Unchecked_Type_Conversion
                 and then not Is_Constant_Folded (Entity (Node))
               then
                  Check_Definition (Node);
               end if;

               Cprint_Node (Entity (Node));

            else
               if not Is_Constant_Folded (Entity (Node)) then
                  Check_Definition (Node);
               end if;

               Write_Id (Node);
            end if;

         when N_If_Expression =>
            declare
               Condition  : constant Node_Id := First (Expressions (Node));
               Then_Expr  : constant Node_Id := Next (Condition);
               Else_Expr  : constant Node_Id := Next (Then_Expr);

            begin
               Write_Char ('(');
               Cprint_Node (Condition);
               Write_Str (") ? ");
               Cprint_Node_Paren (Then_Expr);
               Write_Str (" : ");
               Cprint_Node_Paren (Else_Expr);
            end;

         when N_If_Statement =>
            Write_Source_Lines (Sloc (Node), Last_Line (Condition (Node)));

            if In_Compound_Statement then
               Write_Char ('(');
               Cprint_Node (Condition (Node));
               Write_Str (") ? (");
               Cprint_Comma_List (Then_Statements (Node));
               Write_Str (") : ");

               if Present (Elsif_Parts (Node)) then
                  declare
                     Elsif_Part : Node_Id := First (Elsif_Parts (Node));
                  begin
                     loop
                        Write_Char ('(');
                        Cprint_Node (Condition (Elsif_Part));
                        Write_Str (") ? (");
                        Cprint_Comma_List (Then_Statements (Elsif_Part));
                        Write_Str (") : ");

                        Next (Elsif_Part);
                        exit when No (Elsif_Part);
                     end loop;
                  end;
               end if;

               if Present (Else_Statements (Node)) then
                  Write_Char ('(');
                  Cprint_Comma_List (Else_Statements (Node));
                  Write_Char (')');
               else
                  --  Complete by a dummy value since if-expressions in C
                  --  require an else part.

                  Write_Char ('0');
               end if;
            else
               Write_Indent_Str ("if (");
               Cprint_Node (Condition (Node));
               Write_Str_Col_Check (")");

               Write_Char (' ');
               Open_Scope;
               Cprint_Indented_List (Then_Statements (Node));

               if No (Elsif_Parts (Node))
                 and then No (Else_Statements (Node))
               then
                  Write_Source_Lines
                    (Sloc (Node) + Text_Ptr (UI_To_Int (End_Span (Node))));
               end if;

               Write_Indent;
               Close_Scope;

               Cprint_Opt_Node_List (Elsif_Parts (Node));

               if Present (Else_Statements (Node)) then

                  --  Guess where ELSE keyword is

                  declare
                     FES : constant Physical_Line_Number :=
                             First_Line (First (Else_Statements (Node)));
                  begin
                     if FES /= No_Physical_Line_Number then
                        Write_Source_Lines (FES - 1, FES - 1);
                     end if;
                  end;

                  Write_Indent_Str ("else ");
                  Open_Scope;
                  Cprint_Indented_List (Else_Statements (Node));

                  Write_Source_Lines
                    (Sloc (Node) + Text_Ptr (UI_To_Int (End_Span (Node))));
                  Write_Indent;
                  Close_Scope;
               end if;
            end if;

         when N_Implicit_Label_Declaration =>
            null; -- not output in C code

         when N_In =>
            if Present (Right_Opnd (Node)) then
               declare
                  Rng : Node_Id := Right_Opnd (Node);
               begin
                  if Nkind (Rng) = N_Identifier then
                     Rng := Scalar_Range (Etype (Rng));
                  end if;

                  Cprint_Left_Opnd (Node);
                  Write_Str (" >= ");
                  Cprint_Node (Low_Bound (Rng));
                  Write_Str (" && ");
                  Cprint_Left_Opnd (Node);
                  Write_Str (" <= ");
                  Cprint_Node (High_Bound (Rng));
               end;
            else
               Cprint_Bar_List (Alternatives (Node));
            end if;

         when N_Incomplete_Type_Declaration
            | N_Index_Or_Discriminant_Constraint
         =>
            null; -- not output in C code

         when N_Indexed_Component =>
            declare
               Pref : constant Node_Id := Unqual_Conv (Prefix (Node));

            begin
               --  For unidimensional arrays we directly use the pointer to the
               --  array components.

               if Is_Unidimensional_Array_Type (Etype (Pref)) then
                  if Is_Unconstrained_Array_Formal (Pref) then
                     Write_Unconstrained_Array_Prefix (Pref);

                  --  Generate the standard C array index (i.e. arr[n])

                  elsif Is_Array_Formal (Pref) then
                     Write_Char ('(');
                     Cprint_Node (Pref);
                     Write_Char (')');

                  elsif Nkind (Pref) = N_Explicit_Dereference then
                     if Is_Unconstrained_Array_Type (Etype (Pref)) then
                        Write_Unconstrained_Array_Prefix (Pref);
                     else
                        Write_Str ("(*");
                        Cprint_Node_Paren (Prefix (Pref));
                        Write_Char (')');
                     end if;

                  else
                     Cprint_Node_Paren (Pref);
                  end if;

               --  For multidimensional arrays we generate code that relies on
               --  the itype. This is not supported under ISO C90.

               else
                  if Is_Unconstrained_Array_Formal (Pref) then
                     Write_Unconstrained_Array_Prefix (Pref);

                  elsif Is_Array_Formal (Pref) then
                     Cprint_Node (Pref);

                  elsif Nkind (Pref) = N_Explicit_Dereference then
                     if Is_Unconstrained_Array_Type (Etype (Pref)) then
                        Write_Fatptr_Indexed_Component (Node);

                        --  No further code needed here since the previous call
                        --  generates code which displaces the pointer to
                        --  reference the indexed component.

                        goto Leave;
                     else
                        Write_Str ("(*");
                        Cprint_Node_Paren (Prefix (Pref));
                        Write_Char (')');
                     end if;
                  else
                     Cprint_Node_Paren (Pref);
                  end if;
               end if;
            end;

            declare
               Pref : constant Node_Id := Prefix (Node);
               Typ  : constant Node_Id := Get_Full_View (Etype (Pref));
               Idx  : Nat;
               Ind  : Node_Id;
               Sub  : Node_Id;

            begin
               Sub := First (Expressions (Node));
               Ind := First_Index (Typ);
               Idx := 1;

               loop
                  Write_Char ('[');

                  if not Is_Constrained (Typ) then
                     Cprint_Node (Sub);
                     Write_Str_Col_Check (" - ");

                     --  Reference '.first' in the fat pointer

                     if Is_AREC_Reference (Node) then
                        Cprint_Node (Prefix (Node));
                     else
                        Cprint_Node (Pref);
                     end if;

                     Write_Str (".");
                     Write_Fatptr_First (Typ, Idx);

                  elsif Ekind (Typ) = E_String_Literal_Subtype then
                     Cprint_Difference
                       (Sub, String_Literal_Low_Bound (Typ),
                        Minus_One_Min => False);

                  else
                     Cprint_Difference
                       (Sub, Type_Low_Bound (Etype (Ind)),
                        Minus_One_Min => False);
                  end if;

                  Write_Char (']');
                  Next (Sub);
                  exit when No (Sub);
                  Next_Index (Ind);
                  Idx := Idx + 1;
               end loop;
            end;

            <<Leave>>
            null;

         when N_Integer_Literal =>

            --  Note: do not bother with writing in hex in C output for now

            --  Protect the backend against not fully decorated nodes; for
            --  example, the alignment value may have no Etype.

            if Present (Etype (Node)) then
               Write_Uint
                 (U       => Intval (Node),
                  Modular => Is_Modular_Integer_Type (Etype (Node)));
            else
               Write_Uint (Intval (Node));
            end if;

         when N_Iterated_Component_Association =>
            raise Program_Error;

         when N_Iterated_Element_Association =>
            raise Program_Error;

         when N_Iteration_Scheme =>
            raise Program_Error; -- handled as part of loop handling

         when N_Iterator_Specification =>
            Error_Msg_N ("unsupported kind of iterator", Node);

         when N_Itype_Reference =>
            Cprint_Declare (Itype (Node));

         when N_Label =>
            Write_Source_Lines (Node);
            Write_Indent;
            Write_Id (Identifier (Node));
            Write_Str (": ;");

         when N_Loop_Parameter_Specification =>
            raise Program_Error; -- handled by N_Loop_Statement

         when N_Loop_Statement =>
            declare
               ISS : constant Node_Id := Iteration_Scheme (Node);

               For_Loop_Var : Entity_Id := Empty;
               --  Set to defining identifier of for loop variable for FOR loop

               LBD : Node_Id;
               HBD : Node_Id;

               For_Loop_Reverse : Boolean;
               --  Set True if reverse for loop, False for normal for loop

               Incr : String (1 .. 2) := "++";
               --  Change to "--" if reverse FOR loop

               Use_While : Boolean := False;
               --  Set True if we have the case of a FOR loop that had to be
               --  expanded into a C while loop, and thus needs a statement
               --  adding at the end of the body that increments/decrements
               --  the loop variable.

            begin
               --  Handle iteration scheme

               if Present (ISS) then
                  Write_Source_Lines (Sloc (Node), Last_Line (ISS));

                  --  WHILE loop case, generates C while

                  if Present (Condition (ISS)) then
                     Write_Indent_Str ("while (");
                     Cprint_Node (Condition (ISS));
                     Write_Char (')');

                  --  FOR loop case

                  else
                     --  For loops are tricky, consider this example:

                     --     for X in Integer range 1 .. N loop

                     --  Suppose we decide to translate this to C as

                     --     {
                     --       int x;
                     --       for (x = 1; x <= N; x++) {
                     --          loop body
                     --       }
                     --     }

                     --  That seems right, but it does not work in the case
                     --  where N = Integer'Last, since we will increment
                     --  this value before the test, causing overflow. In the
                     --  case where we have that possibility, the required
                     --  translation is:

                     --    {
                     --      int x = 1;
                     --      if (x <= N) {
                     --        while (true) {
                     --          loop body
                     --          if (x != N) x++; else break;
                     --        }
                     --      }
                     --    }

                     --  For performance reasons, we try to use 'for' loops
                     --  where possible.

                     declare
                        LPS : constant Node_Id :=
                                Loop_Parameter_Specification (ISS);
                        DSD : constant Node_Id :=
                                Discrete_Subtype_Definition (LPS);
                        Rng : Node_Id;

                        Comp : String (1 .. 4) := " <= ";
                        --  Change to " >= " if reverse loop

                        Loop_Btype : Entity_Id;
                        --  Base type of type of loop variable

                        OK : Boolean;
                        Lo : Uint;
                        Hi : Uint;
                        --  Parameters for Determine_Range call

                     begin
                        For_Loop_Var := Defining_Identifier (LPS);
                        For_Loop_Reverse := Reverse_Present (LPS);
                        Loop_Btype := Base_Type (Etype (For_Loop_Var));

                        case Nkind (DSD) is
                           when N_Range =>
                              Rng := DSD;
                           when N_Subtype_Indication =>
                              Rng := Range_Expression (Constraint (DSD));
                           when others =>
                              raise Program_Error;
                        end case;

                        LBD := Low_Bound (Rng);
                        HBD := High_Bound (Rng);

                        --  Set things up for reverse loop case

                        if For_Loop_Reverse then
                           Incr := "--";
                           Comp := " >= ";

                           declare
                              Temp : constant Node_Id := LBD;
                           begin
                              LBD := HBD;
                              HBD := Temp;
                           end;
                        end if;

                        --  Now see whether we need a do-while loop

                        Determine_Range
                          (HBD, OK, Lo, Hi, Assume_Valid => True);

                        if For_Loop_Reverse then
                           Use_While :=
                             Lo <= Expr_Value (Type_Low_Bound (Loop_Btype));
                        else
                           Use_While :=
                             Hi >= Expr_Value (Type_High_Bound (Loop_Btype));
                        end if;

                        --  Create outer block defining the for variable and
                        --  itypes.

                        Write_Indent;
                        Open_Scope;
                        Indent_Begin;
                        Write_Indent;

                        --  Generate itype for loop variable, then declare the
                        --  loop variable, then generate remaining itypes if
                        --  any, since some itypes may reference the loop
                        --  variable.

                        Write_Itypes_In_Subtree (Etype (For_Loop_Var));
                        Write_Indent;
                        Check_Definition (Etype (For_Loop_Var),
                          Error_Node => For_Loop_Var);
                        Cprint_Type_Name (Etype (For_Loop_Var));
                        Write_Char (' ');
                        Cprint_Node (For_Loop_Var, Declaration => True);

                        if Use_While then
                           Write_Str (" = ");
                           Cprint_Node (LBD);
                        end if;

                        Write_Char (';');
                        Write_Indent;

                        Set_In_Statements;

                        --  Case of using while loop

                        if Use_While then
                           --  Write while header

                           Write_Indent_Str ("if (");
                           Cprint_Node (For_Loop_Var, Declaration => True);
                           Write_Str (Comp);
                           Cprint_Node (HBD);
                           Write_Str (") ");
                           Open_Scope;
                           Indent_Begin;
                           Write_Indent_Str ("while (true)");

                        --  Case where we can use for loop safely

                        else
                           Write_Indent_Str ("for (");
                           Cprint_Node (For_Loop_Var, Declaration => True);
                           Write_Str (" = ");
                           Cprint_Node (LBD);
                           Write_Str ("; ");
                           Cprint_Node (For_Loop_Var, Declaration => True);
                           Write_Str (Comp);
                           Cprint_Node (HBD);
                           Write_Str ("; ");
                           Cprint_Node (For_Loop_Var, Declaration => True);
                           Write_Str (Incr);
                           Write_Char (')');
                        end if;
                     end;
                  end if;

               --  No iteration scheme present

               else
                  Write_Source_Lines (Sloc (Node));
                  Write_Indent_Str ("while (true)");
               end if;

               --  Output the loop body

               Write_Char (' ');
               Open_Scope;
               Indent_Begin;
               Cprint_Node_List (Statements (Node));

               --  End of while loop if needed

               if Use_While then
                  Write_Indent_Str ("if (");
                  Cprint_Node (For_Loop_Var, Declaration => True);
                  Write_Str (" != ");
                  Cprint_Node (HBD);
                  Write_Str (") ");
                  Cprint_Node (For_Loop_Var, Declaration => True);
                  Write_Str (Incr);
                  Write_Str ("; else break;");
               end if;

               --  Deal with loop closure

               Write_Source_Lines (End_Label (Node));

               Indent_End;
               Write_Indent;
               Close_Scope;

               --  Close the outer block if FOR case

               if Present (For_Loop_Var) then
                  Indent_End;
                  Write_Indent;
                  Close_Scope;
               end if;

               if Use_While then
                  Indent_End;
                  Write_Indent;
                  Close_Scope;
               end if;

               --  Output label at end of loop as possible exit target

               if Present (Identifier (Node))
                 and then (Is_Back_End_Label (Entity (Identifier (Node)))
                            or else Nkind (Original_Node (Node)) /=
                              N_Loop_Statement
                            or else not
                              Has_Created_Identifier (Original_Node (Node)))
               then
                  Write_Source_Lines (End_Label (Node));
                  Write_Indent;
                  Write_Id (Identifier (Node));
                  Write_Str (": ;");
               end if;
            end;

         when N_Mod_Clause =>
            raise Program_Error;

         when N_Modular_Type_Definition =>
            raise Program_Error;

         when N_Not_In =>
            if Present (Right_Opnd (Node)) then
               Cprint_Left_Opnd (Node);
               Write_Str ("<");
               Cprint_Node (Low_Bound (Right_Opnd (Node)));
               Write_Str (" && ");
               Cprint_Left_Opnd (Node);
               Write_Str (">");
               Cprint_Node (High_Bound (Right_Opnd (Node)));
            else
               Write_Str ("!(");
               Cprint_Bar_List (Alternatives (Node));
               Write_Str (")");
            end if;

         when N_Null =>
            declare
               Typ : constant Entity_Id :=
                       Get_Full_View (Etype (Node));
            begin
               if Has_Fat_Pointer (Typ) then
                  Write_Fatptr_Init (Node, Typ);
               else
                  Write_Str_Col_Check ("NULL");
               end if;
            end;

         when N_Null_Statement =>
            Write_Source_Lines (Node);

            if Comes_From_Source (Node)
              or else not Is_List_Member (Node)
              or else (No (Prev (Node)) and then No (Next (Node)))
            then
               if Nkind (Parent (Node)) /= N_Freeze_Entity then
                  Write_Indent_Str ("{}");
               end if;
            end if;

         when N_Number_Declaration =>
            null; -- not output in C code

         when N_Object_Declaration =>

            --  When we are unrolling the declarations of the wrapper package
            --  of a subprogram instantiation we cannot output declarations of
            --  variables that have an initialization expression since they are
            --  used to map generic parameters into actuals and hence their
            --  initial value depends on the caller.

            if Present (Unrolling_Instance_Subp_Id)
              and then Present (Expression (Node))
            then
               goto Leave_Cprint_Node;
            end if;

            declare
               function Expr_Init_With_Assignment
                 (Node : Node_Id) return Boolean;
               --  Return True if the object declaration Node has an init
               --  expression which is initialized by the back end by means
               --  of a separate assignment statement. Used to avoid declaring
               --  the object as a constant.

               procedure Output_Specifiers
                 (Id        : Entity_Id;
                  Is_Extern : Boolean := False);
               --  Output the storage class specifier followed by the type
               --  qualifier specifier associated with Id.

               function Requires_Elaboration (Expr : Node_Id) return Boolean;
               --  Determines if the given expression requires elaboration
               --  code.

               -------------------------------
               -- Expr_Init_With_Assignment --
               -------------------------------

               function Expr_Init_With_Assignment
                 (Node : Node_Id) return Boolean
               is
                  Id     : constant Entity_Id := Defining_Identifier (Node);
                  U_Expr : constant Node_Id   :=
                             Unqual_Conv (Expression (Node));
                  U_Typ  : Entity_Id;

               begin
                  if No (U_Expr) then
                     return False;
                  end if;

                  U_Typ := Get_Full_View (Etype (U_Expr));

                  if Is_Access_Type (U_Typ) then
                     U_Typ := Get_Full_View (Designated_Type (U_Typ));
                  end if;

                  if Nkind (Original_Node (U_Expr)) = N_Allocator
                    and then Nkind (Expression (Original_Node (U_Expr))) =
                               N_Qualified_Expression
                  then
                     return True;

                  elsif Nkind (Original_Node (U_Expr)) = N_Allocator
                    and then Is_Unconstrained_Array_Type (U_Typ)
                  then
                     return True;

                  elsif Nkind (U_Expr) = N_Slice then
                     return True;

                  elsif Special_Elaboration_Code
                    and then Nkind (U_Expr) = N_Aggregate
                  then
                     return True;

                  else
                     if not Requires_Elaboration (U_Expr)
                       and then not Is_Raise_Statement (U_Expr)
                     then
                        if Is_Array_Type (U_Typ)
                          and then
                            (Nkind (U_Expr) = N_Identifier
                              or else
                                (Nkind (U_Expr) = N_Qualified_Expression
                                  and then Nkind (Expression (U_Expr)) =
                                             N_Identifier))
                        then
                           return True;

                        elsif Nkind (Expression (Node)) =
                                N_Unchecked_Type_Conversion
                          and then Is_Composite_Type
                                     (Get_Full_View (Etype (Id)))
                        then
                           return True;
                        end if;
                     end if;
                  end if;

                  return False;
               end Expr_Init_With_Assignment;

               -----------------------
               -- Output_Specifiers --
               -----------------------

               procedure Output_Specifiers
                 (Id        : Entity_Id;
                  Is_Extern : Boolean := False)
               is
                  Typ : constant Entity_Id := Get_Full_View (Etype (Id));
               begin
                  --  Generate packaged array integer type before specifiers

                  Output_Packed_Array_Type (Typ);

                  --  C storage specifiers are 'extern' or 'static'

                  if Is_Extern then
                     Write_Indent_Str ("extern ");

                  elsif Is_Statically_Allocated (Id)
                    and then Present (Current_Subp_Entity)
                  then
                     Write_Str ("static ");
                  end if;

                  --  C type-qualifiers are 'const', 'restrict', or 'volatile'

                  --  Handle 'const'

                  if Constant_Present (Node) then
                     declare
                        Expr : Node_Id;

                     begin
                        if Present (Full_View (Id)) then
                           Expr := Expression (Parent (Full_View (Id)));
                        else
                           Expr := Expression (Node);
                        end if;

                        if Present (Expr)
                          and then not Requires_Elaboration (Expr)
                        then
                           --  If the Id is referenced by a nested subprogram
                           --  it cannot be defined as constant since we need
                           --  to store its address in the activation record

                           if not No_Initialization (Node)
                             and then not Expr_Init_With_Assignment (Node)
                             and then not Is_Uplevel_Referenced_Entity (Id)
                           then
                              Write_Str ("const ");
                           end if;
                        end if;
                     end;
                  end if;

                  --  'volatile' is handled by Cprint_Object_Reference
               end Output_Specifiers;

               --------------------------
               -- Requires_Elaboration --
               --------------------------

               function Requires_Elaboration (Expr : Node_Id) return Boolean is
                  L : List_Id;
                  N : Node_Id;

               begin
                  if Library_Level and then Present (Expr) then
                     if Nkind (Expr) = N_Aggregate then
                        L := Expressions (Expr);

                        if Present (L) then
                           N := First (L);

                           while Present (N) loop
                              if Requires_Elaboration (N) then
                                 return True;
                              end if;

                              Next (N);
                           end loop;

                        elsif Present (Component_Associations (Expr)) then
                           L := Component_Associations (Expr);
                           N := First (L);

                           while Present (N) loop
                              if Requires_Elaboration (Expression (N)) then
                                 return True;
                              end if;

                              Next (N);
                           end loop;
                        end if;
                     else
                        return (Nkind (Expr) /= N_Qualified_Expression
                                 or else Nkind (Expression (Expr)) /=
                                           N_Aggregate)
                          and then not Compile_Time_Known_Value (Expr)
                          and then
                            (Nkind (Expr) /= N_Attribute_Reference
                              or else
                                not Is_Access_Attribute_Reference (Expr));
                     end if;
                  end if;

                  return False;
               end Requires_Elaboration;

               --  Local variables

               Id   : constant Entity_Id := Defining_Identifier (Node);
               Typ  : constant Entity_Id := Get_Full_View (Etype (Id));
               Expr : Node_Id;

               Can_Be_Skipped : constant Boolean :=
                 Comes_From_Source (Node)
                   and then not In_Library_Unit_Decl
                   and then Is_Constant_Folded (Id, In_Decl => True);

               Is_Unreferenced_Internal_Decl : constant Boolean :=
                 Back_End_Stage >= Removing_Decls
                   and then not Comes_From_Source (Id)
                   and then not Is_Library_Level_Entity (Id)
                   and then (Present (Current_Subp_Entity)
                               or else not (In_Header_File))
                   and then not Has_Object_References (Id);

               Is_Source_Side_Effects_Removal_Decl : constant Boolean :=
                 Is_Internal (Id)
                   and then Is_True_Constant (Id)
                   and then Comes_From_Source (Expression (Node));

               Is_Skipped_Decl : constant Boolean :=
                 Is_Unreferenced_Internal_Decl
                   and then not Is_Source_Side_Effects_Removal_Decl;

               Init_Expr_Required_Only : constant Boolean :=
                 Is_Unreferenced_Internal_Decl
                   and then Is_Source_Side_Effects_Removal_Decl;

               Closing_In_Ada_Body_Required : Boolean := False;

            begin
               Write_Itypes_In_Subtree (Node);

               if not In_Declarations
                 and then not Can_Be_Skipped
                 and then not Is_Skipped_Decl
               then
                  Open_Extra_Scope;
               end if;

               if Is_Skipped_Decl or else Can_Be_Skipped then
                  null;

               elsif Init_Expr_Required_Only then
                  Write_Str ("(void)(");
                  Cprint_Node (Expression (Node));
                  Write_Str (");");
                  Write_Indent;

               --  Nothing to do if this is a debug renaming type or the object
               --  has already been processed, or there is an address clause on
               --  the object (will be handled as part of
               --  N_Attribute_Definition_Clause)

               elsif Typ = Standard_Debug_Renaming_Type
                 or else (Entity_Table.Get (Id)
                           and then not Special_Elaboration_Code)
                 or else Present (Address_Clause (Id))
               then
                  null;

               --  Normal case

               else
                  Write_Source_Lines (Node);
                  Write_Indent;

                  --  Case of elab procedure, will generate code below

                  if Special_Elaboration_Code
                    and Node = Current_Elab_Entity
                  then
                     null;

                  elsif In_Main_Unit
                    and then not Library_Level
                    and then Constant_Present (Node)
                    and then Present (Full_View (Id))
                  then
                     goto Leave_Cprint_Node;

                  elsif In_Header_File
                    and then In_Main_Unit
                    and then No (Current_Subp_Entity)
                  then
                     --  Output the external declaration of this variable. For
                     --  supported variable size record types it is not needed
                     --  since we generate a macro definition.

                     if not Is_Supported_Variable_Size_Record (Typ) then
                        Output_Specifiers (Id, Is_Extern => True);

                        if Cprint_Object_Reference (Id) then
                           Write_Char (';');
                        end if;
                     end if;

                     --  Output the full declaration of this variable

                     Enter_In_Ada_Body;
                     Write_Indent;
                     Output_Specifiers (Id);
                     Cprint_Declare (Id, Semicolon => False);
                     Closing_In_Ada_Body_Required := True;
                  else
                     Check_Definition (Typ, Error_Node => Node);
                     Output_Specifiers (Id, Is_Extern => not In_Main_Unit);
                     Cprint_Declare (Id, Semicolon => False);
                  end if;

                  --  Add initializer if present

                  Expr := Expression (Node);

                  if In_Main_Unit
                    and then not No_Initialization (Node)
                    and then Present (Expr)
                  then
                     declare
                        Prev_Value : constant Boolean :=
                                       In_Object_Decl_Init_Expr;
                        Elab_Code  : constant Boolean :=
                          Special_Elaboration_Code
                            and Node = Current_Elab_Entity;

                     begin
                        In_Object_Decl_Init_Expr := True;

                        if not Requires_Elaboration (Expr) then
                           if Is_Raise_Statement (Expr) then
                              if not Elab_Code then
                                 Write_Char (';');
                              end if;

                              Set_In_Statements;

                              Write_Indent;
                              Cprint_Node (Expr);

                           elsif Is_Access_Type (Typ)
                             and then Has_Fat_Pointer (Typ)
                           then
                              --  Initialize null fat pointers by means of
                              --  aggregates.

                              if Nkind (Expr) = N_Null then
                                 Write_Str_Col_Check (" = ");
                                 Write_Fatptr_Init (Expr, Typ,
                                   Use_Aggregate => True);

                              --  The called function returns a fat pointer

                              elsif Nkind (Expr) = N_Function_Call then
                                 Write_Str_Col_Check (" = ");
                                 Cprint_Node (Expr);

                              else
                                 if Expr_Init_With_Assignment (Node) then
                                    if not Elab_Code then
                                       Write_Char (';');
                                    end if;

                                    Set_In_Statements;
                                    Write_Indent;
                                    Write_Id (Id);
                                 end if;

                                 Write_Str (" = ");

                                 if Nkind (Expr) = N_Attribute_Reference then
                                    Handle_Attribute (Expr);
                                 else
                                    Write_Fatptr_Init (Expr,
                                      Get_Full_View (Designated_Type (Typ)),
                                      Use_Aggregate => False);
                                 end if;
                              end if;

                           --  Variable size records are generally handled
                           --  separately from expressions initialized with
                           --  assignments because they must not be defined
                           --  constant. However in this case we generate the
                           --  same code for both cases.

                           elsif Is_Supported_Variable_Size_Record (Typ)
                             or else Expr_Init_With_Assignment (Node)
                           then
                              if not Elab_Code then
                                 Write_Char (';');
                              end if;

                              Set_In_Statements;
                              Write_Indent;
                              Cprint_Copy
                                (Target     => Id,
                                 Source     => Expr,
                                 Use_Memcpy => True);

                           else
                              if Elab_Code then
                                 if Ekind (Typ) in Composite_Kind then
                                    Set_In_Statements;
                                    Write_Indent;
                                    Cprint_Copy
                                      (Target     => Id,
                                       Source     => Expr,
                                       Use_Memcpy => True);

                                 else
                                    Write_Id (Id);
                                    Write_Str_Col_Check (" = ");
                                    Cprint_Node (Expr);
                                 end if;
                              else
                                 Write_Str_Col_Check (" = ");
                                 Cprint_Node (Expr);
                              end if;
                           end if;

                        --  A library-level declaration and not a compile-time
                        --  known value: defer initialization to elab proc.

                        else
                           Elaboration_Table.Append (Node);
                        end if;

                        In_Object_Decl_Init_Expr := Prev_Value;
                     end;
                  end if;

                  if Last_Char /= ASCII.NUL then
                     Write_Char (';');
                  end if;

                  Register_Entity (Id);

                  if In_Header_File
                    and then Closing_In_Ada_Body_Required
                  then
                     Exit_In_Ada_Body;
                     Write_Indent;
                  end if;

                  --  Ensure that we do not generate object declarations twice
                  --  in case of public/private views.

                  if not In_Main_Unit
                    and then Constant_Present (Node)
                    and then Present (Full_View (Id))
                  then
                     Register_Entity (Full_View (Id));
                  end if;
               end if;
            end;

         when N_Object_Renaming_Declaration =>
            Object_Renaming_Declaration : declare
               procedure Define_Renaming_Macro (Node : Node_Id);
               --  Defined the macro associated with the object renaming
               --  declaration Node.

               ---------------------------
               -- Define_Renaming_Macro --
               ---------------------------

               procedure Define_Renaming_Macro (Node : Node_Id) is
               begin
                  Write_Eol;
                  Write_Str ("#define ");
                  Write_Id (Defining_Identifier (Node));
                  Write_Str (" (");
                  Cprint_Node (Name (Node));
                  Write_Char (')');
                  Write_Eol;

                  --  Record this macro so that it will be #undef'ed at the end
                  --  of the current scope.

                  if not Library_Level then
                     Macro_Table.Append (Defining_Identifier (Node));
                  end if;
               end Define_Renaming_Macro;

               --  Local variables

               Def_Id : constant Node_Id := Defining_Identifier (Node);

            --  Start of processing for Object_Renaming_Declaration

            begin
               --  Most renamings are handled by the front end, handle
               --  remaining ones via preprocessor macros.

               if not Is_Renaming_Of_Object (Def_Id) then
                  if Nkind (Name (Node)) in N_Identifier | N_Expanded_Name then
                     Define_Renaming_Macro (Node);
                  else
                     Error_Msg_N ("unsupported kind of object renaming", Node);
                  end if;

               --  For internally generated renamings associated with iterators
               --  we need to generate the macro; in this case the front end
               --  does not perform the macro substitution done for entities
               --  that have set the attribute Is_Renaming_Of_Object (most
               --  probably to facilitate reporting errors/warnings on the
               --  iterator variable).

               elsif not Comes_From_Source (Node)
                 and then Present (Related_Expression (Def_Id))
                 and then Nkind (Parent (Related_Expression (Def_Id))) =
                            N_Iterator_Specification
               then
                  Define_Renaming_Macro (Node);
               end if;

               --  Remember that this entity is defined

               Register_Entity (Defining_Identifier (Node));
            end Object_Renaming_Declaration;

         when N_Op_Abs =>
            declare
               Typ : constant Entity_Id :=
                       Matching_Standard_Type (Etype (Node));
            begin
               if Typ = Standard_Short_Short_Integer
                 or else Typ = Standard_Short_Integer
                 or else Typ = Standard_Integer
                 or else Typ = Standard_Short_Short_Unsigned
                 or else Typ = Standard_Short_Unsigned
                 or else Typ = Standard_Unsigned
               then
                  Write_Str ("abs(");

               elsif Typ = Standard_Long_Integer
                 or else Typ = Standard_Long_Unsigned
               then
                  Write_Str ("labs(");

               elsif Typ = Standard_Long_Long_Integer
                 or else Typ = Standard_Long_Long_Unsigned
               then
                  Write_Str ("llabs(");

               elsif Typ = Standard_Short_Float
                 or else Typ = Standard_Float
               then
                  Write_Str ("fabsf(");

               elsif Typ = Standard_Long_Float then
                  Write_Str ("fabs(");

               elsif Typ = Standard_Long_Long_Float then
                  Write_Str ("fabsl(");

               else
                  raise Program_Error;
               end if;

               Cprint_Right_Opnd (Node);
               Write_Char (')');
            end;

         when N_Op_Add =>
            Write_Binary_Op (Node, " + ");

         when N_Op_And =>
            Write_Binary_Op (Node, " & ");

         when N_Op_Concat =>
            raise Program_Error; -- should always be expanded

         when N_Op_Divide =>
            if Rounded_Result (Node) then

               --  Note that we know the divisor is always positive (for fixed
               --  point), so we generate:
               --  ((left<0) ? (left - right/2)/right : (left + right/2)/right)

               Write_Str ("((");
               Cprint_Left_Opnd (Node);
               Write_Str (" < 0) ? (");

               Cprint_Left_Opnd (Node);
               Write_Str (" - ");
               Cprint_Right_Opnd (Node);
               Write_Str (" / 2) / ");
               Cprint_Right_Opnd (Node);

               Write_Str (" : (");

               Cprint_Left_Opnd (Node);
               Write_Str (" + ");
               Cprint_Right_Opnd (Node);
               Write_Str (" / 2) / ");
               Cprint_Right_Opnd (Node);
               Write_Char (')');

            else
               Cprint_Left_Opnd (Node);
               Write_Str (" / ");
               Cprint_Right_Opnd (Node);
            end if;

         when N_Op_Eq =>
            declare
               LHS   : constant Node_Id := Left_Opnd (Node);
               RHS   : constant Node_Id := Right_Opnd (Node);
               L_Typ : constant Node_Id := Get_Full_View (Etype (LHS));
               R_Typ : constant Node_Id := Get_Full_View (Etype (RHS));

            begin
               if Has_Fat_Pointer (L_Typ)
                 or else Has_Fat_Pointer (R_Typ)
               then
                  Write_Fatptr_Compare (LHS, RHS);

               elsif Ekind (L_Typ) in Composite_Kind then
                  if Is_Entity_Name (Unqual_Conv (LHS))
                    or else Is_Entity_Name (Unqual_Conv (RHS))
                    or else Nkind (LHS) in N_Explicit_Dereference
                                         | N_Indexed_Component
                                         | N_Selected_Component
                                         | N_Slice
                                         | N_String_Literal
                    or else Nkind (RHS) in N_Explicit_Dereference
                                         | N_Indexed_Component
                                         | N_Selected_Component
                                         | N_Slice
                                         | N_String_Literal
                  then
                     --  Replace composite equality by a call to memcmp(). Also
                     --  compare sizes in case of different types.

                     if L_Typ /= R_Typ then
                        Write_Char ('(');
                        Output_Sizeof (LHS);
                        Write_Str_Col_Check (" == ");
                        Output_Sizeof (RHS);
                        Write_Str_Col_Check (" && ");
                     end if;

                     Write_Str ("!memcmp(");

                     if Nkind (LHS) = N_Explicit_Dereference then
                        Cprint_Node (Prefix (LHS), Declaration => True);
                     else
                        if Requires_Address (L_Typ) then
                           Write_Char ('&');
                        end if;

                        Cprint_Node (LHS, Declaration => True);
                     end if;

                     Write_Str (", ");

                     if Nkind (RHS) = N_Explicit_Dereference then
                        Cprint_Node (Prefix (RHS), Declaration => True);
                     else
                        if Requires_Address (R_Typ) then
                           Write_Char ('&');
                        end if;

                        Cprint_Node (RHS, Declaration => True);
                     end if;

                     Write_Str (", ");
                     Output_Sizeof (LHS, RHS);
                     Write_Char (')');

                     if L_Typ /= R_Typ then
                        Write_Char (')');
                     end if;

                  else
                     declare
                        S : constant String := Node_Kind'Image (Nkind (LHS));
                     begin
                        Error_Msg_Strlen := S'Length;
                        Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                        Error_Msg_N ("unsupported comparison (~)", Node);
                     end;
                  end if;

               else
                  Array_Cast : declare
                     function Is_Access_Array_Formal
                       (N : Node_Id) return Boolean;
                     --  Return True if N is null or a formal whose type is
                     --  access to array type.

                     ----------------------------
                     -- Is_Access_Array_Formal --
                     ----------------------------

                     function Is_Access_Array_Formal
                       (N : Node_Id) return Boolean
                     is
                     begin
                        return Nkind (N) = N_Null
                          or else
                            (Nkind (N) = N_Identifier
                              and then Is_Formal (Entity (N))
                              and then Is_Access_Type
                                         (Get_Full_View (Etype (N)))
                              and then Is_Array_Type
                                         (Designated_Type
                                           (Get_Full_View (Etype (N)))));
                     end Is_Access_Array_Formal;

                     --  Local variables

                     Left  : constant Node_Id := Left_Opnd (Node);
                     Right : constant Node_Id := Right_Opnd (Node);

                  --  Start of processing for Array_Cast

                  begin
                     --  Casting needed for comparison of formals that are
                     --  access to array types (since in the C profile we
                     --  generated the designated type), unless the other
                     --  operand is also an access to array formal.

                     if Nkind (Left) /= N_Null
                       and then Is_Access_Array_Formal (Left)
                       and then not Is_Access_Array_Formal (Right)
                     then
                        Write_Char ('(');
                        Cprint_Type_Name (Get_Full_View (Etype (Left)));
                        Write_Str (") ");
                     end if;

                     Cprint_Left_Opnd (Node);

                     Write_Str (" == ");

                     --  Casting needed for comparison of formals that are
                     --  access to array types (since in the C profile we
                     --  generated the designated type), unless the other
                     --  operand was also an access to array formal.

                     if Nkind (Right) /= N_Null
                       and then Is_Access_Array_Formal (Right)
                       and then not Is_Access_Array_Formal (Left)
                     then
                        Write_Char ('(');
                        Cprint_Type_Name (Get_Full_View (Etype (Right)));
                        Write_Str (") ");
                     end if;

                     Cprint_Right_Opnd (Node);
                  end Array_Cast;
               end if;
            end;

         when N_Op_Expon =>

            --  Will probably never happen since expander uses a runtime call

            Write_Str ("pow(");
            Cprint_Left_Opnd (Node);
            Write_Char (',');
            Cprint_Right_Opnd (Node);
            Write_Char (')');

         when N_Op_Ge =>
            Cprint_Left_Opnd (Node);
            Write_Str (" >= ");
            Cprint_Right_Opnd (Node);

         when N_Op_Gt =>
            Cprint_Left_Opnd (Node);
            Write_Str (" > ");
            Cprint_Right_Opnd (Node);

         when N_Op_Le =>
            Cprint_Left_Opnd (Node);
            Write_Str (" <= ");
            Cprint_Right_Opnd (Node);

         when N_Op_Lt =>
            Cprint_Left_Opnd (Node);
            Write_Str (" < ");
            Cprint_Right_Opnd (Node);

         when N_Op_Minus =>
            Write_Str ("-");
            Cprint_Right_Opnd (Node);

         when N_Op_Mod =>
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Multiply =>
            Write_Binary_Op (Node, " * ");

         when N_Op_Ne =>
            declare
               LHS   : constant Node_Id := Left_Opnd (Node);
               L_Typ : constant Node_Id := Get_Full_View (Etype (LHS));
               RHS   : constant Node_Id := Right_Opnd (Node);
               R_Typ : constant Node_Id := Get_Full_View (Etype (RHS));

            begin
               if Has_Fat_Pointer (L_Typ) or else Has_Fat_Pointer (R_Typ) then
                  Write_Char ('!');
                  Write_Fatptr_Compare (LHS, RHS);

               elsif (Is_Entity_Name (LHS)
                       or else Nkind (LHS) = N_Selected_Component)
                 and then Ekind (L_Typ) in Composite_Kind
               then
                  --  Replace composite equality by a call to memcmp()

                  if L_Typ /= R_Typ then
                     Write_Char ('(');
                     Output_Sizeof (LHS);
                     Write_Str_Col_Check (" != ");
                     Output_Sizeof (RHS);
                     Write_Str_Col_Check (" || ");
                  end if;

                  Write_Str ("memcmp(");

                  if Requires_Address (L_Typ) then
                     Write_Char ('&');
                  end if;

                  Cprint_Node (LHS, Declaration => True);
                  Write_Str (", ");

                  if Requires_Address (R_Typ) then
                     Write_Char ('&');
                  end if;

                  Cprint_Node (RHS, Declaration => True);
                  Write_Str (", ");
                  Output_Sizeof (LHS, RHS);
                  Write_Char (')');

                  if L_Typ /= R_Typ then
                     Write_Char ('(');
                  end if;

               else
                  Cprint_Left_Opnd (Node);
                  Write_Str (" != ");
                  Cprint_Right_Opnd (Node);
               end if;
            end;

         when N_Op_Not =>
            if Is_Boolean_Type (Etype (Node)) then
               Write_Str ("!");
            elsif Is_Modular_Integer_Type (Etype (Node)) then
               if Non_Standard_Modular_Type (Etype (Node)) then
                  Write_Str ("(");
                  Cprint_Node (Etype (Node));
                  Write_Str (")(~((unsigned_32)");
                  Cprint_Right_Opnd (Node);

                  if Non_Binary_Modulus (Etype (Node)) then
                     Write_Str (") % ");
                     Write_Uint (Modulus (Etype (Node)));
                  else
                     Write_Str (") & ");
                     Write_Uint (Modulus (Etype (Node)) - Uint_1);
                  end if;

                  Write_Str (")");
               else
                  Write_Str ("~");
               end if;
            else
               Error_Msg_N ("unsupported NOT operator", Node);
               Write_Str ("/* unsupported NOT operator */ ~");
            end if;

            Cprint_Right_Opnd (Node);

         when N_Op_Or =>
            Write_Binary_Op (Node, " | ");

         when N_Op_Plus =>
            Write_Str ("+");
            Cprint_Right_Opnd (Node);

         when N_Op_Rem =>
            Cprint_Left_Opnd (Node);
            Write_Str (" % ");
            Cprint_Right_Opnd (Node);

         when N_Op_Rotate_Left
            | N_Op_Rotate_Right
         =>
            --  Should have been rewritten in Modify_Tree_For_C mode

            raise Program_Error;

         when N_Op_Shift_Right | N_Op_Shift_Right_Arithmetic =>
            Write_Binary_Op (Node, " >> ");

         when N_Op_Shift_Left =>
            Write_Binary_Op (Node, " << ");

         when N_Op_Subtract =>
            Write_Binary_Op (Node, " - ");

         when N_Op_Xor =>
            Write_Binary_Op (Node, " ^ ");

         when N_Operator_Symbol =>

            --  Replaced by the corresponding N_Op_XX node by the expander

            raise Program_Error;

         when N_Ordinary_Fixed_Point_Definition =>
            raise Program_Error; -- handled by N_Full_Type_Declaration

         when N_Or_Else =>
            Cprint_Left_Opnd (Node);
            Write_Str (" || ");
            Cprint_Right_Opnd (Node);

         when N_Others_Choice =>
            raise Program_Error;

         when N_Package_Body =>
            if Ekind (Corresponding_Spec (Node)) = E_Generic_Package then
               if Nkind (Parent (Node)) = N_Compilation_Unit then
                  Set_Has_No_Elaboration_Code (Parent (Node), True);
               end if;
            else
               Cprint_Node_List (Declarations (Node));
               Ensure_New_Line;

               declare
                  Stmts     : constant Node_Id :=
                                Handled_Statement_Sequence (Node);
                  Has_Stmts : constant Boolean :=
                                Present (Stmts)
                                  and then Has_Non_Null_Statements
                                             (Statements (Stmts));

                  Unit : Node_Id;

               begin
                  --  Only generate elaboration procedures when in main unit.

                  if not In_Main_Unit then
                     null;

                  --  For packages inside subprograms, generate elaboration
                  --  code as standard code as part of the enclosing unit.

                  elsif not Library_Level then
                     if Has_Stmts then
                        Open_Scope;
                        Set_In_Statements;
                        Indent_Begin;
                        Cprint_Node (Stmts);
                        Indent_End;
                        Ensure_New_Line;
                        Close_Scope;
                     end if;

                  elsif Nkind (Parent (Node)) /= N_Compilation_Unit then
                     if Has_Stmts then
                        Elaboration_Table.Append (Stmts);
                     end if;

                  elsif Elaboration_Table.Last = 0
                    and then not Has_Stmts
                  then
                     Set_Has_No_Elaboration_Code (Parent (Node), True);

                  else
                     Unit := Defining_Unit_Name (Node);

                     if Nkind (Unit) = N_Defining_Program_Unit_Name then
                        Unit := Defining_Identifier (Unit);
                     end if;

                     Write_Indent_Str ("extern void ");
                     Cprint_Node (Unit, Declaration => True);
                     Write_Str ("___elabb();");

                     Write_Indent_Str ("void ");
                     Cprint_Node (Unit, Declaration => True);
                     Write_Str ("___elabb() ");
                     Open_Scope;

                     Ensure_New_Line;
                     Indent_Begin;

                     declare
                        Save_Library_Level : constant Boolean := Library_Level;
                     begin
                        Library_Level := False;
                        Special_Elaboration_Code := True;

                        for J in 1 .. Elaboration_Table.Last loop
                           Current_Elab_Entity := Elaboration_Table.Table (J);
                           Check_Elaboration_Code_Allowed
                             (Current_Elab_Entity);
                           Cprint_Node (Current_Elab_Entity);
                        end loop;

                        Elaboration_Table.Set_Last (0);
                        Current_Elab_Entity := Empty;
                        Special_Elaboration_Code := False;

                        if Has_Stmts then
                           Check_Elaboration_Code_Allowed (Stmts);
                           Cprint_Node (Stmts);
                        end if;

                        Library_Level := Save_Library_Level;
                     end;

                     Indent_End;
                     Ensure_New_Line;
                     Write_Indent;
                     Close_Scope;
                  end if;
               end;
            end if;

         when N_Package_Declaration =>
            Write_Indent;
            Cprint_Node (Specification (Node), Declaration => True);

         when N_Package_Instantiation
            | N_Package_Renaming_Declaration
         =>
            if Nkind (Parent (Node)) = N_Compilation_Unit then
               Set_Has_No_Elaboration_Code (Parent (Node), True);
            end if;

         when N_Package_Specification =>
            Write_Source_Lines (Node);

            --  Open the new scope associated with this package specification
            --  to ensure that we are ready to start processing declarations
            --  (see Open_Scope). No explicit block is associated with this
            --  scope because:
            --    * for library level packages must not be generated
            --    * for nested packages the block is not needed

            Open_Scope (With_Block => False);

            declare
               Scope_Id : constant Nat := Current_Scope_Id;

            begin
               Cprint_Node_List (Visible_Declarations (Node));

               if Present (Private_Declarations (Node)) then
                  Cprint_Node_List (Private_Declarations (Node));
               end if;

               Set_In_Statements;

               --  We can safely close this package scope if it has no inner
               --  back-end scopes to close.

               if Current_Scope_Id = Scope_Id then
                  Close_Scope;

               --  For library level packages we can also close this scope and
               --  all its inner back-end scopes (if any)

               elsif Is_Library_Level_Entity (Defining_Entity (Node)) then
                  Close_Scope;

               --  For nested packages we must defer closing it (and its extra
               --  scopes) since its extra back-end scopes may have been added
               --  to handle declarations which can be referenced from its
               --  enclosing scope.

               else
                  null;
               end if;
            end;

            --  Only generate elaboration procedures for library-level packages
            --  and when part of the main unit. No action needed if we are just
            --  unrolling declarations of the wrapper package of a subprogram
            --  instantiation.

            if In_Main_Unit
              and then Nkind (Parent (Parent (Node))) = N_Compilation_Unit
              and then No (Unrolling_Instance_Subp_Id)
            then
               if Elaboration_Table.Last = 0 then
                  Set_Has_No_Elaboration_Code (Parent (Parent (Node)), True);
               else
                  declare
                     Unit : Node_Id := Defining_Unit_Name (Node);
                  begin
                     if Nkind (Unit) = N_Defining_Program_Unit_Name then
                        Unit := Defining_Identifier (Unit);
                     end if;

                     Write_Indent_Str ("extern void ");
                     Cprint_Node (Unit, Declaration => True);
                     Write_Str ("___elabs();");

                     Enter_In_Ada_Body;

                     Write_Indent_Str ("void ");
                     Cprint_Node (Unit, Declaration => True);
                     Write_Str ("___elabs() ");
                  end;

                  Open_Scope;
                  Ensure_New_Line;
                  Indent_Begin;

                  declare
                     Save_Library_Level : constant Boolean := Library_Level;
                  begin
                     Library_Level := False;
                     Special_Elaboration_Code := True;
                     Set_In_Statements;

                     for J in 1 .. Elaboration_Table.Last loop
                        Current_Elab_Entity := Elaboration_Table.Table (J);
                        Check_Elaboration_Code_Allowed (Current_Elab_Entity);
                        Cprint_Node (Elaboration_Table.Table (J));
                     end loop;

                     Current_Elab_Entity := Empty;
                     Special_Elaboration_Code := False;
                     Library_Level := Save_Library_Level;
                  end;

                  Elaboration_Table.Set_Last (0);
                  Indent_End;
                  Ensure_New_Line;
                  Write_Indent;
                  Close_Scope;

                  Exit_In_Ada_Body;
               end if;
            end if;

         when N_Parameter_Association =>
            raise Program_Error;

         when N_Parameter_Specification =>
            declare
               Ent    : constant Entity_Id := Defining_Identifier (Node);
               Typ    : constant Entity_Id := Get_Full_View (Etype (Ent));
               Ignore : Boolean;

            begin
               if (Is_Record_Type (Typ)
                    or else Is_Array_Type (Typ)
                    or else Is_Descendant_Of_Address (Typ))
                 and then Ekind (Ent) = E_In_Parameter
                 and then not Is_Uplevel_Referenced_Entity (Ent)
               then
                  Write_Str ("const ");
               end if;

               Ignore :=
                 Cprint_Object_Reference
                   (Ent, Add_Access => Pass_Pointer (Ent));
            end;

         when N_Pop_Constraint_Error_Label
            | N_Pop_Program_Error_Label
            | N_Pop_Storage_Error_Label
         =>
            null;

         when N_Private_Extension_Declaration
            | N_Private_Type_Declaration
         =>
            --  We cannot delay declaration in C in general, and since we
            --  do not care about privacy in the generated code, go ahead
            --  and generate the type here.

            declare
               Prev_State : constant Boolean := Unrolling_Full_Type_Decl;
            begin
               Unrolling_Full_Type_Decl := True;
               Cprint_Declare (Full_View (Defining_Identifier (Node)));
               Unrolling_Full_Type_Decl := Prev_State;
            end;

         when N_Push_Constraint_Error_Label
            | N_Push_Program_Error_Label
            | N_Push_Storage_Error_Label
         =>
            null;

         when N_Pragma =>

            --  We only output pragma Comment and we don't even do that if we
            --  are printing the full source, since there is no point.

            if Pragma_Name (Node) in Name_Annotate | Name_GNAT_Annotate
              and then List_Length (Pragma_Argument_Associations (Node)) = 3
            then
               declare
                  Arg1 : Node_Id :=
                    First (Pragma_Argument_Associations (Node));
                  Arg2 : Node_Id := Next (Arg1);
                  Arg3 : Node_Id := Next (Arg2);

               begin
                  Arg1 := Expression (Arg1);
                  Arg2 := Expression (Arg2);
                  Arg3 := Expression (Arg3);

                  if Nkind (Arg1) = N_Identifier
                    and then Nkind (Arg2) = N_Identifier
                    and then Get_Name_String (Chars (Arg1)) = "ccg"
                    and then Nkind (Arg3) = N_String_Literal
                  then
                     Get_Name_String (Chars (Arg2));

                     if Name_Buffer (1 .. Name_Len) = "c_pragma" then
                        if Column /= 1 then
                           Write_Eol;
                        end if;

                        Write_Str ("#pragma ");
                        String_To_Name_Buffer (Strval (Arg3));
                        Write_Str (Name_Buffer (1 .. Name_Len));
                        Write_Eol;

                     elsif Name_Buffer (1 .. Name_Len) = "verbatim" then
                        if Column /= 1 then
                           Write_Eol;
                        end if;

                        String_To_Name_Buffer (Strval (Arg3));
                        Write_Str (Name_Buffer (1 .. Name_Len));
                        Write_Eol;
                     end if;
                  end if;
               end;

            elsif Pragma_Name (Node) = Name_Comment
              and then Is_Non_Empty_List (Pragma_Argument_Associations (Node))
              and then not Dump_Source_Text
            then
               --  Blank line, unless another Comment pragma precedes

               if not Is_List_Member (Node)
                 or else No (Prev (Node))
                 or else Nkind (Prev (Node)) /= N_Pragma
                 or else Pragma_Name (Prev (Node)) /= Name_Comment
               then
                  Write_Eol;
               end if;

               Write_Indent_Str ("/* ");
               String_To_Name_Buffer
                 (Strval
                   (Expression (First (Pragma_Argument_Associations (Node)))));
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Str (" */");

               --  Blank line unless another Comment pragma follows

               if not Is_List_Member (Node)
                 or else No (Next (Node))
                 or else Nkind (Next (Node)) /= N_Pragma
                 or else Pragma_Name (Next (Node)) /= Name_Comment
               then
                  Write_Eol;
               end if;
            end if;

         when N_Pragma_Argument_Association =>
            raise Program_Error;

         when N_Procedure_Call_Statement =>
            Write_Source_Lines (Node);
            Write_Indent;
            Cprint_Call (Node);
            Write_Char (';');

         when N_Procedure_Instantiation =>
            null; -- not output in C code

         when N_Procedure_Specification =>
            declare
               Subp : constant Entity_Id := Unique_Defining_Entity (Node);

            begin
               Append_Subprogram_Prefix (Node);
               Write_Source_Lines (Node);
               Declare_Subprogram_Types (Node);

               if not Is_Public (Subp) then
                  Write_Str_Col_Check ("static ");

                  if Is_Inlined (Subp) or else Has_Pragma_Inline (Subp) then
                     Write_Str_Col_Check ("GNAT_INLINE ");
                  end if;

               elsif Declaration then
                  Write_Str_Col_Check ("extern ");
               end if;

               Write_Str_Col_Check ("void ");
               Check_Attributes (Defining_Entity (Node));
               Cprint_Node (Defining_Unit_Name (Node), Declaration => True);
               Write_Param_Specs (Node);

               --  Remember that this entity is defined

               Register_Entity (Defining_Entity (Node));
            end;

         when N_Protected_Body =>
            raise Program_Error;

         when N_Protected_Definition
            | N_Protected_Type_Declaration
         =>
            raise Program_Error; -- handled by the expander

         when N_Qualified_Expression =>

            --  At the C level, we can ignore the qualification

            Cprint_Node (Expression (Node));

         when N_Quantified_Expression =>
            raise Program_Error; -- handled by the expander

         when N_Raise_Expression =>
            Handle_Raise (Node);

         when N_Raise_xxx_Error
            | N_Raise_Statement
            | N_Raise_When_Statement
         =>
            Write_Source_Lines (Node);
            Handle_Raise (Node);

         when N_Range
            | N_Range_Constraint
         =>
            raise Program_Error;

         when N_Real_Literal =>
            if Ekind (Etype (Node)) in Fixed_Point_Kind then
               Write_Uint (Corresponding_Integer_Value (Node));
            else
               Write_Ureal_Col_Check (Node);
            end if;

         when N_Real_Range_Specification
            | N_Record_Definition
         =>
            raise Program_Error;

         when N_Record_Representation_Clause =>
            declare
               Typ : constant Entity_Id := Etype (Identifier (Node));

            begin
               --  Record representation clauses applied to derived types are
               --  not supported.

               if Etype (Typ) /= Typ then
                  Error_Msg_N
                    ("unsupported representation clause on derived type",
                     Node);
               end if;
            end;

         when N_Reference =>
            if Nkind (Prefix (Node)) = N_Function_Call then
               Error_Msg_N ("unsupported kind of function call", Node);
            end if;

            Write_Char ('&');
            Cprint_Node_Paren (Prefix (Node));

         when N_Requeue_Statement
            | N_SCIL_Dispatch_Table_Tag_Init
            | N_SCIL_Dispatching_Call
            | N_SCIL_Membership_Test
         =>
            raise Program_Error;

         when N_Return_When_Statement =>
            Write_Source_Lines (Node);
            Write_Indent_Str ("return;"); --  Stub it for now ???

         when N_Simple_Return_Statement =>
            Write_Source_Lines (Node);

            declare
               Expr : constant Node_Id := Expression (Node);
            begin
               if Present (Expr) then
                  if Nkind (Expr) = N_Allocator then
                     Open_Scope;
                     Indent_Begin;
                     Write_Indent;
                     Check_Definition (Etype (Expr), Error_Node => Expr);
                     Cprint_Type_Name (Etype (Expr));
                     Write_Str (" _tmp = ");
                     Cprint_Node (Expr);
                     Write_Str (";");
                     Write_Indent_Str ("return _tmp;");
                     Indent_End;
                     Close_Scope;

                  elsif Is_Array_Formal (Expr)
                    and then Present (Entity (Expr))
                    and then Is_Access_Type (Etype (Entity (Expr)))
                    and then Is_Constrained_Array_Type
                               (Get_Full_View
                                 (Designated_Type (Etype (Entity (Expr)))))
                  then
                     Write_Indent_Str ("return ((");
                     Write_Id (Etype (Expr));
                     Write_Char (')');
                     Cprint_Node (Expr);
                     Write_Str (");");
                  else
                     --  Replace "return [raise xxx]" by "[raise xxx]"

                     if Nkind (Expr) in N_Raise_xxx_Error then
                        Cprint_Node (Expr);
                     else
                        Write_Indent_Str ("return (");
                        Cprint_Node (Expr);
                        Write_Str (");");
                     end if;
                  end if;
               else
                  Write_Indent_Str ("return;");
               end if;
            end;

         when N_Selected_Component =>

            --  If reference to parameter passed by pointer, use -> notation

            if Is_Entity_Name (Prefix (Node))
              and then Present (Entity (Prefix (Node)))
              and then Is_Formal (Entity (Prefix (Node)))
              and then Pass_Pointer (Entity (Prefix (Node)))
            then
               --  For a->b, call Write_Id directly, we don't want Write_Node
               --  adding a star, this is a special case for handling params.

               Write_Id (Entity (Prefix (Node)));
               Write_Str ("->");

            --  Also use -> if prefix is explicit dereference

            elsif Nkind (Prefix (Node)) = N_Explicit_Dereference then
               declare
                  Pref      : constant Node_Id := Prefix (Prefix (Node));
                  Attr_Name : Name_Id;
                  Attr_Id   : Attribute_Id;

               begin
                  if Nkind (Pref) /= N_Attribute_Reference then
                     Cprint_Node_Paren (Pref);

                  else
                     Attr_Name := Attribute_Name (Pref);
                     Attr_Id   := Get_Attribute_Id (Attr_Name);

                     case Attr_Id is
                        when Attribute_Access
                           | Attribute_Address
                           | Attribute_Unchecked_Access
                           | Attribute_Unrestricted_Access
                        =>
                           --  Handle cases where we can directly reference the
                           --  entity of the prefix. For example, X'Access.all
                           --  where X is a formal passed by reference.

                           if Etype (Prefix (Pref)) = Etype (Prefix (Node))
                             and then Nkind (Prefix (Pref)) in N_Has_Entity
                             and then Present (Entity (Prefix (Pref)))
                             and then Is_Formal (Entity (Prefix (Pref)))
                             and then Pass_Pointer (Entity (Prefix (Pref)))
                           then
                              --  Casting needed for in-mode parameters to
                              --  avoid C warning because they were passed
                              --  as 'const'.

                              if Ekind (Entity (Prefix (Pref)))
                                = E_In_Parameter
                              then
                                 Write_Char ('(');
                                 Write_Char ('(');
                                 Write_Id (Etype (Entity (Prefix (Pref))));
                                 Write_Str ("*)");
                                 Cprint_Node (Entity (Prefix (Pref)));
                                 Write_Char (')');
                              else
                                 Cprint_Node (Entity (Prefix (Pref)));
                              end if;
                           else
                              Cprint_Node_Paren (Pref);
                           end if;

                        when others =>
                           Cprint_Node_Paren (Pref);
                     end case;
                  end if;

                  Write_Str ("->");
               end;

            --  Normal case of using a.b

            else
               Cprint_Node_Paren (Prefix (Node));
               Write_Char ('.');
            end if;

            --  For derived tagged types prepend the name of the selector with
            --  prefix "_parent." until we reach the parent containing the
            --  component declaration.

            if Is_Tagged_Type (Etype (Prefix (Node))) then
               declare
                  Selector   : constant Entity_Id :=
                                 Entity (Selector_Name (Node));
                  Parent_Typ : constant Entity_Id :=
                                 Get_Full_View
                                   (Scope
                                     (Original_Record_Component (Selector)));
                  Typ        : Entity_Id;

               begin
                  Typ := Get_Full_View (Etype (Prefix (Node)));

                  if Ekind (Typ) = E_Record_Subtype then
                     Typ := Get_Full_View (Etype (Typ));
                  end if;

                  if Is_Class_Wide_Type (Typ) then
                     Typ := Get_Full_View (Root_Type (Typ));
                  end if;

                  while Typ /= Parent_Typ loop
                     Write_Str ("_parent.");
                     Typ := Get_Full_View (Etype (Typ));
                  end loop;
               end;
            end if;

            --  Output the internal name given to structs and unions generated
            --  for record type variants.

            if Is_Declared_Within_Variant (Entity (Selector_Name (Node)))
              and then not Is_Simple_Unchecked_Union
                             (Get_Full_View (Etype (Prefix (Node))))
            then
               declare
                  Comp       : constant Entity_Id :=
                                          Entity (Selector_Name (Node));
                  Comp_Decl  : constant Node_Id := Parent (Comp);
                  Comp_List  : constant Node_Id := Parent (Comp_Decl);
                  Comp_Items : constant List_Id := Component_Items (Comp_List);
                  Var_Node   : constant Node_Id := Parent (Comp_List);

               begin
                  Write_Str (Anon_Union_Prefix);
                  Write_Char ('.');

                  --  When its component list has several components we must
                  --  also output the internal name given to the struct.

                  if List_Length (Comp_Items) > 1 then
                     Output_Anon_Struct_Name (Var_Node);
                     Write_Char ('.');
                  end if;
               end;
            end if;

            Cprint_Node (Selector_Name (Node), Declaration => True);

         when N_Selective_Accept
            | N_Signed_Integer_Type_Definition
            | N_Single_Protected_Declaration
            | N_Single_Task_Declaration
         =>
            raise Program_Error;

         when N_Slice =>
            declare
               Is_Access   : Boolean;
               Lbd         : Node_Id;
               Lo          : Node_Id;
               N           : Node_Id;
               Next        : Node_Id;
               Rng         : Node_Id;
               Same_Values : Boolean := False;
               Typ         : Entity_Id;

            begin
               N := Node;

               --  Handle slices of slices by using the final (relevant) slice

               if Nkind (Prefix (Node)) = N_Slice then
                  loop
                     Next := Prefix (N);
                     exit when Nkind (Next) /= N_Slice;
                     N := Next;
                  end loop;
               end if;

               Typ := Get_Full_View (Etype (Prefix (N)));
               Is_Access := Is_Access_Type (Typ);

               if Is_Access then
                  Typ := Get_Full_View (Directly_Designated_Type (Typ));
               end if;

               if Ekind (Typ) = E_String_Literal_Subtype then
                  Lbd := String_Literal_Low_Bound (Typ);
               else
                  Lbd := Type_Low_Bound (Etype (First_Index (Typ)));
               end if;

               Rng := Discrete_Range (Node);

               --  We generate &arr[slice-low-bound - index-low-bound]

               if Nkind (Rng) = N_Range then
                  Lo := Low_Bound (Rng);
               else
                  Lo := Type_Low_Bound (Etype (Rng));
               end if;

               --  Omit & if prefix is an access type (for e.g. a function call
               --  that returns a pointer to an array).

               if Is_Access then
                  Cprint_Node_Paren (Prefix (N));

               elsif Is_Unconstrained_Array_Type (Typ) then
                  Write_Char ('&');
                  Write_Unconstrained_Array_Prefix (Prefix (N));
                  Write_Char ('[');
                  Cprint_Node (Lo);
                  Write_Str (" - ");
                  Cprint_Node (Prefix (N));
                  Write_Char ('.');
                  Write_Fatptr_First (Typ, 1);
                  Write_Char (']');
                  Same_Values := True;

               --  Generate simply arr instead of &arr[0]

               elsif Has_Same_Int_Value (Lo, Lbd) then
                  Cprint_Node_Paren (Prefix (N));
                  Same_Values := True;

               --  Normal case of an array, where we need the &

               else
                  Write_Char ('&');
                  Cprint_Node_Paren (Prefix (N));
               end if;

               if not Same_Values then
                  Write_Char ('[');
                  Cprint_Difference (Lo, Lbd, Minus_One_Min => False);
                  Write_Char (']');
               end if;
            end;

         when N_String_Literal =>
            declare
               Str : constant String_Id := Strval (Node);
            begin
               --  This test for line overflow is not quite right because of
               --  the business of escaping back slashes, but it's near enough.

               if String_Length (Str) + Column > Sprint_Line_Limit then
                  Write_Indent_Str ("  ");
               end if;

               --  Output string literal

               Write_Char ('"');

               for J in 1 .. String_Length (Str) loop
                  Write_C_Char_Code (Get_String_Char (Str, J));
               end loop;

               Write_Char ('"');
            end;

         when N_Subprogram_Body =>

            --  Skip generic subprograms

            if Present (Corresponding_Spec (Node))
             and then Ekind (Corresponding_Spec (Node)) in
                        Generic_Subprogram_Kind
            then
               null;

            --  Skip front-end-inlined subprograms

            elsif Has_Pragma_Inline_Always
                    (Unique_Defining_Entity (Specification (Node)))
            then
               null;

            --  Declare withed subprograms that have no spec and skip
            --  subprogram bodies outside of main units unless they are
            --  internally built public init-procs.

            elsif not In_Main_Unit then
               if Acts_As_Spec (Node) then
                  declare
                     Subp : constant Entity_Id :=
                              Unique_Defining_Entity (Specification (Node));
                  begin
                     if Nkind (Parent (Node)) = N_Compilation_Unit
                       or else
                         (Is_Init_Proc (Subp)
                           and then Is_Public (Subp)
                           and then not Is_Null_Init_Proc (Subp))
                       or else
                         (Is_TSS (Subp, TSS_Rep_To_Pos)
                           and then Is_Public (Subp))
                     then
                        Cprint_Node (Specification (Node));
                        Write_Str (";");
                     end if;
                  end;
               end if;

            elsif In_Library_Unit_Decl
              and then No (Current_Subp_Entity)
            then
               if Acts_As_Spec (Node) then
                  Write_Indent_Str ("extern ");
                  Cprint_Node (Specification (Node));
                  Write_Str (";");
                  Write_Indent;
               end if;

               Enter_In_Ada_Body;
               Cprint_Subprogram_Body (Node);
               Exit_In_Ada_Body;

            --  Otherwise write subprogram body

            else
               Cprint_Subprogram_Body (Node);
            end if;

         when N_Subprogram_Declaration =>
            declare
               Subp : constant Entity_Id := Unique_Defining_Entity (Node);

            begin
               --  No action needed if this is the declaration of the generic
               --  subprogram instantiation that we are unrolling.

               if Present (Unrolling_Instance_Subp_Id)
                 and then Unrolling_Instance_Subp_Id = Subp
               then
                  pragma Assert (Entity_Table.Get (Subp));
                  goto Leave_Cprint_Node;
               end if;

               Write_Indent;
               Write_Itypes_In_Subtree (Specification (Node));

               --  Do not print intrinsic subprogram as calls to those will be
               --  expanded.

               if Convention (Subp) = Convention_Intrinsic
                 or else Is_Intrinsic_Subprogram (Subp)
               then
                  null;

               --  Do not print functions that return arrays because they have
               --  been rewritten as procedures.

               elsif Ekind (Subp) = E_Function
                 and then Rewritten_For_C (Subp)
               then
                  null;

               --  Do not print C imported subprograms if -gnatd.5

               elsif Debug_Flag_Dot_5
                 and then Is_Imported (Subp)
                 and then Convention (Subp) = Convention_C
               then
                  null;

               --  Do not print front-end-inlined subprograms

               elsif Has_Pragma_Inline_Always (Subp) then
                  null;

               else
                  if Last_Char = ';' then
                     Write_Indent;
                  end if;

                  Cprint_Node (Specification (Node), Declaration => True);
                  Write_Char (';');
               end if;
            end;

         when N_Subprogram_Renaming_Declaration =>
            null; -- not output in C code

         when N_Subtype_Declaration =>
            declare
               Def_Id : constant Entity_Id := Defining_Identifier (Node);

            begin
               --  For unidimensional unconstrained arrays the internal subtype
               --  generated by the front end is not needed by the generated
               --  C code since we directly use the pointer to the array
               --  components available in the fat pointer.

               if Is_Internal (Def_Id)
                 and then Ekind (Def_Id) = E_Array_Subtype
                 and then Is_Unconstrained_Array_Type (Etype (Def_Id))
                 and then No (Next_Index (First_Index (Def_Id)))
               then
                  null;
               else
                  Write_Source_Lines (Node);
                  Cprint_Declare (Defining_Identifier (Node));
               end if;
            end;

         when N_Subtype_Indication =>

            --  Should have been handled higher up in tree

            raise Program_Error;

         when N_Subunit =>

            --  This kind of node is not visible to the back end, since it has
            --  been replaced by the corresponding N_Body_Stub node.

            null;

         when N_Target_Name =>
            Error_Msg_N ("Ada 2022 feature not supported", Node);

         when N_Task_Body
            | N_Task_Definition
         =>
            raise Program_Error;

         when N_Task_Type_Declaration =>
            null;

         when N_Terminate_Alternative
            | N_Timed_Entry_Call
            | N_Triggering_Alternative
         =>
            raise Program_Error;

         when N_Type_Conversion =>
            declare
               Typ     : constant Entity_Id := Entity (Subtype_Mark (Node));
               Src_Typ : constant Entity_Id :=
                           Get_Full_View (Etype (Expression (Node)));

            begin
               --  Conversions from an access-to-constrained-array type to an
               --  access-to-unconstrained-array type must be handled when
               --  processing the parent node since they require initializing
               --  all the components of the target fat pointer.

               if Is_Access_Type (Typ)
                 and then Has_Fat_Pointer (Typ)
                 and then not Has_Fat_Pointer (Etype (Expression (Node)))
               then
                  Error_Msg_N
                    ("unsupported conversion to access to unconstrained array",
                     Node);
               end if;

               --  Casting of array and record types not allowed in C

               if not Is_Array_Type (Typ)
                 and then not Is_Record_Type (Typ)
               then
                  Write_Char ('(');
                  Check_Definition (Typ, Error_Node => Subtype_Mark (Node));
                  Cprint_Type_Name (Typ);
                  Write_Char (')');
               end if;

               --  Handle floating point rounding if needed

               if (Is_Integer_Type (Typ) or else Is_Fixed_Point_Type (Typ))
                 and then Is_Floating_Point_Type (Src_Typ)
                 and then not Float_Truncate (Node)
               then
                  Convert_Float_To_Integer (Expression (Node));
               else
                  Cprint_Node_Paren (Expression (Node));
               end if;

               if Is_Access_Type (Typ)
                 and then not Has_Fat_Pointer (Typ)
                 and then Has_Fat_Pointer (Etype (Expression (Node)))
               then
                  Write_Fatptr_Dereference;
               end if;
            end;

         when N_Unchecked_Expression =>
            raise Program_Error;

         when N_Unchecked_Type_Conversion =>
            declare
               function Is_Pointer_Type (Typ : Entity_Id) return Boolean;
               --  Return True if Typ is an access type or descendant of
               --  System.Address.

               ---------------------
               -- Is_Pointer_Type --
               ---------------------

               function Is_Pointer_Type (Typ : Entity_Id) return Boolean is
               begin
                  return Is_Access_Type (Typ)
                    or else Is_Descendant_Of_Address (Typ);
               end Is_Pointer_Type;

               --  Local variables

               Target_Typ    : constant Entity_Id :=
                                 Get_Full_View (Entity (Subtype_Mark (Node)));
               Parens_Needed : Natural := 0;
               Source        : Node_Id;
               Source_Typ    : Entity_Id;

               Source_Already_Output : Boolean := False;

            begin
               --  In the case of nested unchecked type conversions we generate
               --  code that directly performs the cast of the innermost source
               --  type to the outermost target type. In this way the generated
               --  code is simpler and cleaner (the semantic analyzer has
               --  previously checked that they all match!).

               Source := Expression (Node);
               while Nkind (Source) = N_Unchecked_Type_Conversion loop
                  Source := Expression (Source);
               end loop;

               Source_Typ := Get_Full_View (Etype (Source));

               if Is_Packed_Array (Source_Typ) then
                  Source_Typ := Packed_Array_Impl_Type (Source_Typ);
               end if;

               --  No need to generate a cast if both types match. Compare base
               --  types, since in the generated C code all derived types and
               --  subtypes are equivalent.

               if Base_Type (Source_Typ) = Base_Type (Target_Typ) then
                  null;

               --  Ignore array type conversions which are not supported in C,
               --  and assume this conversion is not needed.

               elsif Ekind (Target_Typ) = E_Array_Subtype then
                  null;

               elsif

                 --  discrete-or-fixed-point <-> discrete-or-fixed-point

                 (Is_Discrete_Or_Fixed_Point_Type (Source_Typ)
                   and then Is_Discrete_Or_Fixed_Point_Type (Target_Typ))

                 --  access/address <-> access/address

                 or else (Is_Pointer_Type (Source_Typ)
                           and then Is_Pointer_Type (Target_Typ))
               then
                  Write_Str ("((");
                  Check_Definition (Target_Typ,
                    Error_Node => Subtype_Mark (Node));
                  Cprint_Type_Name (Target_Typ);
                  Write_Str (")(");
                  Parens_Needed := 2;

               elsif Is_Composite_Type (Source_Typ)
                 or else Is_Composite_Type (Target_Typ)
                 or else Ekind (Source_Typ) /= Ekind (Target_Typ)
               then
                  --  Strip extra type conversion

                  Source := Unqual_Conv (Source);

                  if (Is_Entity_Name (Source)
                        and then Is_Object (Entity (Source)))
                    or else
                      (Nkind (Source) = N_Selected_Component
                         and then Is_Entity_Name (Selector_Name (Source))
                         and then Is_Object (Entity (Selector_Name (Source))))
                  then
                     Write_Str ("(*(");
                     Check_Definition (Target_Typ,
                       Error_Node => Subtype_Mark (Node));
                     Cprint_Type_Name (Target_Typ);
                     Write_Str ("*)(&");
                     Parens_Needed := 2;

                  elsif Is_Real_Type (Etype (Source))
                    and then
                      (Is_Fixed_Point_Type (Target_Typ)
                        or else Is_Integer_Type (Target_Typ))
                  then
                     Write_Str ("((");
                     Check_Definition (Target_Typ,
                       Error_Node => Subtype_Mark (Node));
                     Cprint_Type_Name (Target_Typ);
                     Write_Str (")(");
                     Parens_Needed := 2;

                     Convert_Float_To_Integer (Source);
                     Source_Already_Output := True;

                  elsif Nkind (Source) = N_Explicit_Dereference then
                     Write_Str ("(*(");
                     Check_Definition (Target_Typ,
                       Error_Node => Subtype_Mark (Node));
                     Cprint_Type_Name (Target_Typ);
                     Write_Str ("*)(");
                     Parens_Needed := 2;

                  --  If source is not an object, should do a copy to a
                  --  temporary. For now emit an error.

                  else
                     Error_Msg_N ("unsupported unchecked_conversion", Node);
                  end if;
               else
                  Error_Msg_N ("unsupported unchecked_conversion", Node);
               end if;

               if not Source_Already_Output then
                  Cprint_Node_Paren (Source);
               end if;

               for J in 1 .. Parens_Needed loop
                  Write_Char (')');
               end loop;
            end;

         when N_Unconstrained_Array_Definition
            | N_Unused_At_Start
            | N_Unused_At_End
         =>
            raise Program_Error;

         when N_Use_Package_Clause
            | N_Use_Type_Clause
            | N_Validate_Unchecked_Conversion
         =>
            null;

         when N_Variant
            | N_Variant_Part
         =>
            raise Program_Error;

         --  Variable reference markers are internal tree annotations in the
         --  form of nodes and should not be present in the output.

         when N_Variable_Reference_Marker =>
            null;

         when N_With_Clause =>

            --  "with" clauses can be ignored, since we are dumping all units
            --  inline.

            null;
      end case;

      <<Leave_Cprint_Node>>
      Cprint_Node_Stack.Decrement_Last;
      Dump_Node := Save_Dump_Node;
   end Cprint_Node;

   ----------------------
   -- Cprint_Node_List --
   ----------------------

   procedure Cprint_Node_List (List : List_Id; New_Lines : Boolean := False) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Cprint_Node (Node);
            Next (Node);
            exit when Node = Empty;
         end loop;
      end if;

      if New_Lines and then Column /= 1 then
         Write_Eol;
      end if;
   end Cprint_Node_List;

   -----------------------
   -- Cprint_Node_Paren --
   -----------------------

   procedure Cprint_Node_Paren (N : Node_Id) is
   begin
      --  Add parens if we have an operator or short circuit operation. But
      --  don't add the parens if already parenthesized, since we will get
      --  them anyway and don't add if definitely not needed.

      if (Nkind (N) in N_Op
           or else Nkind (N) in N_And_Then
                              | N_Attribute_Reference
                              | N_Explicit_Dereference
                              | N_If_Expression
                              | N_In
                              | N_Not_In
                              | N_Or_Else)
        and then Parens_Needed (N)
      then
         Write_Char ('(');
         Cprint_Node (N);
         Write_Char (')');
      else
         Cprint_Node (N);
      end if;
   end Cprint_Node_Paren;

   --------------------------
   -- Cprint_Opt_Node_List --
   --------------------------

   procedure Cprint_Opt_Node_List (List : List_Id) is
   begin
      if Present (List) then
         Cprint_Node_List (List);
      end if;
   end Cprint_Opt_Node_List;

   -----------------------------
   -- Cprint_Object_Reference --
   -----------------------------

   function Cprint_Object_Reference
     (Ent        : Entity_Id;
      Add_Access : Boolean := False) return Boolean
   is
      Original_Type : Entity_Id := Etype (Ent);
      Typ           : Entity_Id := Get_Full_View (Original_Type);

      procedure Add_Star;
      --  Outputs '*' if Add_Access is True, otherwise does nothing

      procedure Declare_Array_Formal;
      --  Declare the array formal Ent

      procedure Declare_Object;
      --  Declare the variable associated with Ent

      procedure Declare_Variable_Size_Object;
      --  Declare object Ent whose last field depends on its discriminant
      --  constraints.

      --------------
      -- Add_Star --
      --------------

      procedure Add_Star is
      begin
         if Add_Access then
            Write_Char ('*');
         end if;
      end Add_Star;

      --------------------------
      -- Declare_Array_Formal --
      --------------------------

      procedure Declare_Array_Formal is
         Orig_Full_Type : constant Entity_Id := Get_Full_View (Original_Type);

      begin
         Check_Definition (Orig_Full_Type, Error_Node => Ent);

         if Is_Access_Type (Typ) then
            if Is_Out_Mode_Access_Formal (Ent) then
               if Is_Unconstrained_Array_Type (Typ) then
                  Write_Fatptr_Name (Orig_Full_Type);
               else
                  Cprint_Node (Orig_Full_Type, Declaration => True);
               end if;

               Write_Str ("* ");
            else
               declare
                  DDT : constant Entity_Id :=
                          Get_Full_View (Designated_Type (Orig_Full_Type));

               begin
                  if Is_Constrained_Array_Type (DDT) then
                     Write_Id (DDT);
                     Write_Char (' ');
                  else
                     Write_Id (Orig_Full_Type);
                     Write_Char (' ');
                     Add_Star;
                  end if;
               end;
            end if;
         else
            if Is_Unconstrained_Array_Type (Typ) then
               Write_Fatptr_Name (Orig_Full_Type);
            else
               Cprint_Node (Orig_Full_Type, Declaration => True);
            end if;

            Write_Char (' ');
         end if;

         Cprint_Node (Ent, Declaration => True);
      end Declare_Array_Formal;

      --------------------
      -- Declare_Object --
      --------------------

      procedure Declare_Object is
      begin
         Check_Definition (Original_Type, Error_Node => Ent);

         if Is_Imported (Ent)
           and then In_Main_Unit
           and then not In_Header_File
         then
            Write_Str ("extern ");
         end if;

         Cprint_Node (Original_Type, Declaration => True);

         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent, Declaration => True);

         Check_Attributes (Ent);
      end Declare_Object;

      ----------------------------------
      -- Declare_Variable_Size_Object --
      ----------------------------------

      procedure Declare_Variable_Size_Object is
         Field : constant Node_Id := Last_Field (Typ);
         Rng   : Node_Id;

      begin
         --  For an object declaration whose type is a record with
         --  discriminants and whose last field depends on this
         --  discriminant, generate:
         --    unsigned_8 _<id>[<size>];
         --    #define <id> (*(<type>)_<id>)

         Write_Str ("unsigned_8 _");
         Cprint_Node (Ent, Declaration => True);

         if not In_Main_Unit then
            Write_Str ("[]");

         else
            Write_Str ("[sizeof(");
            Check_Definition (Original_Type, Error_Node => Ent);
            Cprint_Node (Original_Type, Declaration => True);
            Write_Str (") + ");
            Rng := First_Index (Etype (Field));

            if Nkind (Rng) = N_Range
              and then Nkind (Low_Bound (Rng)) = N_Integer_Literal
              and then Nkind (High_Bound (Rng)) = N_Integer_Literal
            then
               --  Note: we do not add +1 here since sizeof() already accounts
               --  for 1 element.

               Write_Uint
                 (Intval (High_Bound (Rng)) -
                  Intval (Low_Bound  (Rng)));
               Write_Str (" * sizeof(");
               Check_Definition (Component_Type (Etype (Field)),
                 Error_Node => Field);
               Cprint_Type_Name (Component_Type (Etype (Field)));
               Write_Char (')');

            else
               Error_Msg_N ("cannot compute size for field", Field);
               Write_Char ('0');
            end if;

            Write_Str ("]");
         end if;

         --  Set attributes: given that this is an internal declaration, when
         --  no alignment is explicitly given to the entity we must propagate
         --  the alignment of its type declaration.

         if No (Alignment_Clause (Ent)) then
            Write_Indent_Str ("GNAT_ALIGN(GNAT_ALIGNOF(");
            Cprint_Node (Typ, Declaration => True);
            Write_Str ("))");
         end if;

         Check_Attributes (Ent);
         Write_Str (";");

         Write_Eol;
         Write_Str ("#define ");
         Cprint_Node (Ent, Declaration => True);
         Write_Str (" (*(");
         Cprint_Node (Original_Type, Declaration => True);
         Write_Str (" *)_");
         Cprint_Node (Ent, Declaration => True);
         Write_Str (")");
         Write_Eol;

         --  Record this macro so that it will be #undef'ed at the end of the
         --  current scope.

         if not Library_Level then
            Macro_Table.Append (Ent);
         end if;

         --  Remember that this entity is defined

         Register_Entity (Ent);
      end Declare_Variable_Size_Object;

   --  Start of processing for Cprint_Object_Reference

   begin
      if Is_Packed_Array (Typ) then
         Output_Packed_Array_Type (Typ);
         Typ := Packed_Array_Impl_Type (Typ);
         Original_Type := Typ;
      end if;

      Check_Volatile_Atomic (Ent);

      if Ekind (Ent) = E_Variable
        and then Is_Supported_Variable_Size_Record (Typ)
      then
         Declare_Variable_Size_Object;
         return False;

      elsif Is_Formal (Ent)
        and then
          (Is_Array_Type (Typ)
            or else
              (Is_Access_Type (Typ)
                and then
                  Is_Array_Type (Get_Full_View (Designated_Type (Typ)))))
      then
         Declare_Array_Formal;
         return True;

      --  Common case

      else
         Declare_Object;
         return True;
      end if;
   end Cprint_Object_Reference;

   ---------------------------
   -- Cprint_Type_Reference --
   ---------------------------

   function Cprint_Type_Reference
     (Ent        : Entity_Id;
      Add_Access : Boolean := False;
      Virtual_OK : Boolean := False) return Boolean
   is
      Original_Type  : constant Entity_Id := Etype (Ent);
      Typ            : constant Entity_Id := Get_Full_View (Original_Type);
      Need_Semicolon : Boolean := True;

      procedure Add_Star;
      --  Outputs '*' if Add_Access is True, otherwise does nothing

      procedure Check_Unsupported_Types;
      --  Report an error on type declarations that have unsupported features

      procedure Declare_Access_Type (Typ : Entity_Id);
      --  Output the declaration of the discrete type Typ

      procedure Declare_Access_To_Array_Type (Typ : Entity_Id);
      --  Output the declaration of the access-to-array type Typ

      procedure Declare_Array_Type
        (Typ            : Entity_Id;
         Need_Semicolon : in out Boolean);
      --  Output the declaration of the array type Typ

      procedure Declare_Dependent_Types (Typ : Entity_Id);
      --  Declare types on which Typ depends. Required to handle private types
      --  since we generate the code of its full view when we see the partial
      --  view.

      procedure Declare_Discrete_Type (Typ : Entity_Id);
      --  Output the declaration of the discrete type Typ

      procedure Declare_Fixed_Point_Type (Typ : Entity_Id);
      --  Output the declaration of the fixed-point type Typ

      procedure Declare_Record_Type (Typ : Entity_Id);
      --  Output the declaration of the record type Typ

      procedure Declare_Subtype (Typ : Entity_Id);
      --  Output the declaration of Ent as a subtype of Typ

      procedure Fix_Indent;
      --  Fix the output indentation (depending on the last character of the
      --  previous output).

      --------------
      -- Add_Star --
      --------------

      procedure Add_Star is
      begin
         if Add_Access then
            Write_Char ('*');
         end if;
      end Add_Star;

      -----------------------------
      -- Check_Unsupported_Types --
      -----------------------------

      procedure Check_Unsupported_Types is
      begin
         if (Is_Record_Type (Ent) or else Is_Array_Type (Ent))
           and then Reverse_Storage_Order (Ent)
         then
            Error_Msg_N ("non default storage order not supported", Ent);
         end if;
      end Check_Unsupported_Types;

      ----------------------------------
      -- Declare_Access_To_Array_Type --
      ----------------------------------

      procedure Declare_Access_To_Array_Type (Typ : Entity_Id) is
      begin
         --  For unconstrained array types, generate a typedef alias of this
         --  access type and the fat pointer of the array.

         if not Is_Constrained (Get_Full_View (Designated_Type (Typ))) then
            Write_Fatptr_Name (Designated_Type (Typ));
            Write_Str (" ");
            Cprint_Node (Ent, Declaration => True);

         --  Constrained array types

         else
            Cprint_Type_Name (Designated_Type (Typ));
            Write_Char (' ');

            --  No need to define a pointer to the array if this access
            --  type is an itype associated with a formal; otherwise we
            --  erroneously would generate the typedef of a pointer to a
            --  pointer.

            if Is_Itype (Ent)
              and then Nkind (Associated_Node_For_Itype (Ent)) in
                         N_Function_Specification | N_Procedure_Specification
            then
               null;
            else
               Write_Char ('*');
            end if;

            Cprint_Node (Ent, Declaration => True);
         end if;
      end Declare_Access_To_Array_Type;

      -------------------------
      -- Declare_Access_Type --
      -------------------------

      procedure Declare_Access_Type (Typ : Entity_Id) is
         procedure Write_Access_To_Subprogram_Decl;
         --  Generate the C profile associated with an access-to-subprogram
         --  declaration.

         -------------------------------------
         -- Write_Access_To_Subprogram_Decl --
         -------------------------------------

         procedure Write_Access_To_Subprogram_Decl is
            DT : constant Node_Id := Designated_Type (Typ);

         begin
            if Etype (DT) = Standard_Void_Type then
               Write_Str ("void ");
            else
               Write_Name_Col_Check (Chars (Etype (DT)));
               Write_Str (" ");
            end if;

            Write_Str_Col_Check ("(*");
            Write_Id (Typ);
            Write_Str_Col_Check (")");

            Write_Param_Specs (DT);
         end Write_Access_To_Subprogram_Decl;

      --  Start of processing for Declare_Access_Type

      begin
         Write_Str ("typedef ");
         Check_Volatile_Atomic (Typ);

         if Ekind (Ent) in E_Access_Protected_Subprogram_Type
                         | E_Access_Subprogram_Type
                         | E_Anonymous_Access_Subprogram_Type
         then
            Write_Access_To_Subprogram_Decl;

         elsif Is_Array_Type (Get_Full_View (Designated_Type (Typ))) then
            Declare_Access_To_Array_Type (Typ);

         else
            declare
               DDT : Entity_Id := Get_Full_View (Designated_Type (Typ));

            begin
               if Is_Record_Type (DDT) then
                  Write_Str ("struct _");
               end if;

               if Ekind (DDT) = E_Record_Subtype then
                  DDT := Etype (DDT);
               end if;

               --  Although for class-wide types we generated a typedef
               --  renaming the C type associated with its root type we
               --  must reference here the Root_Type; otherwise the C
               --  compiler reports that we are referencing an incomplete
               --  type.

               if Is_Class_Wide_Type (DDT) then
                  Cprint_Type_Name (Root_Type (DDT));
               else
                  Cprint_Type_Name (DDT);
               end if;
            end;

            --  For access-to-subprogram references there is no need to
            --  generate an explicit dereference, since we generate a
            --  typedef which has it (see Write_Access_To_Subprogram_Decl).

            if Ekind (Typ) = E_Anonymous_Access_Subprogram_Type then
               Write_Str (" ");
            else
               Write_Str (" *");
               Add_Star;
            end if;

            Cprint_Node (Ent, Declaration => True);
         end if;

         Check_Attributes (Ent);
      end Declare_Access_Type;

      ------------------------
      -- Declare_Array_Type --
      ------------------------

      procedure Declare_Array_Type
        (Typ            : Entity_Id;
         Need_Semicolon : in out Boolean)
      is
         Idx : Nat := 0;
         --  Position of the current array index

         procedure Declare_Pointer_To_Component_Type;
         --  Output declaration of pointer to component type

         procedure Output_Known_Length (Indx : Node_Id);
         --  If the array length exceeds the maximum C array length then
         --  generate code that computes it; otherwise compute the array
         --  length at compile time.

         procedure Check_Bounds (Indx : Node_Id);
         --  For library level array type declarations check that bounds are
         --  known at compile time (since at file scope C does not allow the
         --  declaration of arrays whose size depends on a variable).

         procedure Check_Large_Array_Type (Typ : Entity_Id);
         --  Check if the array length is supported

         ------------------
         -- Check_Bounds --
         ------------------

         procedure Check_Bounds (Indx : Node_Id) is
            LBD : constant Node_Id := Type_Low_Bound (Etype (Indx));
            UBD : constant Node_Id := Type_High_Bound (Etype (Indx));
            Nod : Node_Id;

         begin
            if (In_Header_File
                  or else (In_Main_Unit
                             and then Library_Level))
              and then (not Compile_Time_Known_Value (LBD)
                          or else not Compile_Time_Known_Value (UBD))
              and then No (Current_Subp_Entity)
            then
               if not Compile_Time_Known_Value (LBD) then
                  Nod := LBD;
               else
                  Nod := UBD;
               end if;

               Error_Msg_N
                 ("unsupported variably modified C array at file scope", Nod);
            end if;
         end Check_Bounds;

         ----------------------------
         -- Check_Large_Array_Type --
         ----------------------------

         procedure Check_Large_Array_Type (Typ : Entity_Id) is
            Indx         : Node_Id := First_Index (Typ);
            Total_Length : Uint    := Uint_0;

         begin
            loop
               declare
                  Length : Uint;
                  LBD    : constant Node_Id := Type_Low_Bound (Etype (Indx));
                  UBD    : constant Node_Id := Type_High_Bound (Etype (Indx));
               begin
                  if Compile_Time_Known_Value (LBD)
                    and then Compile_Time_Known_Value (UBD)
                  then
                     Length := Expr_Value (UBD) - Expr_Value (LBD) + Uint_1;
                     Total_Length := Total_Length + Length;
                  end if;
               end;

               if Exceeds_Maximum_Array_Length (Total_Length) then
                  Error_Msg_N
                    ("object too large: last_chance_handler will be called at"
                     & " run time??", Typ);
                  Write_Str (Last_Chance & ";");
                  Write_Indent;
                  return;
               end if;

               Idx := Idx + 1;
               Next_Index (Indx);
               exit when No (Indx);
            end loop;
         end Check_Large_Array_Type;

         ---------------------------------------
         -- Declare_Pointer_To_Component_Type --
         ---------------------------------------

         procedure Declare_Pointer_To_Component_Type is
         begin
            Write_Str ("typedef ");
            Check_Volatile_Atomic (Typ);
            Cprint_Type_Name (Component_Type (Typ));
            Write_Char (' ');

            if not Is_Constrained (Typ) then
               Write_Char ('*');
            end if;

            Cprint_Node (Ent, Declaration => True);
         end Declare_Pointer_To_Component_Type;

         -------------------------
         -- Output_Known_Length --
         -------------------------

         procedure Output_Known_Length (Indx : Node_Id) is
            LBD      : constant Node_Id := Type_Low_Bound (Etype (Indx));
            UBD      : constant Node_Id := Type_High_Bound (Etype (Indx));
            Length   : Uint;

         begin
            Length := Expr_Value (UBD) - Expr_Value (LBD) + Uint_1;

            if Length < Uint_0 then
               Length := Uint_0;

            --  If the array length exceeds the maximum C array length then
            --  generate code that computes the maximum length.

            elsif Exceeds_Maximum_Array_Length (Length) then

               --  For multidimensional arrays we cannot compute the maximum
               --  array length.

               if Idx > 1 then
                  Error_Msg_N ("unsupported array length", Indx);
               end if;

               Write_Uint (LPosInt);
               Write_Char ('/');
               Output_Sizeof (Component_Type (Typ));
               return;
            end if;

            Write_Uint (Length);
         end Output_Known_Length;

         --  Local variables

         Indx : Node_Id;
         LBD  : Node_Id;
         UBD  : Node_Id;

      --  Start of processing for Declare_Array_Type

      begin
         Check_Definition (Component_Type (Typ), Error_Node => Ent);

         if Size_Known_At_Compile_Time (Typ) then
            Check_Large_Array_Type (Typ);
         end if;

         --  For multidimensional unconstrained array types, declare the
         --  typedef of its fat pointer.

         if not Is_Constrained (Typ) then
            if not Is_Unidimensional_Array_Type (Typ) then
               Write_Fatptr_Declare (Ent);
               Need_Semicolon := False;
            else
               Declare_Pointer_To_Component_Type;
            end if;

         --  Handle constrained array types

         else
            Declare_Pointer_To_Component_Type;

            --  Loop through subscripts

            Idx  := 1;
            Indx := First_Index (Typ);
            loop
               Write_Char ('[');

               if Is_Constrained (Typ) or not Virtual_OK then
                  Check_Bounds (Indx);

                  LBD := Type_Low_Bound (Etype (Indx));
                  UBD := Type_High_Bound (Etype (Indx));

                  if Compile_Time_Known_Value (LBD) then
                     if Compile_Time_Known_Value (UBD) then
                        Output_Known_Length (Indx);

                     elsif Expr_Value (LBD) = 1 then
                        Cprint_Node (UBD);

                     elsif Expr_Value (LBD) < 1 then
                        Cprint_Sum (UBD, 1 - Expr_Value (LBD), False);

                     else
                        Cprint_Difference
                          (UBD, Expr_Value (LBD) - 1, B => False);
                     end if;
                  else
                     Cprint_Difference (UBD, LBD, Minus_One_Min => True);
                     Write_Str (" + 1");
                  end if;
               end if;

               Write_Char (']');

               Idx := Idx + 1;
               Next_Index (Indx);
               exit when No (Indx);
            end loop;
         end if;

         Check_Attributes (Ent);
      end Declare_Array_Type;

      -----------------------------
      -- Declare_Dependent_Types --
      -----------------------------

      procedure Declare_Dependent_Types (Typ : Entity_Id) is
         procedure Declare_Access_Dependent_Types;
         --  Force the declaration of the designated type of access type Typ

         procedure Declare_Array_Dependent_Types;
         --  Force the declaration of the component type of the array type Typ

         procedure Declare_Record_Dependent_Types;
         --  Force the declaration of the types of the discriminants and
         --  components of the record type Typ.

         ------------------------------------
         -- Declare_Access_Dependent_Types --
         ------------------------------------

         procedure Declare_Access_Dependent_Types is
         begin
            --  For access types ensure that its designated type is declared.
            --  Access-to-procedure types are obviously excluded;
            --  access-to-record types are excluded since Cprint_Type_Reference
            --  has support for access-to-incomplete record types. For example:

            --        type Rec;
            --        type Ptr is access all Rec;
            --        type Rec is record
            --           Next : Ptr;
            --        end record;

            if Etype (Designated_Type (Typ)) /= Standard_Void_Type
              and then not
                Is_Record_Type (Get_Full_View (Designated_Type (Typ)))
            then
               Dump_Type (Designated_Type (Typ));
            end if;

            --  Ensure that the designated type of access-to-constrained-array
            --  types is defined.

            if Ekind (Ent) = E_Access_Subtype
              and then Is_Constrained_Array_Type
                         (Get_Full_View (Designated_Type (Ent)))
            then
               Dump_Type (Designated_Type (Ent));
            end if;
         end Declare_Access_Dependent_Types;

         -----------------------------------
         -- Declare_Array_Dependent_Types --
         -----------------------------------

         procedure Declare_Array_Dependent_Types is
         begin
            Dump_Type (Component_Type (Typ));
         end Declare_Array_Dependent_Types;

         ------------------------------------
         -- Declare_Record_Dependent_Types --
         ------------------------------------

         procedure Declare_Record_Dependent_Types is
            procedure Declare_Component_Types (Clist : Node_Id);
            --  Recursive routine to declare the type of each list component

            procedure Declare_Discriminant_Types (Typ : Node_Id);
            --  Declare the type of each discriminant of Typ

            -----------------------------
            -- Declare_Component_Types --
            -----------------------------

            procedure Declare_Component_Types (Clist : Node_Id) is
               Comp : Node_Id;
               Typ  : Entity_Id;
               Var  : Node_Id;

            begin
               --  No action needed if no components are available

               if No (Clist) then
                  return;
               end if;

               Comp := First (Component_Items (Clist));
               while Present (Comp) loop

                  --  Skip the declaration of component types defined in
                  --  Standard

                  if Nkind (Comp) = N_Component_Declaration
                    and then Sloc (Etype (Defining_Identifier (Comp))) >
                               Standard_Location
                  then
                     Typ := Etype (Defining_Identifier (Comp));

                     --  Skip types depending on discriminants

                     if Size_Depends_On_Discriminant (Typ) then
                        Register_Entity (Typ);
                     else
                        Dump_Type (Typ);
                     end if;

                     if Is_Packed_Array (Typ) then
                        Dump_Type (Packed_Array_Impl_Type (Typ));
                     end if;
                  end if;

                  Next (Comp);
               end loop;

               --  Handle variant part

               if Present (Variant_Part (Clist)) then
                  Var := First (Variants (Variant_Part (Clist)));

                  while Present (Var) loop
                     Declare_Component_Types (Component_List (Var));
                     Next (Var);
                  end loop;
               end if;
            end Declare_Component_Types;

            --------------------------------
            -- Declare_Discriminant_Types --
            --------------------------------

            procedure Declare_Discriminant_Types (Typ : Node_Id) is
               Discr : Entity_Id;

            begin
               Discr := First_Discriminant (Typ);

               while Present (Discr) loop
                  Dump_Type (Etype (Discr));
                  Next_Discriminant (Discr);
               end loop;
            end Declare_Discriminant_Types;

            --  Local variables

            Decl : Node_Id := Declaration_Node (Typ);
            RecD : Node_Id;

         --  Start of processing for Declare_Record_Dependent_Types

         begin
            if Nkind (Decl) = N_Private_Type_Declaration then
               Decl :=
                 Declaration_Node (Get_Full_View (Defining_Identifier (Decl)));
            end if;

            if Nkind (Decl) = N_Full_Type_Declaration then
               if Has_Discriminants (Typ) then
                  Declare_Discriminant_Types (Typ);
               end if;

               RecD := Type_Definition (Decl);

               if Nkind (RecD) = N_Derived_Type_Definition then
                  RecD := Record_Extension_Part (RecD);
               end if;

               if Nkind (RecD) = N_Record_Definition then
                  Declare_Component_Types (Component_List (RecD));
               end if;
            else
               declare
                  S : constant String := Node_Kind'Image (Nkind (Decl));
               begin
                  Error_Msg_Strlen := S'Length;
                  Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                  Error_Msg_N ("unsupported type (~)", Decl);
               end;

               Write_Str ("/* unsupported type */");
            end if;
         end Declare_Record_Dependent_Types;

      --  Start of processing for Declare_Dependent_Types

      begin
         --  For record types ensure that the types of all the components are
         --  declared before we generate the corresponding C struct.

         if Is_Record_Type (Typ) then
            if Ekind (Typ) not in E_Record_Subtype |
                                  E_Record_Subtype_With_Private
            then
               Declare_Record_Dependent_Types;
            end if;

         --  For array types ensure that its component type is declared

         elsif Is_Array_Type (Typ) then
            Declare_Array_Dependent_Types;

         elsif Is_Access_Type (Typ) then
            Declare_Access_Dependent_Types;
         end if;
      end Declare_Dependent_Types;

      ---------------------------
      -- Declare_Discrete_Type --
      ---------------------------

      procedure Declare_Discrete_Type (Typ : Entity_Id) is
      begin
         Write_Str ("typedef ");
         Check_Volatile_Atomic (Typ);
         Check_Definition (Typ, Error_Node => Ent);
         Cprint_Type_Name (Typ, Use_Typedef => Typ /= Ent);
         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent, Declaration => True);
         Check_Attributes (Ent);

         if Is_Enumeration_Type (Typ)
           and then Sloc (Typ) > Standard_Location
         then
            Write_Char (';');
            Write_Indent;
            Declare_Enumeration_Type (Typ);
         end if;
      end Declare_Discrete_Type;

      ------------------------------
      -- Declare_Fixed_Point_Type --
      ------------------------------

      procedure Declare_Fixed_Point_Type (Typ : Entity_Id) is
      begin
         Write_Str ("typedef ");
         Check_Volatile_Atomic (Typ);

         if Etype (Typ) = Typ then

            --  The front end generates unsigned types for fixed-point type
            --  derivations whose range is positive; however, given that
            --  we declare them using their base type, we always generate
            --  signed integer types.

            pragma Assert (not Is_Unsigned_Type (Typ));

            Write_Integer_Type
              (UI_To_Int (Esize (Typ)), Signed => not Is_Unsigned_Type (Typ));
         else
            Check_Definition (Typ, Error_Node => Ent);
            Cprint_Node (Typ, Declaration => True);
         end if;

         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent, Declaration => True);
         Check_Attributes (Ent);
      end Declare_Fixed_Point_Type;

      -------------------------
      -- Declare_Record_Type --
      -------------------------

      procedure Declare_Record_Type (Typ : Entity_Id) is
         procedure Declare_Class_Wide_Type;
         --  Output the declaration of the class-wide record type Typ

         procedure Output_Components;
         --  Output record components

         -----------------------------
         -- Declare_Class_Wide_Type --
         -----------------------------

         procedure Declare_Class_Wide_Type is
         begin
            if Ekind (Ent) /= E_Class_Wide_Subtype then
               Write_Str ("typedef struct _");
               Cprint_Node (Root_Type (Ent));
               Write_Str (" _");
               Cprint_Node (Ent, Declaration => True);
               Write_Char (';');
               Write_Indent;
            end if;

            Write_Str ("typedef ");
            Cprint_Node (Root_Type (Ent), Declaration => True);
            Write_Char (' ');
            Cprint_Node (Ent, Declaration => True);
         end Declare_Class_Wide_Type;

         -----------------------
         -- Output_Components --
         -----------------------

         procedure Output_Components is
            Decl           : constant Node_Id := Declaration_Node (Typ);
            Has_Rep_Clause : constant Boolean := Has_Non_Standard_Rep (Typ);
            Comp_Clauses   : List_Id := No_List;
            RecD           : Node_Id;
            Rep_Item       : Node_Id;
            Ignore_Rep     : Boolean;
            Pad_Num        : Int;

            procedure Output_Component_List (Clist : Node_Id);
            --  Recursive routine to output a component list

            procedure Write_Padding (Padding : Uint; Rep_Clause : Node_Id);
            --  Write padding field of Padding size (in bits). Generate an
            --  error message and set Ignore_Rep to True if not possible.

            ---------------------------
            -- Output_Component_List --
            ---------------------------

            procedure Output_Component_List (Clist : Node_Id) is
               Comp       : Node_Id;
               Comp_Size  : Uint;
               Comp_Typ   : Entity_Id;
               FB         : Uint;
               LB         : Uint;
               Pos        : Uint;
               Prev_Bit   : Uint    := Uint_Minus_1;
               Prev_Pos   : Uint    := Uint_0;
               Rep_Clause : Node_Id := Empty;
               Siz        : Uint;
               Var        : Node_Id;

               Bitfield_Warned : Boolean := False;
               Ignore_Comp_Rep : Boolean := False;

               procedure Unsupported_Rep_clause (Comp : Node_Id);
               --  Emit a warning about unsupported representation clause
               --  and ignore further rep clauses.

               ----------------------------
               -- Unsupported_Rep_clause --
               ----------------------------

               procedure Unsupported_Rep_clause (Comp : Node_Id) is
               begin
                  Cprint_Declare (Defining_Identifier (Comp));
                  Error_Msg_N
                    ("??unsupported representation clause, assuming "
                     & "confirming", Rep_Clause);
                  Ignore_Rep := True;

                  --  Reset Has_Non_Standard_Rep since we are ignoring it

                  Set_Has_Non_Standard_Rep
                    (Get_Full_View (Etype (Ent)), False);
               end Unsupported_Rep_clause;

            --  Start of processing for Output_Component_List

            begin
               --  No action needed if no components are available

               if No (Clist) then
                  return;
               end if;

               if Comp_Clauses /= No_List then
                  Rep_Clause := First (Comp_Clauses);
               end if;

               Ignore_Rep := False;
               Pad_Num    := 1;

               --  Output components (ignore types, pragmas etc)

               Comp := First (Component_Items (Clist));

               --  Look for relevant component clause if any

               if Present (Comp)
                 and then Present (Rep_Clause)
                 and then Entity (Component_Name (Rep_Clause)) /=
                            Defining_Identifier (Comp)
               then
                  Error_Msg_N
                    ("unsupported representation clause (out of order)",
                     Rep_Clause);
                  return;
               end if;

               while Present (Comp) loop
                  if Nkind (Comp) = N_Component_Declaration then
                     Comp_Typ :=
                       Get_Full_View (Etype (Defining_Identifier (Comp)));

                     if Present (Rep_Clause) and then not Ignore_Rep then

                        --  Check that the rep clause has no holes since we
                        --  only support this configuration for now. Also
                        --  check that components are not larger than 64 bits.

                        FB  := Intval (First_Bit (Rep_Clause));
                        LB  := Intval (Last_Bit (Rep_Clause));
                        Pos := Intval (Position (Rep_Clause));

                        if LB < Uint_64 then

                           --  Is padding needed before this field?

                           if not ((FB = Uint_0
                                    and then Pos =
                                      Prev_Pos + (Prev_Bit + Uint_1) / Uint_8)
                                   or else (FB = Prev_Bit + Uint_1
                                            and then Pos = Prev_Pos))
                           then
                              Write_Padding
                                ((Pos - Prev_Pos) * 8 + FB - Prev_Bit - 1,
                                 Rep_Clause);
                              Prev_Bit := LB;
                              Prev_Pos := Pos;
                           end if;

                           --  Use type as-is if it is compatible with C
                           --  bitfields (integer types).

                           Siz := LB - FB + Uint_1;
                           Comp_Size := Esize (Comp_Typ);

                           --  First check whether we have a confirming rep
                           --  clause.

                           if Siz = Comp_Size
                             or else
                             (Is_Record_Type (Comp_Typ)
                                and then Has_Record_Rep_Clause (Comp_Typ))
                           then
                              Write_Indent;
                              Cprint_Declare
                                (Defining_Identifier (Comp),
                                 Semicolon => True);
                              Ignore_Comp_Rep := True;

                           --  Replace type by an integer of the right size
                           --  (32 or 64 bits) when possible.

                           elsif Is_Discrete_Type (Comp_Typ) then
                              Write_Indent;

                              if Comp_Size <= Uint_32 then
                                 Write_Integer_Type (32,
                                   Signed => not Is_Unsigned_Type (Comp_Typ));
                              else
                                 Write_Integer_Type (64,
                                   Signed => not Is_Unsigned_Type (Comp_Typ));
                              end if;

                              Write_Char (' ');
                              Cprint_Node
                                (Defining_Identifier (Comp),
                                 Declaration => True);

                           else
                              Unsupported_Rep_clause (Comp);
                           end if;

                           --  Handle some cases of padding, when the size of
                           --  the component type is known by the front end.

                           if Comp_Size > Uint_0 and then Comp_Size < Siz then
                              Siz := Siz - Comp_Size;

                              if Siz > Uint_64 then
                                 Error_Msg_N
                                   ("??unsupported representation clause (size"
                                    & "exceeds 64 bits), assuming confirming",
                                    Rep_Clause);
                                 Ignore_Rep := True;
                                 Set_Has_Non_Standard_Rep
                                   (Get_Full_View (Etype (Ent)), False);

                              else
                                 Write_Str (" : ");
                                 Write_Uint (Comp_Size);
                                 Write_Char (';');
                                 Write_Indent;

                                 if Siz <= Uint_32 then
                                    Write_Integer_Type (32, Signed => False);
                                 else
                                    Write_Integer_Type (64, Signed => False);
                                 end if;

                                 Write_Str (" _pad");
                                 Write_Int (Pad_Num);
                                 Pad_Num := Pad_Num + 1;
                              end if;
                           end if;

                           Prev_Bit := LB;
                           Prev_Pos := Pos;

                           if Ignore_Comp_Rep then
                              Ignore_Comp_Rep := False;
                           elsif not Ignore_Rep then
                              if not Bitfield_Warned then
                                 Error_Msg_N
                                   ("??representation clause mapped to non " &
                                    "portable bitfield", Rep_Clause);
                                 Bitfield_Warned := True;
                              end if;

                              Write_Str (" : ");
                              Write_Uint (Siz);
                              Write_Char (';');
                           end if;
                        else
                           --  Skip error for runtime files

                           if not In_Predefined_Unit (Rep_Clause) then
                              Error_Msg_N
                                ("??unsupported representation clause, " &
                                 "assuming confirming",
                                 Rep_Clause);
                              Ignore_Rep := True;
                              Set_Has_Non_Standard_Rep
                                (Get_Full_View (Etype (Ent)), False);
                           end if;

                           Cprint_Declare (Defining_Identifier (Comp));
                        end if;

                        Next (Rep_Clause);

                     else
                        if Size_Depends_On_Discriminant (Comp_Typ) then
                           Write_Indent;
                           Cprint_Node (Component_Type (Base_Type (Comp_Typ)));
                           Write_Char (' ');
                           Cprint_Node (Defining_Identifier (Comp));
                           Write_Str ("[1];");

                        else
                           Cprint_Declare (Defining_Identifier (Comp));
                        end if;
                     end if;
                  end if;

                  Next (Comp);
               end loop;

               --  Output variant part if present

               if Present (Variant_Part (Clist)) then

                  if not Is_Simple_Unchecked_Union (Typ) then
                     Write_Indent_Str ("union {");
                     Indent_Begin;
                  end if;

                  Var := First (Variants (Variant_Part (Clist)));
                  while Present (Var) loop
                     declare
                        VCList  : constant Node_Id := Component_List (Var);
                        VCItems : constant List_Id := Component_Items (VCList);

                     begin
                        --  No output when there are no components in this
                        --  component list. For example, this case corresponds
                        --  with a variant specifying 'when others => null;'.

                        if Is_Empty_List (VCItems) then
                           null;

                        --  If only one component in this component list, we
                        --  can output it as a single member of the union.

                        elsif List_Length (VCItems) = 1 then
                           Output_Component_List (VCList);

                        --  Otherwise we have more than one component, so we
                        --  have to introduce a named struct.

                        else
                           Write_Indent_Str ("struct {");
                           Indent_Begin;
                           Output_Component_List (VCList);
                           Indent_End;
                           Write_Indent_Str ("} ");
                           Output_Anon_Struct_Name (Var);
                           Write_Char (';');
                        end if;
                     end;

                     Next (Var);
                  end loop;

                  --  Generate named unions and structs since anonymous unions
                  --  and structs are not supported by C90

                  if not Is_Simple_Unchecked_Union (Typ) then
                     Indent_End;
                     Write_Indent_Str ("} ");
                     Write_Str (Anon_Union_Prefix);
                     Write_Char (';');
                  end if;
               end if;

               --  Add padding to fill the specified record size if needed

               if Bitfield_Warned
                 and then not Ignore_Rep
                 and then Known_Static_RM_Size (Declare_Record_Type.Typ)
               then
                  Siz :=
                    RM_Size (Declare_Record_Type.Typ) -
                      (Prev_Pos * 8 + Prev_Bit + 1);

                  if Siz > 0 then
                     Write_Padding (Siz, Declare_Record_Type.Typ);
                  end if;
               end if;
            end Output_Component_List;

            -------------------
            -- Write_Padding --
            -------------------

            procedure Write_Padding (Padding : Uint; Rep_Clause : Node_Id) is

               procedure Write_Padding_Variable (Padding : Uint);
               --  Write a padding variable of size padding

               procedure Write_Padding_Variable (Padding : Uint) is
               begin
                  Write_Indent;

                  if Padding <= Uint_32 then
                     Write_Integer_Type (32, Signed => False);
                  else
                     Write_Integer_Type (64, Signed => False);
                  end if;

                  Write_Str (" _pad");
                  Write_Int (Pad_Num);
                  Pad_Num := Pad_Num + 1;
                  Write_Str (" : ");
                  Write_Uint (Padding);
                  Write_Char (';');
               end Write_Padding_Variable;

            begin
               if Padding < Uint_0 then
                  Error_Msg_N
                    ("unsupported representation clause (unordered bits)",
                     Rep_Clause);

               elsif Padding > Uint_64 then
                  declare
                     Padd : Uint := Padding;
                  begin
                     loop
                        Write_Padding_Variable (Uint_64);
                        Padd := Padd - Uint_64;
                        exit when Padd < Uint_64;
                     end loop;

                     if Padd /= 0 then
                        Write_Padding_Variable (Padd);
                     end if;
                  end;
               else
                  Write_Padding_Variable (Padding);
               end if;
            end Write_Padding;

         --  Start of output for Output_Components

         begin
            --  For now, limit cases we handle

            if Nkind (Decl) = N_Full_Type_Declaration then
               RecD := Type_Definition (Decl);

               if Nkind (RecD) = N_Derived_Type_Definition then
                  RecD := Record_Extension_Part (RecD);
               end if;

               if Nkind (RecD) = N_Record_Definition then
                  if Has_Rep_Clause then
                     Rep_Item := First_Rep_Item (Typ);

                     while Present (Rep_Item)
                       and then Nkind (Rep_Item) /=
                                  N_Record_Representation_Clause
                     loop
                        Next_Rep_Item (Rep_Item);
                     end loop;

                     if Present (Rep_Item) then
                        Comp_Clauses := Component_Clauses (Rep_Item);

                        if Present (Variant_Part (Component_List (RecD))) then
                           Error_Msg_N
                             ("unsupported representation clause with variant "
                              & "part", Rep_Item);
                           return;
                        end if;
                     end if;
                  end if;

                  --  Output discriminants (and skip them for Unchecked_Union)

                  declare
                     Disc : Node_Id;
                  begin
                     if Present (Discriminant_Specifications (Decl))
                       and then not Is_Unchecked_Union (Typ)
                     then
                        Disc := First (Discriminant_Specifications (Decl));

                        while Present (Disc) loop
                           Cprint_Declare (Defining_Identifier (Disc));
                           Next (Disc);
                        end loop;
                     end if;
                  end;

                  --  Empty structs are not permitted in C99 (6.2.5:20,
                  --  "A structure type describes a sequentially allocated
                  --  nonempty set of member objects."). We mirror here
                  --  the C++ behavior, where empty structs have size 1.

                  if No (Discriminant_Specifications (Decl))
                    and then No (Component_List (RecD))
                  then
                     Write_Indent_Str ("character _null_record;");

                  --  Output components

                  else
                     Output_Component_List (Component_List (RecD));
                  end if;
               else
                  declare
                     S : constant String := Node_Kind'Image (Nkind (Decl));
                  begin
                     Error_Msg_Strlen := S'Length;
                     Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                     Error_Msg_N ("unsupported type (~)", Decl);
                  end;

                  Error_Msg_N ("unsupported type", Decl);
                  Write_Str ("/* unsupported type */");
               end if;
            else
               declare
                  S : constant String := Node_Kind'Image (Nkind (Decl));
               begin
                  Error_Msg_Strlen := S'Length;
                  Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                  Error_Msg_N ("unsupported type (~)", Decl);
               end;

               Error_Msg_N ("unsupported type", Decl);
               Write_Str ("/* unsupported type */");
            end if;
         end Output_Components;

      --  Start of processing for Declare_Record_Type

      begin
         if Is_Class_Wide_Type (Typ) then
            Declare_Class_Wide_Type;
            return;
         end if;

         if Is_Interface (Typ) then
            Error_Msg_N ("interface types not supported", Typ);
         end if;

         Write_Str ("typedef ");
         Check_Volatile_Atomic (Typ);

         if Is_Simple_Unchecked_Union (Typ) then
            Write_Str ("union ");
         else
            Write_Str ("struct ");
         end if;

         if Is_Packed (Typ) and then not Full_Code_Generation then
            Error_Msg_N ("packed structs not supported", Typ);
         end if;

         Write_Char ('_');
         Cprint_Node (Ent, Declaration => False);
         Write_Str (" {");
         Indent_Begin;

         Output_Components;

         Indent_End;
         Write_Indent_Str ("} ");
         Add_Star;

         --  Add GNAT_PACKED to indicate to the C compiler that it must use the
         --  minimum required memory to represent the type (since bit-field
         --  components are declared using 32- or 64-bit unsigned types).

         if Has_Record_Rep_Clause (Typ) then
            Write_Str ("GNAT_PACKED ");
         end if;

         Cprint_Node (Ent, Declaration => True);
         Check_Attributes (Ent);
      end Declare_Record_Type;

      ---------------------
      -- Declare_Subtype --
      ---------------------

      procedure Declare_Subtype (Typ : Entity_Id) is
      begin
         Write_Str ("typedef ");
         Check_Volatile_Atomic (Typ);

         --  Handle the declaration of access subtypes whose designated type is
         --  a constrained array type. This is specially needed if the access
         --  type is a subtype of an access-to-unconstrained-array type, since
         --  no fat pointer will be used with this access subtype (the bounds
         --  of the array type are available in the constrained designated
         --  type).

         if Ekind (Ent) = E_Access_Subtype
           and then Is_Constrained_Array_Type
                      (Get_Full_View (Designated_Type (Ent)))
         then
            Declare_Access_To_Array_Type (Ent);
            return;
         end if;

         --  When declaring a scalar typedef, check whether the base type and
         --  the subtype have the same size, otherwise use a different base
         --  type.

         if Is_Scalar_Type (Typ)
           and then Esize (Typ) /= Esize (Ent)
         then
            Write_Integer_Type
              (UI_To_Int (Esize (Ent)),
               Signed => not Is_Unsigned_Or_Modular_Type (Ent));

         elsif Ekind (Ent) = E_String_Literal_Subtype then
            Write_Str ("character");

         elsif Ekind (Ent) = E_Private_Subtype then
            Check_Definition (Typ, Error_Node => Ent);
            Cprint_Node (Typ, Declaration => True);

            --  Ensure that we do not generate dummy typedef declarations
            --  like: "typedef sometype sometype;"

            pragma Assert (Chars (Typ) /= Chars (Ent));

         else
            Check_Definition (Original_Type, Error_Node => Ent);
            Cprint_Node (Original_Type, Declaration => True);

            --  Ensure that we do not generate dummy typedef declarations
            --  like: "typedef sometype sometype;"

            pragma Assert (Chars (Original_Type) /= Chars (Ent));
         end if;

         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent, Declaration => True);

         if Ekind (Ent) = E_String_Literal_Subtype then
            declare
               Val : Uint := String_Literal_Length (Ent);
            begin
               Write_Str ("[");

               if Val < Uint_0 then
                  Val := Uint_0;
               end if;

               Write_Uint (Val);
               Write_Char (']');
            end;
         end if;

         Check_Attributes (Ent);
      end Declare_Subtype;

      ----------------
      -- Fix_Indent --
      ----------------

      procedure Fix_Indent is
      begin
         if Last_Char = ';' or else Last_Char = '{' then
            Write_Indent;
         end if;
      end Fix_Indent;

   --  Start of processing for Cprint_Type_Reference;

   begin
      if not In_Declarations then
         Open_Extra_Scope;
      end if;

      Check_Unsupported_Types;

      --  Ensure the declaration of Typ and its dependent types. Required for
      --  private types since we generate the code of its full view when we see
      --  the partial view.

      if Ekind (Ent) = E_Record_Type then
         Declare_Dependent_Types (Ent);
      end if;

      Declare_Dependent_Types (Typ);
      Fix_Indent;

      if not Entity_Table.Get (Typ)
        and then (Entity_Is_In_Main_Unit (Typ) or else Is_Itype (Typ))
      then
         Cprint_Declare (Typ);
         Write_Indent;
      end if;

      --  Untagged private types and all subtypes (including untagged record
      --  subtypes). Array subtypes are excluded since their lengths most
      --  probably differ.

      if Typ /= Ent
        and then Ekind (Ent) /= E_Array_Subtype
        and then not Is_Tagged_Type (Ent)
      then

         --  Force the declaration of its parent type if we are unrolling a
         --  private type since we may be traversing subtype declarations
         --  bottom-up. Skip entities defined in Standard.

         if Unrolling_Full_Type_Decl
           and then Sloc (Typ) > Standard_Location
         then
            Cprint_Declare (Typ);

            if Last_Char = ';' then
               Write_Indent;
            end if;
         end if;

         Declare_Subtype (Typ);

      --  Discrete type

      elsif Is_Discrete_Type (Typ) then
         Declare_Discrete_Type (Typ);

      --  Access type

      elsif Is_Access_Type (Typ) then
         Declare_Access_Type (Typ);

      --  Record type

      elsif Is_Record_Type (Typ) then
         if Ekind (Ent) in E_Record_Subtype | E_Record_Subtype_With_Private
         then
            Declare_Subtype (Ent);
         else
            pragma Assert
              (Is_Class_Wide_Type (Ent) or else Ekind (Ent) = E_Record_Type);
            Declare_Record_Type (Ent);
         end if;

      --  Array type

      elsif Is_Array_Type (Typ) then
         --  For array subtypes, directly use this entity to compute the
         --  length of the array.

         if Ekind (Ent) = E_Array_Subtype then
            Declare_Array_Type (Ent, Need_Semicolon);
         else
            Declare_Array_Type (Typ, Need_Semicolon);
         end if;

      elsif Is_Fixed_Point_Type (Typ) then
         Declare_Fixed_Point_Type (Typ);

      --  For anything else, other than a type declaration, assume we have
      --  typedef reference.

      elsif Typ /= Ent then
         Write_Str ("typedef ");
         Check_Volatile_Atomic (Typ);
         Check_Definition (Typ, Error_Node => Ent);
         Cprint_Node (Typ, Declaration => True);
         Write_Char (' ');
         Add_Star;
         Cprint_Node (Ent, Declaration => True);
         Check_Attributes (Ent);

      --  Generate an error on other cases

      else
         declare
            S : constant String := Entity_Kind'Image (Ekind (Typ));
         begin
            Error_Msg_Strlen := S'Length;
            Error_Msg_String (1 .. Error_Msg_Strlen) := S;
            Error_Msg_N ("unsupported type (~)", Typ);
         end;

         Need_Semicolon := False;
      end if;

      return Need_Semicolon;
   end Cprint_Type_Reference;

   -----------------------
   -- Cprint_Right_Opnd --
   -----------------------

   procedure Cprint_Right_Opnd (N : Node_Id) is
      Opnd : constant Node_Id := Right_Opnd (N);
   begin
      Cprint_Node_Paren (Opnd);
   end Cprint_Right_Opnd;

   ------------------------------
   -- Append_Subprogram_Prefix --
   ------------------------------

   procedure Append_Subprogram_Prefix (Spec : Node_Id) is
      function Name_String (Name : Name_Id) return String;
      --  Returns the name string associated with Name

      function New_Name_Id (Name : String) return Name_Id;
      --  Returns a Name_Id corresponding to the given name string

      -----------------
      -- Name_String --
      -----------------

      function Name_String (Name : Name_Id) return String is
      begin
         pragma Assert (Name /= No_Name);
         return Get_Name_String (Name);
      end Name_String;

      -----------------
      -- New_Name_Id --
      -----------------

      function New_Name_Id (Name : String) return Name_Id is
      begin
         for J in 1 .. Name'Length loop
            Name_Buffer (J) := Name (Name'First + (J - 1));
         end loop;

         Name_Len := Name'Length;
         return Name_Find;
      end New_Name_Id;

      --  Local variables

      Subp : constant Entity_Id := Unique_Defining_Entity (Spec);

   --  Start of processing for Append_Subprogram_Prefix

   begin
      if Is_Compilation_Unit (Subp)
        and then Back_End_Stage = Generating_Output
      then
         declare
            Prefix    : constant String := "_ada_";
            Subp_Name : Name_Id := Chars (Subp);
            Subp_Str  : constant String := Name_String (Subp_Name);

         begin
            --  Do not append the prefix if already done as part of processing
            --  its declaration.

            if Subp_Str'Length <= Prefix'Length
              or else
                Subp_Str (Subp_Str'First ..
                          Subp_Str'First + Prefix'Length - 1) /= Prefix
            then
               Subp_Name := New_Name_Id ("_ada_" & Name_String (Subp_Name));
               Set_Chars (Subp, Subp_Name);
            end if;
         end;
      end if;
   end Append_Subprogram_Prefix;

   ----------------------------
   -- Cprint_Subprogram_Body --
   ----------------------------

   --  Note: we already dealt with outputting the header for this subprogram

   procedure Cprint_Subprogram_Body (N : Node_Id) is
      procedure Output_One_Body (Node : Node_Id);
      --  Output a single subprogram body, for this call, any subprogram nested
      --  within this subprogram will have been removed.

      procedure Unnest_Types (Scop : Entity_Id; N : Node_Id);
      --  Force the declaration of the relevant types referenced in the tree N
      --  and which are not defined in the scope Scop.

      procedure Unsupported_Nested_Subprogram (N : Node_Id);
      --  Locate the first inner nested subprogram and report the error on it

      ---------------------
      -- Output_One_Body --
      ---------------------

      procedure Output_One_Body (Node : Node_Id) is

         function Instance_Wrapper_Id (N : Node_Id) return Entity_Id;
         --  Return the unique identifier of the wrapper package internally
         --  built by the frontend for the subprogram body instantiation N.

         -------------------------
         -- Instance_Wrapper_Id --
         -------------------------

         function Instance_Wrapper_Id (N : Node_Id) return Entity_Id is
            Subp_Id : constant Entity_Id := Unique_Defining_Entity (N);
            E       : Entity_Id;
            P       : Node_Id := Parent (N);

         begin
            while Present (P) and then Nkind (P) /= N_Package_Body loop
               P := Parent (P);
            end loop;

            E := Unique_Defining_Entity (P);

            --  Ensure that the returned entity is correct

            pragma Assert (Ekind (E) = E_Package
              and then Is_Generic_Instance (E)
              and then Present (Related_Instance (E))
              and then (Subp_Id = Unique_Defining_Entity
                                    (Parent (Related_Instance (E)))
                          or else
                            (Present (Corresponding_Function (Subp_Id))
                               and then
                             Corresponding_Function (Subp_Id)
                               = Unique_Defining_Entity
                                   (Parent (Related_Instance (E))))));

            return E;
         end Instance_Wrapper_Id;

         --  Local variables

         Prev_Id : constant Entity_Id := Current_Subp_Entity;
         Subp_Id : constant Entity_Id := Unique_Defining_Entity (Node);
         Scop_Id : Nat;

      begin
         --  For instantiations of generic subprograms the frontend generates
         --  a wrapper package that includes the mappings of generic parameters
         --  into actuals (Sem_Ch12.Analyze_Instance_And_Renamings). Therefore,
         --  before generating the code of a subprogram instantiation, we must
         --  output all the declarations found in the spec of such wrapper but
         --  forcing the declaration (unrolling) all its dependant types to
         --  ensure their full declaration in the generated C code.

         if Is_Generic_Instance (Subp_Id) then
            declare
               Wrapper_Id : constant Entity_Id := Instance_Wrapper_Id (Node);

            begin
               --  Unroll subtype declarations found in the specification of
               --  the wrapper package.

               if not Entity_Table.Get (Wrapper_Id) then
                  declare
                     Prev_Id : constant Entity_Id :=
                                 Unrolling_Instance_Subp_Id;
                  begin
                     Unrolling_Instance_Subp_Id := Subp_Id;
                     Cprint_Node (Parent (Wrapper_Id));
                     Unrolling_Instance_Subp_Id := Prev_Id;
                  end;
               end if;
            end;
         end if;

         Unnest_Types (Subp_Id, Node);

         Library_Level := False;
         Ensure_New_Line;
         Write_Source_Lines (Specification (Node));

         Write_Indent;
         Cprint_Node (Declaration_Node (Subp_Id));

         Write_Char (' ');
         Open_Scope;
         Scop_Id := Current_Scope_Id;
         Current_Subp_Entity := Subp_Id;
         Declare_Back_End_Itypes (Subp_Id);

         if Is_Non_Empty_List (Declarations (Node)) then
            Cprint_Indented_List (Declarations (Node));
         end if;

         Set_In_Statements;
         Cprint_Node (Handled_Statement_Sequence (Node));

         --  #undef registered macros for this subprogram, if any

         for J in 1 .. Macro_Table.Last loop
            Write_Indent_Str ("#undef ");
            Write_Id (Macro_Table.Table (J));
         end loop;

         if Macro_Table.Last > 0 then
            Write_Indent;
         end if;

         Macro_Table.Set_Last (0);

         Write_Indent;

         --  Close this scope plus all its inner scopes (that is, its extra
         --  back-end scopes and the deferred scopes of its nested packages;
         --  see Cprint_Node.N_Package_Specification).

         Close_Scope (Scop_Id);

         Library_Level := True;
         Current_Subp_Entity := Prev_Id;
      end Output_One_Body;

      ------------------
      -- Unnest_Types --
      ------------------

      procedure Unnest_Types (Scop : Entity_Id; N : Node_Id) is
         function Depends_On_Formals (Itype : Entity_Id) return Boolean;
         --  Return True if Itype is an array type whose definition depends on
         --  the formals of a subprogram.

         function Search_Type_Refs (Node : Node_Id) return Traverse_Result;
         --  Subtree visitor which looks for relevant references to types
         --  and declare them.

         ------------------------
         -- Depends_On_Formals --
         ------------------------

         function Depends_On_Formals (Itype : Entity_Id) return Boolean is
            function References_Formal (N : Node_Id) return Boolean;
            --  Return True if N is 'First or 'Last applied to a subprogram
            --  formal.

            -----------------------
            -- References_Formal --
            -----------------------

            function References_Formal (N : Node_Id) return Boolean is
            begin
               return Nkind (N) = N_Attribute_Reference
                  and then Nkind (Prefix (N)) in N_Has_Entity
                  and then Is_Formal (Entity (Prefix (N)))
                  and then
                    (Get_Attribute_Id (Attribute_Name (N)) = Attribute_First
                       or else
                     Get_Attribute_Id (Attribute_Name (N)) = Attribute_Last);
            end References_Formal;

         --  Start of processing for Depends_On_Formals

         begin
            if not Is_Array_Type (Itype) then
               return False;
            end if;

            declare
               Ind : Node_Id := First_Index (Itype);

            begin
               while Present (Ind) loop
                  if Nkind (Ind) = N_Range
                    and then
                      (References_Formal (Low_Bound (Ind))
                         or else References_Formal (High_Bound (Ind)))
                  then
                     return True;
                  end if;

                  Next_Index (Ind);
               end loop;
            end;

            return False;
         end Depends_On_Formals;

         ----------------------
         -- Search_Type_Refs --
         ----------------------

         function Search_Type_Refs (Node : Node_Id) return Traverse_Result is
            Typ : Entity_Id := Empty;

         begin
            case Nkind (Node) is
               when N_Attribute_Reference =>
                  if Get_Attribute_Id (Attribute_Name (Node)) = Attribute_Deref
                  then
                     Typ := Get_Full_View (Etype (Prefix (Node)));
                  end if;

               when N_Type_Conversion =>
                  Typ := Get_Full_View (Entity (Subtype_Mark (Node)));

               when N_Unchecked_Type_Conversion =>
                  Typ := Get_Full_View (Entity (Subtype_Mark (Node)));

                  --  For UCs we want to unnest as many types as possible to
                  --  inline UCs in e.g. Cprint_Copy, but beware of dynamic
                  --  array types created to handle aggregates.

                  if Enclosing_Dynamic_Scope (Typ) /= Scop
                    and then not (Is_Itype (Typ) and then Is_Array_Type (Typ))
                  then
                     Dump_Type (Typ);
                     return OK;
                  end if;

               when N_Object_Declaration =>
                  Typ := Get_Full_View (Etype (Defining_Identifier (Node)));

               when others =>
                  null;
            end case;

            if Present (Typ)
              and then Scope_Depth (Scope (Typ)) < Scope_Depth (Scop)
              and then not Depends_On_Formals (Typ)
            then
               Dump_Type (Typ);
            end if;

            return OK;
         end Search_Type_Refs;

         procedure Search is new Traverse_Proc (Search_Type_Refs);
         --  Subtree visitor instantiation

         --  Local variables

         In_Search_Type_Ref_Save : constant Boolean := In_Search_Type_Ref;

      --  Start of processing for Unnest_Types

      begin
         In_Search_Type_Ref := True;
         Search (N);
         In_Search_Type_Ref := In_Search_Type_Ref_Save;
      end Unnest_Types;

      -----------------------------------
      -- Unsupported_Nested_Subprogram --
      -----------------------------------

      procedure Unsupported_Nested_Subprogram (N : Node_Id) is
         function Search_Subprogram (Node : Node_Id) return Traverse_Result;
         --  Subtree visitor which looks for the subprogram

         -----------------------
         -- Search_Subprogram --
         -----------------------

         function Search_Subprogram (Node : Node_Id) return Traverse_Result is
         begin
            if Node /= N
              and then Nkind (Node) = N_Subprogram_Body

               --  Do not report the error on generic subprograms; the error
               --  will be reported only in their instantiations (to leave the
               --  output more clean).

              and then not
                Is_Generic_Subprogram (Unique_Defining_Entity (Node))
            then
               Error_Msg_N ("unsupported kind of nested subprogram", Node);
               return Abandon;
            end if;

            return OK;
         end Search_Subprogram;

         procedure Search is new Traverse_Proc (Search_Subprogram);
         --  Subtree visitor instantiation

      --  Start of processing for Unsupported_Nested_Subprogram

      begin
         Search (N);
      end Unsupported_Nested_Subprogram;

      --  Local declarations

      Subp : constant Entity_Id := Unique_Defining_Entity (N);

   --  Start of processing for Cprint_Subprogram_Body

   begin
      if In_Package_Body_Init or else Present (Current_Subp_Entity) then
         Error_Msg_N ("unsupported kind of nested subprogram", N);
         return;

      --  If no nested subprograms, just output the body

      elsif not Has_Nested_Subprogram (Subp) then
         Output_One_Body (N);
         return;

      --  Protect against unsupported kind of nested subprograms
      --  (for example, subprograms defined in nested instantiations).

      elsif Subps_Index (Subp) = Uint_0 then
         Unsupported_Nested_Subprogram (N);
         return;
      end if;

      --  Here we deal with a subprogram with nested subprograms

      declare
         Subps_First : constant SI_Type := UI_To_Int (Subps_Index (Subp));
         Subps_Last  : constant SI_Type := Subps.Table (Subps_First).Last;
         --  First and last indexes for Subps table entries for this nest

         pragma Assert (Subps_First /= 0);

      begin
         --  First step is to output the declarations for ARECnT and ARECnPT
         --  for each subprogram which define these entities for an activation
         --  record. These are generated at the outer level, so that they can
         --  be referenced by the unnested bodies. The ordering is important,
         --  since inner activation records refer to entities in outer records
         --  but the order of entries in Subp guarantees this is the case.

         Output_AREC : for J in Subps_First .. Subps_Last loop
            declare
               STJ   : Subp_Entry renames Subps.Table (J);
               Decls : constant List_Id := Declarations (STJ.Bod);
               Decl  : Node_Id;
               Typ   : Entity_Id;

            begin
               if Present (STJ.ARECnT) then

                  --  First declaration is either an access type for
                  --  unconstrained arrays, or a declaration for ARECnT.

                  Decl := First (Decls);
                  while Present (Decl) loop
                     Typ := Defining_Identifier (Decl);
                     if Ekind (Typ) /= E_General_Access_Type then
                        pragma Assert (Typ = STJ.ARECnT);
                        Cprint_Node (Decl);

                        --  Next declaration must be for ARECnPT

                        Next (Decl);
                        Typ  := Defining_Identifier (Decl);
                        pragma Assert (Typ = STJ.ARECnPT);
                        Cprint_Node (Decl);
                        exit;

                     else
                        Cprint_Node (Decl);
                        Next (Decl);
                     end if;
                  end loop;
               end if;
            end;
         end loop Output_AREC;

         --  Next step is to generate headers for all the nested bodies, and
         --  also for the outer level body if it acts as its own spec. The
         --  order of these does not matter, since we have already output all
         --  the declarations they might reference.

         Output_Headers : for J in Subps_First .. Subps_Last loop
            declare
               STJ : Subp_Entry renames Subps.Table (J);

            begin
               if J /= Subps_First or else Acts_As_Spec (STJ.Bod) then
                  Ensure_New_Line;
                  Write_Source_Lines (Specification (STJ.Bod));
                  Write_Indent;
                  Cprint_Node (Declaration_Node (STJ.Ent));
                  Write_Char (';');

                  --  If there is a separate subprogram specification, remove
                  --  it, since we have now dealt with outputting this spec.

                  if Present (Corresponding_Spec (STJ.Bod)) then
                     Remove (Parent
                       (Declaration_Node (Corresponding_Spec (STJ.Bod))));
                  end if;
               end if;
            end;
         end loop Output_Headers;

         --  Now we can output the actual bodies, we do this in reverse order
         --  so that we deal with and remove the inner level bodies first. That
         --  way when we print the enclosing subprogram, the body is gone!

         Output_Bodies : for J in reverse Subps_First + 1 .. Subps_Last loop
            declare
               STJ : Subp_Entry renames Subps.Table (J);
            begin
               Output_One_Body (STJ.Bod);

               if Is_List_Member (STJ.Bod) then
                  Remove (STJ.Bod);
               end if;
            end;
         end loop Output_Bodies;

         --  And finally we output the outer level body and we are done

         Output_One_Body (N);
      end;
   end Cprint_Subprogram_Body;

   ----------------
   -- Cprint_Sum --
   ----------------

   procedure Cprint_Sum (Val1 : Node_Id; Val2 : Uint; B : Boolean) is
      Modular : constant Boolean := Is_Modular_Integer_Type (Etype (Val1));
   begin
      if Compile_Time_Known_Value (Val1) then
         Write_Uint (Expr_Value (Val1) + Val2, Modular => Modular);

      elsif Val2 = 0 then
         Cprint_Node (Val1);

      elsif B then
         Write_Str_Col_Check ("(");
         Cprint_Node (Val1);
         Write_Str_Col_Check (" + ");
         Write_Uint (Val2, Modular => Modular);
         Write_Str_Col_Check (")");

      else
         Cprint_Node (Val1);
         Write_Str_Col_Check (" + ");
         Write_Uint (Val2, Modular => Modular);
      end if;
   end Cprint_Sum;

   ----------------------
   -- Cprint_Type_Name --
   ----------------------

   procedure Cprint_Type_Name
     (Typ         : Entity_Id;
      Use_Typedef : Boolean := True)
   is
   begin
      --  Print typedef name if available unless inhibited

      if Use_Typedef then
         if Is_Packed_Array (Typ) then
            Cprint_Node (Packed_Array_Impl_Type (Typ));
         else
            Cprint_Node (Typ);
         end if;

      --  System.Address and descendants

      elsif Is_Descendant_Of_Address (Typ) then
         Write_Str ("void*");

      --  Discrete types

      elsif Is_Discrete_Type (Typ) and then Sloc (Typ) > Standard_Location then
         Write_Integer_Type
           (UI_To_Int (Esize (Typ)),
            Signed => not Is_Unsigned_Or_Modular_Type (Typ),
            Typ    => Typ);

      --  One-dimensional unconstrained array type

      elsif Is_Unconstrained_Array_Type (Typ)
        and then Number_Dimensions (Typ) = 1
      then
         Cprint_Type_Name (Component_Type (Typ));
         Write_Char ('*');

      --  Constrained array type

      elsif Is_Constrained_Array_Type (Typ) then
         declare
            Indx : Node_Id;
            LBD  : Node_Id;
            UBD  : Node_Id;

         begin
            Cprint_Type_Name (Component_Type (Typ));

            --  Loop through subscripts

            Indx := First_Index (Typ);
            loop
               Write_Char ('[');
               LBD := Type_Low_Bound (Etype (Indx));
               UBD := Type_High_Bound (Etype (Indx));

               if Compile_Time_Known_Value (LBD) then
                  if Expr_Value (LBD) = 1 then
                     Cprint_Node (UBD);
                  else
                     Cprint_Difference (UBD, Expr_Value (LBD) - 1, B => False);
                  end if;
               else
                  Cprint_Difference (UBD, LBD, Minus_One_Min => True);
                  Write_Str (" + 1");
               end if;

               Write_Char (']');
               Next_Index (Indx);
               exit when No (Indx);
            end loop;
         end;

      --  Access type

      elsif Is_Access_Type (Typ)
        and then (Is_Discrete_Type (Designated_Type (Typ))
                    or else Is_Record_Type (Designated_Type (Typ)))
      then
         if Is_Record_Type (Designated_Type (Typ)) then
            Write_Str ("struct _");
         end if;

         Cprint_Type_Name (Designated_Type (Typ));
         Write_Char ('*');

      --  Otherwise assume we have typedef reference

      else
         Cprint_Node (Typ);
      end if;
   end Cprint_Type_Name;

   -----------------------
   -- Debug_Write_Chars --
   -----------------------

   procedure Debug_Write_Chars (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      if No (N) then
         Write_Str ("<empty> ");
         return;
      end if;

      if Nkind (N) in N_Has_Chars then
         Write_Name (Chars (N));
      end if;

      Write_Str ("(");
      Write_Int (Int (N));
      Write_Str (")");

      if Loc /= No_Location then
         Write_Str (" [");
         Write_Location (Loc);
         Write_Str ("]");
      end if;
   end Debug_Write_Chars;

   --------------------------------------
   -- Debug_Write_Cprint_Declare_Stack --
   --------------------------------------

   procedure Debug_Write_Cprint_Declare_Stack is
   begin
      Write_Eol;
      Write_Str ("---------- Cprint_Declare_Stack");
      Write_Eol;

      for J in reverse 1 .. Cprint_Declare_Stack.Last loop
         Write_Int (J);
         Write_Char (':');
         Debug_Write_Chars (Cprint_Declare_Stack.Table (J));
         Write_Eol;
      end loop;
   end Debug_Write_Cprint_Declare_Stack;

   -----------------------------------
   -- Debug_Write_Cprint_Node_Stack --
   -----------------------------------

   procedure Debug_Write_Cprint_Node_Stack is
   begin
      Write_Eol;
      Write_Str ("---------- Cprint_Node_Stack");
      Write_Eol;

      for J in reverse 1 .. Cprint_Node_Stack.Last loop
         Write_Int (J);
         Write_Char (':');
         Debug_Write_Chars (Cprint_Node_Stack.Table (J));
         Write_Eol;
      end loop;
   end Debug_Write_Cprint_Node_Stack;

   ---------------------------------
   -- Debug_Write_Type_References --
   ---------------------------------

   procedure Debug_Write_Type_References is
      Debug : constant Boolean := False;

      procedure Output_Itype_References;
      --  Output references of itypes stored in the Types Table

      procedure Output_Object_References;
      --  Output references of variables and constants stored in the Objects
      --  Table

      procedure Output_References (Element : Objects_Table_Element_Access);
      --  Output all the references of the given objects table element

      procedure Output_References (Element : Types_Table_Element_Access);
      --  Output all the references of the given types table element

      procedure Output_Type_References;
      --  Output references of non-itype types stored in the Types Table

      -----------------------------
      -- Output_Itype_References --
      -----------------------------

      procedure Output_Itype_References is
      begin
         Write_Str ("   ================");
         Write_Eol;
         Write_Str ("   Itype References");
         Write_Eol;
         Write_Str ("   ================");
         Write_Eol;

         declare
            Key   : Node_Id := Empty;
            Value : Types_Table_Element_Access := null;
         begin
            Types_Table.Get_First (Key, Value);
            while Value /= null loop
               if Is_Itype (Key) then
                  Debug_Write_Chars (Key);

                  if Value.Declaration_Skipped then
                     Write_Str (" [Skipped]");
                  end if;

                  Write_Eol;
                  Output_References (Value);
               end if;

               Types_Table.Get_Next (Key, Value);
            end loop;
         end;
      end Output_Itype_References;

      ------------------------------
      -- Output_Object_References --
      ------------------------------

      procedure Output_Object_References is
      begin
         Write_Str ("   =================");
         Write_Eol;
         Write_Str ("   Object References");
         Write_Eol;
         Write_Str ("   =================");
         Write_Eol;

         declare
            Key   : Node_Id := Empty;
            Value : Objects_Table_Element_Access := null;

         begin
            Objects_Table.Get_First (Key, Value);
            while Value /= null loop
               Debug_Write_Chars (Key);
               Write_Eol;
               Output_References (Value);

               Objects_Table.Get_Next (Key, Value);
            end loop;
         end;
      end Output_Object_References;

      -----------------------
      -- Output_References --
      -----------------------

      procedure Output_References (Element : Objects_Table_Element_Access) is
         Elmt : Elmt_Id;

      begin
         if Present (Element.References) then
            Elmt := First_Elmt (Element.References);
            while Present (Elmt) loop
               Write_Str ("  Ref: ");
               Debug_Write_Chars (Node (Elmt));
               Write_Eol;

               Next_Elmt (Elmt);
            end loop;
         end if;
      end Output_References;

      -----------------------
      -- Output_References --
      -----------------------

      procedure Output_References (Element : Types_Table_Element_Access) is
         Elmt : Elmt_Id;

      begin
         if Present (Element.References) then
            Elmt := First_Elmt (Element.References);
            while Present (Elmt) loop
               Write_Str ("  Ref: ");
               Debug_Write_Chars (Node (Elmt));
               Write_Eol;

               Next_Elmt (Elmt);
            end loop;
         end if;
      end Output_References;

      ----------------------------
      -- Output_Type_References --
      ----------------------------

      procedure Output_Type_References is
      begin
         Write_Str ("   ===============");
         Write_Eol;
         Write_Str ("   Type References");
         Write_Eol;
         Write_Str ("   ===============");
         Write_Eol;

         declare
            Key   : Node_Id := Empty;
            Value : Types_Table_Element_Access := null;

         begin
            Types_Table.Get_First (Key, Value);
            while Value /= null loop
               if not Is_Itype (Key) then
                  Debug_Write_Chars (Key);

                  if Value.Declaration_Skipped then
                     Write_Str (" [Skipped]");
                  end if;

                  Write_Eol;
                  Output_References (Value);
               end if;

               Types_Table.Get_Next (Key, Value);
            end loop;
         end;
      end Output_Type_References;

   --  Start of processing for Debug_Write_Type_References

   begin
      if not Debug then
         return;
      end if;

      In_Comment := True;
      Write_Str ("/*");
      Write_Eol;
      Write_Str ("Number of tree traversals: ");
      Write_Int (Tree_Traversals_Counter + 1);
      Write_Eol;

      Output_Type_References;
      Output_Itype_References;
      Output_Object_References;

      Write_Str ("*/");
      Write_Eol;
      In_Comment := False;
   end Debug_Write_Type_References;

   ------------------------------
   -- Declare_Enumeration_Type --
   ------------------------------

   procedure Declare_Enumeration_Type (Typ : Entity_Id) is
      Lit : Node_Id := First_Literal (Typ);

   begin
      pragma Assert (Present (Lit));
      Write_Str ("enum {");

      loop
         Write_Id (Lit);
         Write_Char ('=');
         Write_Uint (Enumeration_Rep (Lit));
         Next_Literal (Lit);

         exit when No (Lit);

         Write_Str (", ");
      end loop;

      Write_Str ("}");
   end Declare_Enumeration_Type;

   ------------------------------
   -- Declare_Subprogram_Types --
   ------------------------------

   procedure Declare_Subprogram_Types (N : Node_Id) is
      Designator : constant Entity_Id := Unique_Defining_Entity (N);
      Formal     : Node_Id;

   begin
      --  Loop through formals (including any Extra_Formals)

      if Nkind (N) in N_Entity and then Is_Itype (N) then
         Formal := First_Formal_With_Extras (N);
      else
         Formal := First_Formal_With_Extras (Unique_Defining_Entity (N));
      end if;

      while Present (Formal) loop
         Dump_Type (Etype (Formal));
         Next_Formal_With_Extras (Formal);
      end loop;

      if Ekind (Designator) = E_Function then
         Dump_Type (Etype (Designator));
      end if;

      Dump_Delayed_Itype_Decls;

      if Last_Char = ';' then
         Write_Indent;
      end if;
   end Declare_Subprogram_Types;

   ---------------
   -- Dump_Type --
   ---------------

   procedure Dump_Type (Typ : Entity_Id) is
   begin
      if not Entity_Table.Get (Typ)
        and then Sloc (Typ) > Standard_Location
      then
         --  Cannot dump record subtypes until their parent type has been
         --  declared. This situation occurs when Dump_Type() is invoked to
         --  output access to incomplete type declarations.

         if Is_Itype (Typ)
           and then Ekind (Typ) = E_Record_Subtype
           and then not Entity_Table.Get (Etype (Typ))
         then
            Register_Delayed_Itype_Decl (Typ);
            return;
         end if;

         if Is_Array_Type (Typ) then
            Dump_Type (Component_Type (Typ));

            if Is_Packed_Array (Typ) then
               Dump_Type (Packed_Array_Impl_Type (Typ));
            end if;
         end if;

         --  For private types the front end may assign different names to the
         --  entities of the partial and full view of private types, and the
         --  full view must be output before the partial view.

         if Is_Private_Type (Typ) then
            declare
               Full : constant Node_Id := Get_Full_View (Typ);
            begin
               if Full /= Typ then
                  Dump_Type (Get_Full_View (Typ));

                  --  When the names of the partial and full view differ and
                  --  the full view is an itype we cannot output its partial
                  --  view since in such case Cprint_Node [N_Defining_Identif]
                  --  will also output the name of the full view and otherwise
                  --  we would generate a duplicate C type declaration.

                  if not Name_Equals (Chars (Get_Full_View (Typ)), Chars (Typ))
                    and then not Is_Itype (Get_Full_View (Typ))
                  then
                     Cprint_Declare (Typ);
                  end if;
               else
                  Cprint_Declare (Typ);
               end if;
            end;
         else
            Cprint_Declare (Typ);
         end if;

         if Is_Access_Type (Typ) then
            declare
               N : constant Node_Id :=
                     Get_Full_View (Directly_Designated_Type (Typ));
            begin
               if Ekind (N) not in E_Subprogram_Type | E_Class_Wide_Type then
                  Dump_Type (N);
               end if;
            end;
         end if;
      end if;
   end Dump_Type;

   --------
   -- db --
   --------

   procedure db (S : String; N : Int) is
   begin
      Write_Eol;
      Write_Eol;
      Write_Str (">>>>>>>>> ");
      Write_Str (S);
      Write_Str (" N = ");
      Write_Int (N);
      Write_Str (" <<<<<<<<<");
      Write_Eol;
      Write_Eol;
   end db;

   ---------------------
   -- Ensure_New_Line --
   ---------------------

   procedure Ensure_New_Line is
   begin
      if Column /= 1 then
         Write_Eol;
      end if;

      for J in 1 .. Indent loop
         Write_Char (' ');
      end loop;
   end Ensure_New_Line;

   ----------------------------------
   -- Exceeds_Maximum_Array_Length --
   ----------------------------------

   function Exceeds_Maximum_Array_Length (Length : Uint) return Boolean is
   begin
      return Length > LPosInt;
   end Exceeds_Maximum_Array_Length;

   ----------------
   -- First_Line --
   ----------------

   function First_Line (N : Node_Id) return Physical_Line_Number is
   begin
      Get_First_Last_Line (N);
      return FLCache_FL;
   end First_Line;

   -------------------
   -- Get_Full_View --
   -------------------

   function Get_Full_View (Id : Entity_Id) return Entity_Id is
   begin
      if Id /= Standard_Void_Type
        and then (Is_Type (Id) or else Ekind (Id) = E_Constant)
        and then Present (Full_View (Id))
      then
         return Full_View (Id);
      else
         return Id;
      end if;
   end Get_Full_View;

   -------------------------
   -- Get_First_Last_Line --
   -------------------------

   procedure Get_First_Last_Line (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      First_Sloc : Source_Ptr;
      Last_Sloc  : Source_Ptr;

      function Process (N : Node_Id) return Traverse_Result;
      --  Process function for traversal

      procedure Traverse is new Traverse_Proc (Process);

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
         Loc : constant Source_Ptr := Sloc (N);

      begin
         if Loc > No_Location
           and then Get_Source_File_Index (Loc) = Current_Source_File
         then
            if First_Sloc = No_Location or else Loc < First_Sloc then
               First_Sloc := Loc;
            end if;

            if Last_Sloc = No_Location or else Loc > Last_Sloc then
               Last_Sloc := Loc;
            end if;
         end if;

         return OK;
      end Process;

   --  Start of processing for Get_First_Last_Line

   begin
      --  Nothing to do if this is cached value

      if N = FLCache_N then
         return;
      else
         FLCache_N := N;
      end if;

      --  If not from current source file, or no source location available,
      --  then set no line number results

      if Loc <= No_Location
        or else Get_Source_File_Index (Loc) /= Current_Source_File
      then
         FLCache_FL := No_Physical_Line_Number;
         FLCache_LL := No_Physical_Line_Number;
         return;
      end if;

      --  Otherwise do the traversal

      First_Sloc := No_Location;
      Last_Sloc  := No_Location;
      Traverse (N);

      if First_Sloc = No_Location then
         FLCache_FL := No_Physical_Line_Number;
      else
         FLCache_FL := Get_Physical_Line_Number (First_Sloc);
      end if;

      if Last_Sloc = No_Location then
         FLCache_LL := No_Physical_Line_Number;
      else
         FLCache_LL := Get_Physical_Line_Number (Last_Sloc);
      end if;

      FLCache_N := N;
   end Get_First_Last_Line;

   ----------------------
   -- Handle_Attribute --
   ----------------------

   procedure Handle_Attribute (N : Node_Id) is
      Attr_Name   : constant Name_Id      := Attribute_Name (N);
      Attr_Id     : constant Attribute_Id := Get_Attribute_Id (Attr_Name);
      Attr_Prefix : constant Node_Id      := Prefix (N);
      Prefix_Type : constant Entity_Id    :=
                      Get_Full_View (Etype (Attr_Prefix));

      procedure Handle_First_Last (Id : Attribute_Id);
      --  Handle 'First/'Last attribute as specified by Id.

      -----------------------
      -- Handle_First_Last --
      -----------------------

      procedure Handle_First_Last (Id : Attribute_Id) is
         Expr      : constant List_Id := Expressions (N);
         Idx       : Nat := 1;
         Pass_Ptr  : Boolean := False;
         Use_Paren : Boolean;

      begin
         if Is_Array_Type (Prefix_Type) then
            if Present (Expr) then
               Idx := UI_To_Int (Intval (Nlists.First (Expr)));
            end if;

            if Is_Unconstrained_Array_Type (Prefix_Type) then
               if Nkind (Attr_Prefix) in N_Has_Entity
                 and then Present (Entity (Attr_Prefix))
               then
                  Use_Paren := False;
               else
                  Use_Paren := True;
               end if;

               if Use_Paren then
                  Write_Char ('(');
               end if;

               if Nkind (Attr_Prefix) = N_Explicit_Dereference then
                  Cprint_Node (Prefix (Attr_Prefix));
                  Pass_Ptr := False;

               elsif Nkind (Attr_Prefix) = N_Function_Call then
                  Error_Msg_N
                    ("unsupported call to function returning unconstrained " &
                     "array", Attr_Prefix);

               elsif Nkind (Attr_Prefix) = N_If_Expression then
                  Error_Msg_N
                    ("unsupported use of conditional expression", Attr_Prefix);

               elsif Nkind (Attr_Prefix) = N_Expression_With_Actions then
                  Error_Msg_N
                    ("unsupported context for expression with actions",
                     Attr_Prefix);

               else
                  Cprint_Node (Entity (Attr_Prefix));
                  Pass_Ptr := Pass_Pointer (Entity (Attr_Prefix));
               end if;

               if Use_Paren then
                  Write_Char (')');
               end if;

               if Pass_Ptr then
                  Write_Str ("->");
               else
                  Write_Char ('.');
               end if;

               --  Reference the corresponding fat pointer value

               if Id = Attribute_First then
                  Write_Fatptr_First (Prefix_Type, Idx);
               else
                  Write_Fatptr_Last (Prefix_Type, Idx);
               end if;

            --  Selected components and identifiers

            else
               declare
                  Bound : Node_Id;
                  Index : Node_Id := First_Index (Prefix_Type);
                  Rng   : Node_Id;

               begin
                  for J in 2 .. Idx loop
                     Next_Index (Index);
                  end loop;

                  if Nkind (Index) = N_Subtype_Indication then
                     Index := Range_Expression (Constraint (Index));
                  end if;

                  if Nkind (Index) = N_Range then
                     if Id = Attribute_First then
                        Bound := Low_Bound (Index);
                     else
                        Bound := High_Bound (Index);
                     end if;

                     if Nkind (Bound) = N_Identifier
                       and then Present (Entity (Bound))
                     then
                        Bound := Entity (Bound);

                        if Ekind (Bound) = E_Discriminant then
                           Write_Char ('(');
                           Cprint_Node (Prefix (Attr_Prefix));
                           Write_Str (").");
                        end if;
                     end if;

                     Check_Definition (Bound, Error_Node => N);
                     Cprint_Node (Bound);

                  elsif Nkind (Index) = N_Identifier
                    and then Present (Entity (Index))
                    and then Nkind (Entity (Index)) = N_Defining_Identifier
                  then
                     Rng := Scalar_Range (Entity (Index));

                     case Nkind (Rng) is
                        when N_Range =>
                           null;

                        when N_Subtype_Indication =>
                           Rng := Range_Expression (Constraint (Rng));

                        when others =>
                           Unimplemented_Attribute
                             (N, Attr_Name, Node_Kind'Image (Nkind (Rng)));
                     end case;

                     if Id = Attribute_First then
                        Check_Definition (Low_Bound (Rng), Error_Node => N);
                        Cprint_Node (Low_Bound (Rng));
                     else
                        Check_Definition (High_Bound (Rng), Error_Node => N);
                        Cprint_Node (High_Bound (Rng));
                     end if;
                  else
                     Unimplemented_Attribute
                       (N, Attr_Name, Node_Kind'Image (Nkind (Index)));
                  end if;
               end;
            end if;

         elsif Is_Scalar_Type (Prefix_Type) then
            if Id = Attribute_First then
               Check_Definition (Type_Low_Bound (Prefix_Type),
                 Error_Node => N);
               Cprint_Node (Type_Low_Bound (Prefix_Type));
            else
               Check_Definition (Type_High_Bound (Prefix_Type),
                 Error_Node => N);
               Cprint_Node (Type_High_Bound (Prefix_Type));
            end if;
         else
            Unimplemented_Attribute
              (N, Attr_Name, Entity_Kind'Image (Ekind (Prefix_Type)));
         end if;
      end Handle_First_Last;

   --  Start of processing for Handle_Attribute

   begin
      if Is_Raise_Statement (Attr_Prefix) then
         Handle_Raise (Attr_Prefix);
         return;
      end if;

      case Attr_Id is

         --  Access (also Address, Code_Address, Unchecked_Access,
         --  Unrestricted_Access)

         when Attribute_Access
            | Attribute_Address
            | Attribute_Code_Address
            | Attribute_Unchecked_Access
            | Attribute_Unrestricted_Access
         =>
            declare
               Typ : constant Entity_Id := Get_Full_View (Etype (N));

            begin
               --  No need to generate "&" to obtain the address of an explicit
               --  dereference since "(Prefix.all)'Address" is equivalent to
               --  "Prefix".

               if Nkind (Prefix (N)) = N_Explicit_Dereference then
                  declare
                     Typ : constant Entity_Id :=
                             Get_Full_View (Etype (Prefix (Attr_Prefix)));
                  begin
                     Cprint_Node (Prefix (Attr_Prefix));

                     if Has_Fat_Pointer (Typ) then
                        Write_Fatptr_Dereference;
                     end if;
                  end;

               --  Fat pointer

               elsif Is_Access_Type (Typ)
                 and then Has_Fat_Pointer (Typ)
               then
                  Write_Fatptr_Init (Attr_Prefix, Typ,
                    Use_Aggregate =>
                      Present (Parent (N))
                        and then Nkind (Parent (N)) = N_Component_Association);

               elsif Nkind (Attr_Prefix) in N_Has_Entity
                  and then Present (Entity (Attr_Prefix))
                  and then Present (Renamed_Object (Entity (Attr_Prefix)))
                  and then Nkind (Renamed_Object (Entity (Attr_Prefix)))
                             = N_Explicit_Dereference
               then
                  Cprint_Node (Prefix (Renamed_Object (Entity (Attr_Prefix))));

               --  Common case

               else
                  --  Add explicit cast for 'in' record parameters to disable
                  --  warning about discarding 'const'.

                  if Nkind (Attr_Prefix) = N_Identifier
                    and then Ekind (Entity (Attr_Prefix)) = E_In_Parameter
                    and then Is_Record_Type (Prefix_Type)
                  then
                     Write_Char ('(');
                     Check_Definition (Etype (Attr_Prefix), Error_Node => N);
                     Cprint_Type_Name (Etype (Attr_Prefix));
                     Write_Str (" *)");

                  --  Add a cast to System.Address to avoid mismatch between
                  --  integer and pointer. Similarly for 'Unrestricted_Access
                  --  in particular to generate compatible pointers.

                  elsif Is_Descendant_Of_Address (Typ)
                    or else Attr_Id = Attribute_Unrestricted_Access
                  then
                     Write_Str ("(system__address)");
                  end if;

                  --  A constrained array is already an address in C

                  if Attr_Id /= Attribute_Address
                    or else not Is_Array_Type (Prefix_Type)
                    or else Has_Fat_Pointer (Prefix_Type)
                  then
                     Write_Char ('&');
                  end if;

                  Cprint_Node (Attr_Prefix);
               end if;
            end;

         --  Deref

         when Attribute_Deref =>

            --  typ'Deref (expr) => (*((typ *) expr))

            Write_Str ("(*((");
            Cprint_Node (Attr_Prefix);
            Write_Str (" *)");

            if Is_AREC_Reference (N)
                 and then
                   Is_Unconstrained_Array_Type
                     (Etype (AREC_Entity (Selector_Name (Get_AREC_Field (N)))))
            then
               declare
                  AREC_Formal_Type : constant Entity_Id :=
                    Etype (AREC_Entity (Selector_Name (Get_AREC_Field (N))));

               begin
                  Write_Str (" (*(");
                  Write_Fatptr_Name (AREC_Formal_Type);
                  Write_Str ("*) ");
                  Cprint_Node (First (Expressions (N)));
                  Write_Str (")");
                  Write_Fatptr_Dereference;
                  Write_Str ("))");
               end;
            else
               Cprint_Node (First (Expressions (N)));
               Write_Str ("))");
            end if;

         --  First/Last

         when Attribute_First
            | Attribute_Last
         =>
            Handle_First_Last (Attr_Id);

         when Attribute_Length
            | Attribute_Range_Length
         =>
            Write_Char ('(');
            Handle_First_Last (Attribute_Last);
            Write_Str (" < ");
            Handle_First_Last (Attribute_First);
            Write_Str (" ? 0 : ");

            Handle_First_Last (Attribute_Last);
            Write_Str (" - ");
            Handle_First_Last (Attribute_First);
            Write_Str (" + 1)");

         --  Pos/Val

         when Attribute_Pos
            | Attribute_Val
         =>
            Write_Char ('(');
            Cprint_Node (Etype (N));
            Write_Char (')');
            Cprint_Node (First (Expressions (N)));

         --  Pred

         when Attribute_Pred =>
            Cprint_Difference
              (First (Expressions (N)), Uint_1, B => Parens_Needed (N));

         --  Succ

         when Attribute_Succ =>
            Cprint_Sum
              (First (Expressions (N)), Uint_1, Parens_Needed (N));

         --  Size/Object_Size/Value_Size/Max_Size_In_Storage_Elements

         when Attribute_Max_Size_In_Storage_Elements
            | Attribute_Object_Size
            | Attribute_Size
            | Attribute_Value_Size
         =>
            --  If this attribute is used as part of a runtime check, convert
            --  the expression explicitly to universal_integer, since the type
            --  of sizeof is size_t (an unsigned integer).

            declare
               P : Node_Id := Parent (N);
            begin
               while Present (P)
                 and then Nkind (P) not in N_Raise_xxx_Error
               loop
                  P := Parent (P);
               end loop;

               if Present (P) then
                  Write_Str ("(universal_integer)");
               end if;
            end;

            Write_Str ("sizeof(");

            if Is_Packed_Array (Prefix_Type) then
               Cprint_Node (Packed_Array_Impl_Type (Prefix_Type));
            else
               Cprint_Node (Etype (Attr_Prefix));
            end if;

            Write_Char (')');

            if Attr_Id /= Attribute_Max_Size_In_Storage_Elements then
               Write_Str (" * 8");
            end if;

         when Attribute_Machine =>
            if Comes_From_Source (N) then
               Unimplemented_Attribute (N, Attr_Name);
            else
               --  Ignore 'Machine and output the expression
               --  itself on generated code, to support e.g. ** expansion.

               Cprint_Node (First (Expressions (N)));
            end if;

         when Attribute_Valid =>
            Write_Str ("isfinite(");
            Cprint_Node (Attr_Prefix);
            Write_Char (')');

         when Attribute_Alignment =>

            --  Hard code 4 as the alignment for tagged types

            if Is_Tagged_Type (Get_Full_View (Etype (Prefix (N)))) then
               Write_Str ("4");
            else
               Unimplemented_Attribute (N, Attr_Name);
            end if;

         when Attribute_Max =>
            if Is_Unsigned_Type (Prefix_Type)
              and then not Is_Fixed_Point_Type (Prefix_Type)
            then
               declare
                  Size : constant Int := UI_To_Int (Esize (Prefix_Type));

               begin
                  case Size is
                     when 8      => Write_Str ("_umax8(");
                     when 16     => Write_Str ("_umax16(");
                     when 32     => Write_Str ("_umax32(");
                     when 64     => Write_Str ("_umax64(");
                     when others => raise Program_Error;
                  end case;

                  Cprint_Node (First (Expressions (N)));
                  Write_Char (',');
                  Cprint_Node (Next (First (Expressions (N))));
                  Write_Char (')');
               end;

            elsif Is_Integer_Type (Prefix_Type)
              or else Is_Fixed_Point_Type (Prefix_Type)
            then
               declare
                  Size : constant Int := UI_To_Int (Esize (Prefix_Type));

               begin
                  case Size is
                     when 8      => Write_Str ("_imax8(");
                     when 16     => Write_Str ("_imax16(");
                     when 32     => Write_Str ("_imax32(");
                     when 64     => Write_Str ("_imax64(");
                     when others => raise Program_Error;
                  end case;

                  Cprint_Node (First (Expressions (N)));
                  Write_Char (',');
                  Cprint_Node (Next (First (Expressions (N))));
                  Write_Char (')');
               end;

            elsif Is_Floating_Point_Type (Prefix_Type) then
               declare
                  Typ : constant Entity_Id :=
                          Matching_Standard_Type (Prefix_Type);
               begin
                  if Typ = Standard_Short_Float
                    or else Typ = Standard_Float
                  then
                     Write_Str ("fmaxf(");

                  elsif Typ = Standard_Long_Float then
                     Write_Str ("fmax(");

                  elsif Typ = Standard_Long_Long_Float then
                     Write_Str ("fmaxl(");

                  else
                     raise Program_Error;
                  end if;

                  Cprint_Node (First (Expressions (N)));
                  Write_Char (',');
                  Cprint_Node (Next (First (Expressions (N))));
                  Write_Char (')');
               end;
            else
               Unimplemented_Attribute (N, Attr_Name);
            end if;

         when Attribute_Min =>
            if Is_Unsigned_Type (Prefix_Type)
              and then not Is_Fixed_Point_Type (Prefix_Type)
            then
               declare
                  Size : constant Int := UI_To_Int (Esize (Prefix_Type));

               begin
                  case Size is
                     when 8      => Write_Str ("_umin8(");
                     when 16     => Write_Str ("_umin16(");
                     when 32     => Write_Str ("_umin32(");
                     when 64     => Write_Str ("_umin64(");
                     when others => raise Program_Error;
                  end case;

                  Cprint_Node (First (Expressions (N)));
                  Write_Char (',');
                  Cprint_Node (Next (First (Expressions (N))));
                  Write_Char (')');
               end;

            elsif Is_Integer_Type (Prefix_Type)
              or else Is_Fixed_Point_Type (Prefix_Type)
            then
               declare
                  Size : constant Int := UI_To_Int (Esize (Prefix_Type));

               begin
                  case Size is
                     when 8      => Write_Str ("_imin8(");
                     when 16     => Write_Str ("_imin16(");
                     when 32     => Write_Str ("_imin32(");
                     when 64     => Write_Str ("_imin64(");
                     when others => raise Program_Error;
                  end case;

                  Cprint_Node (First (Expressions (N)));
                  Write_Char (',');
                  Cprint_Node (Next (First (Expressions (N))));
                  Write_Char (')');
               end;

            elsif Is_Floating_Point_Type (Prefix_Type) then
               declare
                  Typ : constant Entity_Id :=
                          Matching_Standard_Type (Prefix_Type);
               begin
                  if Typ = Standard_Short_Float
                    or else Typ = Standard_Float
                  then
                     Write_Str ("fminf(");

                  elsif Typ = Standard_Long_Float then
                     Write_Str ("fmin(");

                  elsif Typ = Standard_Long_Long_Float then
                     Write_Str ("fminl(");

                  else
                     raise Program_Error;
                  end if;

                  Cprint_Node (First (Expressions (N)));
                  Write_Char (',');
                  Cprint_Node (Next (First (Expressions (N))));
                  Write_Char (')');
               end;
            else
               Unimplemented_Attribute (N, Attr_Name);
            end if;

         --  No other cases handled for now

         when Attribute_Component_Size =>
            Unimplemented_Attribute (N, Attr_Name);

         when Attribute_Rounding =>
            Unimplemented_Attribute (N, Attr_Name);

         when Attribute_Bit
            | Attribute_Bit_Position
            | Attribute_First_Bit
            | Attribute_Last_Bit
            | Attribute_Position
         =>
            Unimplemented_Attribute (N, Attr_Name);

         when Attribute_Constrained
            | Attribute_Mechanism_Code
            | Attribute_Null_Parameter
            | Attribute_Passed_By_Reference
         =>
            Unimplemented_Attribute (N, Attr_Name);

         when others =>
            Unimplemented_Attribute (N, Attr_Name);
      end case;
   end Handle_Attribute;

   ------------------
   -- Handle_Raise --
   ------------------

   procedure Handle_Raise (N : Node_Id) is

   begin
      case Nkind (N) is
         when N_Raise_Expression =>
            Write_Indent_Str (Last_Chance);

         when N_Raise_Statement  =>
            Write_Indent_Str (Last_Chance & ";");

         when N_Raise_When_Statement => --  Stub for now ???
            Write_Indent_Str (Last_Chance & ";");

         when N_Raise_xxx_Error =>
            if Present (Condition (N)) then
               if In_Compound_Statement then
                  Write_Char ('(');
                  Cprint_Node (Condition (N));
                  Write_Str (") ? " & Last_Chance & " : 0");

               else
                  Write_Indent_Str ("if (");
                  Cprint_Node (Condition (N));
                  Write_Str_Col_Check (")");
                  Indent_Begin;
                  Write_Indent_Str (Last_Chance & ";");
                  Indent_End;
               end if;

            elsif In_Compound_Statement
              or else Nkind (Parent (N)) in
                        N_Assignment_Statement | N_Object_Declaration
            then
               Write_Indent_Str (Last_Chance);
            else
               Write_Indent_Str (Last_Chance & ";");
            end if;

         when others =>
            raise Program_Error;
      end case;
   end Handle_Raise;

   --------------------
   -- Has_AREC_Itype --
   --------------------

   function Has_AREC_Itype (E : Entity_Id) return Boolean is
      ET_Element : constant Types_Table_Element_Access :=
                     Types_Table.Get (Get_Full_View (E));

   begin
      return ET_Element /= null and then ET_Element.Has_AREC_Itype;
   end Has_AREC_Itype;

   ------------------------------
   -- Has_Backend_Itype_Entity --
   ------------------------------

   function Has_Backend_Itype_Entity (E : Entity_Id) return Boolean is
      ET_Element : constant Types_Table_Element_Access :=
                     Types_Table.Get (Get_Full_View (E));

   begin
      return ET_Element /= null and then ET_Element.Has_Backend_Itype_Entity;
   end Has_Backend_Itype_Entity;

   ---------------------------
   -- Has_Object_References --
   ---------------------------

   function Has_Object_References (E : Entity_Id) return Boolean is
      function Is_Referenced (E : Entity_Id) return Boolean;
      --  Return True if E has been referenced during the first tree traversal

      -------------------
      -- Is_Referenced --
      -------------------

      function Is_Referenced (E : Entity_Id) return Boolean is
         Typ_Elmt : Objects_Table_Element_Access;
      begin
         Typ_Elmt := Objects_Table.Get (E);
         return Typ_Elmt /= null and then Present (Typ_Elmt.References);
      end Is_Referenced;

   --  Start of processing for Has_Object_References

   begin
      pragma Assert (Ekind (E) in E_Constant | E_Variable);

      --  If the tree traversal searching for types is disabled then return
      --  true to enable the unconditional output of all variable and constant
      --  declarations (see Cprint_Declare).

      if Debug_Flag_Dot_6 then
         return True;

      elsif Get_Full_View (E) /= E then
         return Is_Referenced (E) or else Is_Referenced (Get_Full_View (E));

      else
         return Is_Referenced (E);
      end if;
   end Has_Object_References;

   -------------------------
   -- Has_Type_References --
   -------------------------

   function Has_Type_References (E : Entity_Id) return Boolean is
      function Is_Referenced (E : Entity_Id) return Boolean;
      --  Return True if E has been referenced during the first tree traversal

      -------------------
      -- Is_Referenced --
      -------------------

      function Is_Referenced (E : Entity_Id) return Boolean is
         Typ_Elmt : Types_Table_Element_Access;
      begin
         Typ_Elmt := Types_Table.Get (E);
         return Typ_Elmt /= null and then Present (Typ_Elmt.References);
      end Is_Referenced;

   --  Start of processing for Has_Type_References

   begin
      pragma Assert (Is_Type (E));

      --  If the tree traversal searching for types is disabled then return
      --  true to enable the unconditional output of all type declarations
      --  (see Cprint_Declare).

      if Debug_Flag_Dot_6 then
         return True;

      elsif Get_Full_View (E) /= E then
         return Is_Referenced (E) or else Is_Referenced (Get_Full_View (E));

      else
         return Is_Referenced (E);
      end if;
   end Has_Type_References;

   -------------------------------------
   -- Has_Or_Inherits_Enum_Rep_Clause --
   -------------------------------------

   function Has_Or_Inherits_Enum_Rep_Clause (E : Entity_Id) return Boolean is
      Typ    : Entity_Id := Get_Full_View (E);
      Result : Boolean   := Has_Enumeration_Rep_Clause (Typ);

   begin
      while Get_Full_View (Etype (Typ)) /= Typ loop
         Typ    := Get_Full_View (Etype (Typ));
         Result := Result or Has_Enumeration_Rep_Clause (Typ);
      end loop;

      return Result;
   end Has_Or_Inherits_Enum_Rep_Clause;

   ------------------------
   -- Has_Same_Int_Value --
   ------------------------

   function Has_Same_Int_Value
     (Val1 : Node_Id;
      Val2 : Node_Id) return Boolean
   is
   begin
      return Compile_Time_Known_Value (Val1)
        and then Compile_Time_Known_Value (Val2)
        and then Expr_Value (Val1) = Expr_Value (Val2);
   end Has_Same_Int_Value;

   ----------
   -- Hash --
   ----------

   function Hash (N : Node_Id) return Header_Num is
   begin
      return Header_Num (1 + N mod Node_Id (Header_Num'Last));
   end Hash;

   ------------------
   -- Indent_Begin --
   ------------------

   procedure Indent_Begin is
   begin
      Indent := Indent + 2;
   end Indent_Begin;

   ----------------
   -- Indent_End --
   ----------------

   procedure Indent_End is
   begin
      Indent := Indent - 2;
   end Indent_End;

   ----------------------
   -- In_Instantiation --
   ----------------------

   function In_Instantiation (S : Source_Ptr) return Boolean is
      SI : constant Source_File_Index := Get_Source_File_Index (S);
   begin
      return Instantiation (SI) /= No_Location;
   end In_Instantiation;

   -----------------------------------
   -- Is_Access_Attribute_Reference --
   -----------------------------------

   function Is_Access_Attribute_Reference (N : Node_Id) return Boolean is
      pragma Assert (Nkind (N) = N_Attribute_Reference);

      Attr_Id : constant Attribute_Id :=
                  Get_Attribute_Id (Attribute_Name (N));

   begin
      return
        Attr_Id = Attribute_Access           or else
        Attr_Id = Attribute_Address          or else
        Attr_Id = Attribute_Unchecked_Access or else
        Attr_Id = Attribute_Unrestricted_Access;
   end Is_Access_Attribute_Reference;

   ------------------------
   -- Is_Constant_Folded --
   ------------------------

   function Is_Constant_Folded
     (E       : Entity_Id;
      In_Decl : Boolean := False) return Boolean
   is
      Can_Be_Folded : constant Boolean :=
                        Ekind (E) = E_Constant
                          and then Is_Scalar_Type (Get_Full_View (Etype (E)));

   begin
      if Back_End_Stage = Searching_Decls then

         --  In the tree traversal searching for declarations to remove we
         --  cannot fold object declarations; we only search for references

         return Can_Be_Folded
           and then not In_Decl;

      elsif In_Decl then
         return Can_Be_Folded
           and then not Has_Object_References (Get_Full_View (E));

      else
         return Can_Be_Folded;
      end if;
   end Is_Constant_Folded;

   ---------------------------------------------
   -- Is_Enum_Literal_Of_Enclosing_Subprogram --
   ---------------------------------------------

   function Is_Enum_Literal_Of_Enclosing_Subprogram
     (E : Entity_Id) return Boolean
   is
   begin
      return Ekind (E) = E_Enumeration_Literal
        and then not Is_Library_Level_Entity (E)
        and then Present (Current_Subp_Entity)
        and then not Within_Scope (E, Current_Subp_Entity);
   end Is_Enum_Literal_Of_Enclosing_Subprogram;

   -----------------------
   -- Is_Fully_Declared --
   -----------------------

   function Is_Fully_Declared (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Is_Type (E));

      for J in 1 .. Cprint_Declare_Stack.Last loop
         if Cprint_Declare_Stack.Table (J) = E then
            return False;
         end if;
      end loop;

      --  For actual subtypes of multidimensional unconstrained array formals,
      --  the back end builds an internal itype. Hence we cannot rely on the
      --  usual machinery to know if the entity is fully declared (since the
      --  entity is always declared by Declare_Back_End_Itypes after the
      --  subprogram profile and before its declarations). Therefore this
      --  routine considers them always available and the caller must use
      --  the service Has_Local_References to check if the body of the
      --  current subprogram has any reference to it.

      if Has_AREC_Itype (E) then
         return True;

      elsif Is_Private_Type (E) then
         return Entity_Table.Get (Get_Full_View (E));

      --  In the common case we just need to check if the entity has been
      --  declared.

      else
         return Entity_Table.Get (E);
      end if;
   end Is_Fully_Declared;

   -------------------------------
   -- Is_Out_Mode_Access_Formal --
   -------------------------------

   function Is_Out_Mode_Access_Formal (E : Node_Id) return Boolean is
   begin
      return Is_Formal (E)
        and then Is_Access_Type (Etype (E))
        and then Ekind (E) in E_In_Out_Parameter | E_Out_Parameter;
   end Is_Out_Mode_Access_Formal;

   ---------------------
   -- Is_Packed_Array --
   ---------------------

   function Is_Packed_Array (Typ : Entity_Id) return Boolean is
   begin
      return Is_Array_Type (Typ)
        and then Present (Packed_Array_Impl_Type (Typ));
   end Is_Packed_Array;

   -----------------------
   -- Is_Raise_Statement --
   ------------------------

   function Is_Raise_Statement (N : Node_Id) return Boolean is
   begin
      return Present (N)
        and then
          (Nkind (N) in
             N_Raise_xxx_Error | N_Raise_Expression | N_Raise_Statement);
   end Is_Raise_Statement;

   -------------------------------
   -- Is_Simple_Unchecked_Union --
   -------------------------------

   function Is_Simple_Unchecked_Union (Typ : Entity_Id) return Boolean is
   begin
      return Is_Unchecked_Union (Typ)
         and then Nkind (Declaration_Node (Typ)) = N_Full_Type_Declaration
         and then
           (No (Component_List (Type_Definition (Declaration_Node (Typ))))
             or else No (First (Component_Items (Component_List
                          (Type_Definition (Declaration_Node (Typ)))))));
   end Is_Simple_Unchecked_Union;

   ---------------------------------------
   -- Is_Supported_Variable_Size_Record --
   ---------------------------------------

   function Is_Supported_Variable_Size_Record
     (Typ : Entity_Id) return Boolean
   is
      Rng : Node_Id;

   begin
      if Is_Record_Type (Typ)
        and then Has_Discriminants (Typ)
        and then Has_Per_Object_Constraint (Last_Field (Typ))
        and then Ekind (Etype (Last_Field (Typ))) = E_Array_Subtype
      then
         Rng := First_Index (Etype (Last_Field (Typ)));

         --  We can compute the size only when the index specifies a range

         if Nkind (Rng) = N_Range then
            return True;
         end if;
      end if;

      return False;
   end Is_Supported_Variable_Size_Record;

   ---------------------------------
   -- Is_Unsigned_Or_Modular_Type --
   ---------------------------------

   function Is_Unsigned_Or_Modular_Type (Typ : Entity_Id) return Boolean is
   begin
      return Is_Unsigned_Type (Typ) or else Is_Modular_Integer_Type (Typ);
   end Is_Unsigned_Or_Modular_Type;

   -----------------
   -- Last_Chance --
   -----------------

   function Last_Chance return String is
   begin
      if Opt.Exception_Locations_Suppressed or Debug_Flag_NN then
         return "GNAT_LAST_CHANCE_HANDLER("", 0)";
      else
         return "GNAT_LAST_CHANCE_HANDLER(__FILE__, __LINE__)";
      end if;
   end Last_Chance;

   ----------------
   -- Last_Field --
   ----------------

   function Last_Field (Typ : Node_Id) return Node_Id is
      Field  : Node_Id := First_Entity (Typ);
      Result : Node_Id := Empty;

   begin
      while Present (Field) loop
         if Is_Object (Field) then
            Result := Field;
         end if;

         Next_Entity (Field);
      end loop;

      return Result;
   end Last_Field;

   ---------------
   -- Last_Line --
   ---------------

   function Last_Line (N : Node_Id) return Physical_Line_Number is
   begin
      Get_First_Last_Line (N);
      return FLCache_LL;
   end Last_Line;

   -------------------------------
   -- Non_Standard_Modular_Type --
   -------------------------------

   function Non_Standard_Modular_Type (Typ : Node_Id) return Boolean is
      M : Uint;
   begin
      if not Is_Modular_Integer_Type (Typ) then
         return False;
      else
         M := Modulus (Typ);
         return M /= Uint_2 ** 8
           and then M /= Uint_2 ** 16
           and then M /= Uint_2 ** 32
           and then M /= Uint_2 ** 64;
      end if;
   end Non_Standard_Modular_Type;

   -----------------------------
   -- Output_Anon_Struct_Name --
   -----------------------------

   procedure Output_Anon_Struct_Name (N : Node_Id) is
      Var_Part  : constant Node_Id := Parent (N);
      Var       : Node_Id;
      Var_Count : Uint := Uint_1;

   begin
      pragma Assert (Nkind (N) = N_Variant);
      pragma Assert (List_Length (Variants (Var_Part)) > 1);

      Write_Str (Anon_Struct_Prefix);

      Var := First (Variants (Var_Part));
      while Present (Var) and then Var /= N loop
         Var_Count := Var_Count + 1;
         Next (Var);
      end loop;

      Write_Uint (Var_Count);
   end Output_Anon_Struct_Name;

   ------------------------------
   -- Output_Packed_Array_Type --
   ------------------------------

   procedure Output_Packed_Array_Type (Typ : Node_Id) is
      Pack : Node_Id;
   begin
      if Is_Packed_Array (Typ) then
         Pack := Packed_Array_Impl_Type (Typ);

         if not Entity_Table.Get (Pack) then
            Cprint_Declare (Pack);
            Write_Indent;
         end if;
      end if;
   end Output_Packed_Array_Type;

   -------------------
   -- Output_Sizeof --
   -------------------

   procedure Output_Sizeof (Target : Node_Id; Source : Node_Id := Empty) is
      Need_Paren : Boolean := False;
      Source_Typ : Node_Id := Empty;
      Target_Typ : Node_Id := Get_Full_View (Etype (Target));
      Unconstr   : Boolean := False;

   begin
      if Has_Fat_Pointer (Target_Typ)
        and then Is_Access_Type (Target_Typ)
      then
         Target_Typ := Get_Full_View (Designated_Type (Target_Typ));
      end if;

      if Present (Source) then
         Source_Typ := Get_Full_View (Etype (Source));

         if Has_Fat_Pointer (Source_Typ)
           and then Is_Access_Type (Source_Typ)
         then
            Source_Typ := Get_Full_View (Designated_Type (Source_Typ));
         end if;
      end if;

      --  In general use sizeof on the type of the expression, unless the type
      --  has not been output yet, in which case use the expression itself: LHS
      --  by default (ie. Target), except in the case of a subprogram parameter
      --  where we take the RHS (ie. Source).

      if Entity_Table.Get (Target_Typ)
        and then (not Is_Array_Type (Target_Typ)
                   or else Is_Constrained (Target_Typ))
      then
         Write_Str ("sizeof(");
         Cprint_Type_Name (Target_Typ);
         Need_Paren := True;

      elsif Present (Source)
        and then Entity_Table.Get (Source_Typ)
        and then (not Is_Array_Type (Source_Typ)
                   or else Is_Constrained (Source_Typ))
      then
         Write_Str ("sizeof(");
         Cprint_Type_Name (Source_Typ);
         Need_Paren := True;

      elsif Present (Source)
         and then (Nkind (Source) /= N_Identifier
                    or else not Is_Formal (Entity (Source)))
      then
         if Nkind (Source) = N_String_Literal then
            Write_Int (String_Length (Strval (Source)));
         else
            Write_Str ("sizeof(");
            Cprint_Node (Source, Declaration => True);
            Need_Paren := True;
         end if;

      elsif Is_Unconstrained_Array_Type (Target_Typ) then
         Write_Str ("sizeof(");
         Write_Id (Component_Type (Target_Typ));
         Unconstr := True;
         Need_Paren := True;

      else
         if Nkind (Target) = N_String_Literal then
            Write_Int (String_Length (Strval (Target)));
         else
            Write_Str ("sizeof(");
            Cprint_Node (Target, Declaration => True);
            Need_Paren := True;
         end if;
      end if;

      if Need_Paren then
         Write_Char (')');
      end if;

      if Unconstr then
         Write_Str_Col_Check (" * ");
         Write_Number_Of_Components (Target, Target_Typ);
      end if;
   end Output_Sizeof;

   -------------------
   -- Parens_Needed --
   -------------------

   function Parens_Needed (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);
   begin
      if Nkind (P) = N_Assignment_Statement then
         return N /= Expression (P);
      else
         return True;
      end if;
   end Parens_Needed;

   ------------------
   -- Pass_Pointer --
   ------------------

   function Pass_Pointer (Ent : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Get_Full_View (Etype (Ent));

   begin
      if Is_Array_Type (Typ) then
         return False;

      --  Pass records by reference when using the default mechanism, otherwise
      --  this will cause an inefficient pass C struct by copy which is not
      --  what users expect by default.

      elsif Is_Record_Type (Typ)
        and then Mechanism (Ent) = Default_Mechanism
      then
         return True;

      elsif Ekind (Ent) in E_In_Out_Parameter | E_Out_Parameter then
         return True;

      --  Pass "flexible arrays" (arrays whose size is determined by a
      --  discriminant) by reference.

      elsif Has_Discriminants (Typ)
        and then Ekind (Etype (Last_Field (Typ))) = E_Array_Subtype
      then
         return True;
      else
         return Mechanism (Ent) = By_Reference;
      end if;
   end Pass_Pointer;

   ---------------------
   -- Register_Entity --
   ---------------------

   procedure Register_Entity (E : Entity_Id) is
   begin
      pragma Assert (Nkind (E) in N_Entity);
      Entity_Table.Set (E, True);
      Enclosing_Subp_Table.Set (E, Current_Subp_Entity);
      Register_Types_Table_Entity (E);
      Register_Objects_Table_Entity (E);
   end Register_Entity;

   -------------------------------
   -- Register_Object_Reference --
   -------------------------------

   procedure Register_Object_Reference (E : Entity_Id) is
      Value : Objects_Table_Element_Access;

   begin
      --  Collect all the object references in the first tree traversal

      if Ekind (E) in E_Constant | E_Variable
        and then not In_Comment
        and then Back_End_Stage = Searching_Decls
      then
         --  The key of the Types hash table is always the full view

         Value := Objects_Table.Get (Get_Full_View (E));

         --  Type declarations must be seen before their references

         if Value /= null then
            declare
               Ref : Node_Id := Empty;

            begin
               --  Locate the inner node referencing this type

               for J in reverse 1 .. Cprint_Node_Stack.Last loop
                  Ref := Cprint_Node_Stack.Table (J);
                  exit when Ref /= E;
               end loop;

               pragma Assert (Present (Ref));

               --  Do not count as a reference the object declaration

               if Ref /= Parent (E) then
                  if No (Value.References) then
                     Value.References := New_Elmt_List;
                  end if;

                  Append_Elmt (Ref, To => Value.References);
               end if;
            end;
         end if;
      end if;
   end Register_Object_Reference;

   -----------------------------------
   -- Register_Objects_Table_Entity --
   -----------------------------------

   procedure Register_Objects_Table_Entity (E : Entity_Id) is
   begin
      if Back_End_Stage = Searching_Decls
        and then Ekind (E) in E_Constant | E_Variable
        and then Objects_Table.Get (Get_Full_View (E)) = null
      then
         --  The key of the Objects hash table is always the full view

         Objects_Table.Set (Get_Full_View (E), new Objects_Table_Element);
      end if;
   end Register_Objects_Table_Entity;

   ------------------------------
   -- Register_Type_Reference --
   ------------------------------

   procedure Register_Type_Reference (E : Entity_Id) is
      Value : Types_Table_Element_Access;

   begin
      if Is_Type (E)
        and then Is_Fully_Declared (E)
        and then not In_Comment
      then
         if Back_End_Stage = Removing_Decls then
            Value := Types_Table.Get (Get_Full_View (E));
            Value.References_Count := Value.References_Count + 1;

         elsif Back_End_Stage = Searching_Decls then
            declare
               Ref : Node_Id := Empty;

            begin
               --  The key of the Types hash table is always the full view

               Value := Types_Table.Get (Get_Full_View (E));

               --  Type declarations must be seen before their references

               pragma Assert (Value /= null);

               if No (Value.References) then
                  Value.References := New_Elmt_List;
               end if;

               --  Locate the inner node referencing this type

               for J in reverse 1 .. Cprint_Node_Stack.Last loop
                  Ref := Cprint_Node_Stack.Table (J);
                  exit when Ref /= E;
               end loop;

               pragma Assert (Present (Ref));
               Append_Elmt (Ref, To => Value.References);
            end;
         end if;
      end if;
   end Register_Type_Reference;

   ---------------------------------
   -- Register_Types_Table_Entity --
   ---------------------------------

   procedure Register_Types_Table_Entity (E : Entity_Id) is
   begin
      --  In the general case at this stage the Types_Table element is not yet
      --  in the Types_Table and we need to create a new element. However, for
      --  entities that require the generation of extra back-end itypes, the
      --  element may have been already created and added to the table (see
      --  Declare_Itype).

      if Back_End_Stage = Searching_Decls
        and then Is_Type (E)
        and then Types_Table.Get (Get_Full_View (E)) = null
      then
         --  The key of the Types hash table is always the full view

         Types_Table.Set (Get_Full_View (E), new Types_Table_Element);
      end if;
   end Register_Types_Table_Entity;

   ----------------------
   -- Requires_Address --
   ----------------------

   function Requires_Address (Typ : Node_Id) return Boolean is
   begin
      return
        not Is_Array_Type (Typ)
          or else (Is_Packed_Array (Typ)
                    and then Is_Integer_Type (Packed_Array_Impl_Type (Typ)));
   end Requires_Address;

   -----------------
   -- Source_Dump --
   -----------------

   procedure Source_Dump is
      function Count_Skipped_Type_Decls return Nat;
      --  Return the number of elements of the Types_Table that are marked as
      --  skipped declaration.

      procedure Cprint_Library_Item (U : Node_Id);
      --  Print C code for unit U

      function File_To_Define (File : String) return String;
      --  Return a C define name from a given filename File

      procedure Search_For_Types;
      --  Perform a tree traversal searching for type declarations and their
      --  references.

      procedure Update_Skipped_Type_Decls;
      --  Traverse the contents of the Types_Table searching for unreferenced
      --  elements to mark them as skipped in the next tree traversal.

      ------------------------------
      -- Count_Skipped_Type_Decls --
      ------------------------------

      function Count_Skipped_Type_Decls return Nat is
         Count : Nat := 0;
         Key   : Node_Id := Empty;
         Value : Types_Table_Element_Access := null;

      begin
         Types_Table.Get_First (Key, Value);
         while Value /= null loop
            if Value.Declaration_Skipped then
               Count := Count + 1;
            end if;

            Types_Table.Get_Next (Key, Value);
         end loop;

         return Count;
      end Count_Skipped_Type_Decls;

      -------------------------
      -- Cprint_Library_Item --
      -------------------------

      procedure Cprint_Library_Item (U : Node_Id) is
         procedure Gen_Define_Source_File;
         --  Define macro associated with the current source file

         procedure Gen_End_Define_Source_File;
         --  Close the definition of the macro associated with the current
         --  source file

         ----------------------------
         -- Gen_Define_Source_File --
         ----------------------------

         procedure Gen_Define_Source_File is
            Define : constant String :=
                       File_To_Define
                         (Get_Name_String (File_Name (Current_Source_File)));

         begin
            Write_Str ("#ifndef ");
            Write_Str (Define);
            Write_Eol;
            Write_Str ("#define ");
            Write_Str (Define);
            Write_Eol;
         end Gen_Define_Source_File;

         --------------------------------
         -- Gen_End_Define_Source_File --
         --------------------------------

         procedure Gen_End_Define_Source_File is
            Define : constant String :=
                       File_To_Define
                         (Get_Name_String (File_Name (Current_Source_File)));

         begin
            Write_Str ("#endif /* ");
            Write_Str (Define);
            Write_Str (" */");
            Write_Eol;
         end Gen_End_Define_Source_File;

         --  Local variables

         Current_Unit     : Unit_Number_Type;
         Compilation_Unit : Node_Id;

      --  Start of processing for Cprint_Library_Item

      begin
         --  Ignore Standard and ASCII packages

         if Sloc (U) <= Standard_Location then
            return;
         end if;

         Compilation_Unit := Parent (U);
         Current_Unit := Get_Cunit_Unit_Number (Compilation_Unit);
         Current_Source_File := Source_Index (Current_Unit);
         Next_Line_To_Print := 1;

         --  For library level subprogram bodies that act as their own spec
         --  generate their declaration in the .h file. Needed to avoid the
         --  C warning on missing prototype.

         if Nkind (U) = N_Subprogram_Body and then Acts_As_Spec (U) then
            Open_Scope (With_Block => False);
            Gen_Define_Source_File;
            Cprint_Node (Specification (U));
            Write_Char (';');
            Write_Eol;
            Gen_End_Define_Source_File;
            Close_Scope;
         end if;

         if Full_Code_Generation then
            In_Main_Unit := In_Extended_Main_Code_Unit (U);

            if Current_Unit = Main_Unit
              and then Back_End_Stage = Generating_Output
            then
               --  Independently of the value of -gnatd.Y the next outputs are
               --  generated for the body file.

               pragma Assert (In_Header_File);
               In_Header_File := False;

               if not Debug_Flag_Dot_YY then
                  Close_H_File;
                  Create_C_File;
                  Set_Output (Output_FD);
               end if;

               Set_File_Name ("h");

               Define_In_Ada_Body;

               Write_Str ("#include """);
               Write_Str (Name_Buffer (1 .. Name_Len - 1));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Has_No_Elaboration_Code is supposed to be set by default
            --  on subprogram bodies, but this is apparently not the case,
            --  so force the flag here. Ditto for subprogram decls.

            if In_Main_Unit
              and then Nkind (U) in
                         N_Subprogram_Body | N_Subprogram_Declaration
            then
               Set_Has_No_Elaboration_Code (Parent (U), True);
            end if;

         elsif Nkind (U) in N_Subprogram_Body | N_Package_Body then
            return;
         end if;

         Write_Eol;

         if Current_Unit /= Main_Unit then
            Gen_Define_Source_File;
         end if;

         --  Open the new scope associated with this unit to be ready to
         --  process its declarations (see Open_Scope). No explicit block is
         --  associated with this scope because for library level declarations
         --  it must not be generated.

         declare
            Scope_Id : Nat;

         begin
            Open_Scope (With_Block => False);
            Scope_Id := Current_Scope_Id;

            In_Library_Unit_Decl :=
              Nkind (U) in N_Package_Declaration | N_Subprogram_Declaration;
            In_Library_Unit_Pkg_Decl := Nkind (U) = N_Package_Declaration;

            --  Handle Aux_Decls_Node

            Cprint_Opt_Node_List
              (Declarations (Aux_Decls_Node (Compilation_Unit)));

            --  Output C text to file

            Cprint_Node (U);

            --  Finish handling Aux_Decls_Node

            Cprint_Opt_Node_List (Actions (Aux_Decls_Node (Compilation_Unit)));
            Cprint_Opt_Node_List
              (Pragmas_After (Aux_Decls_Node (Compilation_Unit)));

            Check_No_Delayed_Itype_Decls;

            --  Close this scope and all its inner scopes

            In_Library_Unit_Pkg_Decl := False;
            In_Library_Unit_Decl := False;
            Close_Scope (Scope_Id);
         end;

         --  Ensure of terminating EOL

         Write_Eol;

         if Current_Unit /= Main_Unit then
            Gen_End_Define_Source_File;
         end if;
      end Cprint_Library_Item;

      --------------------
      -- File_To_Define --
      --------------------

      function File_To_Define (File : String) return String is
         Result : String (File'Range);
      begin
         for J in File'Range loop
            case File (J) is
               when 'A' .. 'Z' | '0' .. '9' | '_' =>
                  Result (J) := File (J);
               when 'a' .. 'z' =>
                  Result (J) := Fold_Upper (File (J));
               when others =>
                  Result (J) := '_';
            end case;
         end loop;

         return Result;
      end File_To_Define;

      --------------------
      -- Walk_All_Units --
      --------------------

      procedure Walk_All_Units is
        new Sem.Walk_Library_Items (Action => Cprint_Library_Item);

      ----------------------
      -- Search_For_Types --
      ----------------------

      procedure Search_For_Types is
      begin
         Set_Special_Output (Ignore_Output'Access);

         --  Lock nodes to ensure that the first tree traversal does not modify
         --  the tree contents. We cannot lock Elists since they are used to
         --  register itype references in their hash table; we cannot lock
         --  Nlists since the tree traversal performed by Walk_Library_Items
         --  adds to the Context_Items lists of the specs the context of the
         --  instance body (see Sem.Walk_Library_Items).

         Lock_Nodes;
         Walk_All_Units;
         Unlock_Nodes;

         Cancel_Special_Output;

         --  Reset all the tables (not including the Types_Table since it is
         --  needed for the second tree traversal)

         Back_End_Scopes_Stack.Reset;
         Cprint_Declare_Stack.Init;
         Cprint_Node_Stack.Init;
         Enclosing_Subp_Table.Reset;
         Entity_Table.Reset;
         Elaboration_Table.Init;
         Macro_Table.Init;
      end Search_For_Types;

      -------------------------------
      -- Update_Skipped_Type_Decls --
      -------------------------------

      procedure Update_Skipped_Type_Decls is
         Key   : Node_Id := Empty;
         Value : Types_Table_Element_Access := null;

      begin
         pragma Assert (Back_End_Stage = Removing_Decls);

         Types_Table.Get_First (Key, Value);
         while Value /= null loop
            if Value.References_Count = 0 then
               Value.Declaration_Skipped := True;
            end if;

            --  Reset values for the next tree traversal

            Value.References_Count := 0;
            Value.Enum_Literals_Declared := False;

            Types_Table.Get_Next (Key, Value);
         end loop;
      end Update_Skipped_Type_Decls;

   --  Start of processing for Source_Dump

   begin
      --  Bump line length limit to avoid too many line drift when using -g
      --  to correlate Ada and C code.

      Sprint_Line_Limit := 120;

      --  Initialize constants for Write_Uint

      LNegInt  := -(Uint_2 ** (ints - 1));
      LPosInt  := abs (LNegInt + 1);
      LNegLong := -(Uint_2 ** (longs - 1));
      LPosLong := abs (LNegLong + 1);
      LNegLL   := -(Uint_2 ** (lls - 1));
      LPosLL   := abs (LNegLL + 1);

      LPosU    := (Uint_2 ** ints) - 1;
      LNegU    := -LPosU;
      LPosUL   := (Uint_2 ** longs) - 1;
      LNegUL   := -LPosUL;
      LPosULL  := (Uint_2 ** lls) - 1;
      LNegULL  := -LPosULL;

      --  Dump C file

      Current_Source_File := Main_Source_File;

      --  Include content of "standard.h" to file

      declare
         FD   : File_Descriptor;
         Hi   : Source_Ptr;
         Lo   : Source_Ptr;
         Text : Source_Buffer_Ptr;

      begin
         Name_Len := 10;
         Name_Buffer (1 .. Name_Len) := "standard.h";
         Read_Source_File (Name_Find, 0, Hi, Text, FD);

         --  Enable Full_Code_Generation when standard.h is found

         if not Null_Source_Buffer_Ptr (Text) then
            Full_Code_Generation := True;
         else
            --  Otherwise defaults to standard.ads.h for generation of headers

            Full_Code_Generation := False;
            Name_Len := 14;
            Name_Buffer (1 .. Name_Len) := "standard.ads.h";
            Read_Source_File (Name_Find, 0, Hi, Text, FD);

            if Null_Source_Buffer_Ptr (Text) then
               Write_Line
                 ("fatal error, run-time library not installed correctly");

               if FD = Null_FD then
                  Write_Line ("cannot locate file standard.ads.h");
               else
                  Write_Line ("no read access for file standard.ads.h");
               end if;

               raise Unrecoverable_Error;
            end if;
         end if;

         --  If minimizing type declarations is enabled then perform several
         --  tree traversal searching for types referenced by the generated C

         if not Debug_Flag_Dot_6 then
            declare
               Current_Skipped_Decls  : Nat := 0;
               Previous_Skipped_Decls : Nat := 0;

            begin
               Back_End_Stage := Searching_Decls;
               Search_For_Types;
               Tree_Traversals_Counter := 1;

               loop
                  Back_End_Stage := Removing_Decls;

                  Search_For_Types;
                  Tree_Traversals_Counter := Tree_Traversals_Counter + 1;

                  Update_Skipped_Type_Decls;
                  Current_Skipped_Decls := Count_Skipped_Type_Decls;

                  --  Stop when the output is stable

                  pragma Assert
                    (Current_Skipped_Decls >= Previous_Skipped_Decls);
                  exit when Current_Skipped_Decls = Previous_Skipped_Decls;

                  Previous_Skipped_Decls := Current_Skipped_Decls;
               end loop;
            end;
         end if;

         Back_End_Stage := Generating_Output;

         --  Further output will be done in the C file, unless -gnatd.Y is set
         --  in which case output goes to stdout, for debugging purposes.

         if not Debug_Flag_Dot_YY then
            Create_H_File;
            Set_Output (Output_FD);
         end if;

         --  Independently of the value of -gnatd.Y the following output
         --  is generated for the header file.

         pragma Assert (not In_Header_File);
         In_Header_File := True;

         Debug_Write_Type_References;

         if Debugger_Level > 0 then
            if Full_Code_Generation then
               Write_Str ("#line 1 ""standard.h""");
            else
               Write_Str ("#line 1 ""standard.ads.h""");
            end if;

            Write_Eol;
         end if;

         Lo := 0;

         --  Remove header in generated code

         if Text'Length >= 2 and then Text (0 .. 1) = "/*" then
            for J in 2 .. Hi loop
               if Text (J) = '/' and Text (J - 1) = '*' then
                  Lo := J + 1;

                  while Text (Lo) = ASCII.LF or Text (Lo) = ASCII.CR loop
                     Lo := Lo + 1;
                  end loop;

                  exit;
               end if;
            end loop;
         end if;

         for J in Lo .. Hi - 1 loop
            Write_Char (Text (J));
         end loop;
      end;

      --  Dump all units to generate a self contained C file

      Walk_All_Units;

      if In_Header_File then
         In_Header_File := False;
      end if;

      --  Close the C file

      if not Debug_Flag_Dot_YY then
         if Full_Code_Generation then
            Close_C_File;
         else
            Close_H_File;
         end if;

         Set_Standard_Output;

         --  Eliminate duplicated errors and remove deleted warnings

         Errout.Finalize (Last_Call => False);

         --  Delete .c and .h files in case of errors generated during code
         --  generation, unless -gnatd.4 is set.

         if Compilation_Errors and not Debug_Flag_Dot_4 then
            Delete_C_File;
            Delete_H_File;
         end if;
      end if;
   end Source_Dump;

   -----------------------------
   -- Unimplemented_Attribute --
   -----------------------------

   procedure Unimplemented_Attribute
     (N       : Node_Id;
      Attr    : Name_Id;
      Context : String := "")
   is
      Name : constant String := Get_Name_String (Attr);

   begin
      Error_Msg_Name_1 := Attr;

      if Context = "" then
         Error_Msg_N ("unsupported attribute%", N);
      else
         Error_Msg_Strlen := Context'Length;
         Error_Msg_String (1 .. Error_Msg_Strlen) := Context;
         Error_Msg_N ("unsupported attribute% in this context (~)", N);
      end if;

      Write_Str ("/* unsupported attribute: " & Name & " */");
   end Unimplemented_Attribute;

   -----------------------
   -- Write_Array_Bound --
   -----------------------

   procedure Write_Array_Bound
     (Expr      : Node_Id;
      Bound     : Bound_Kind;
      Dimension : Pos)
   is
      procedure Write_Bound (Array_Node : Node_Id);
      --  Output the Bound of the given Dimension of Array_Node

      -----------------
      -- Write_Bound --
      -----------------

      procedure Write_Bound (Array_Node : Node_Id) is
         procedure Write_Fatptr_Bounds (Node : Node_Id);
         --  Output the Bound of the given Dimension of a fat pointer

         procedure Write_Range_Bounds (Rng : Node_Id);
         --  Output the Bound of the given Dimension of a range expression

         procedure Write_Type_Bounds (Typ : Entity_Id);
         --  Output the Bound of the given Dimension of an array type

         -------------------------
         -- Write_Fatptr_Bounds --
         -------------------------

         procedure Write_Fatptr_Bounds (Node : Node_Id) is
            Typ : Entity_Id := Get_Full_View (Etype (Array_Node));
         begin
            if Is_Access_Type (Typ) then
               Typ := Get_Full_View (Designated_Type (Typ));
            end if;

            Cprint_Node (Node);
            Write_Char ('.');

            if Bound = Low then
               Write_Fatptr_First (Typ, Dimension);
            else
               Write_Fatptr_Last (Typ, Dimension);
            end if;
         end Write_Fatptr_Bounds;

         ------------------------
         -- Write_Range_Bounds --
         ------------------------

         procedure Write_Range_Bounds (Rng : Node_Id) is
            pragma Assert (Nkind (Rng) = N_Range);
         begin
            if Bound = Low then
               Cprint_Node (Low_Bound (Rng));
            else
               Cprint_Node (High_Bound (Rng));
            end if;
         end Write_Range_Bounds;

         -----------------------
         -- Write_Type_Bounds --
         -----------------------

         procedure Write_Type_Bounds (Typ : Entity_Id) is
            Ind : Node_Id := First_Index (Typ);

         begin
            for J in 2 .. Dimension loop
               Next_Index (Ind);
            end loop;

            if Bound = Low then
               Cprint_Node (Type_Low_Bound (Etype (Ind)));
            else
               Cprint_Node (Type_High_Bound (Etype (Ind)));
            end if;
         end Write_Type_Bounds;

         --  Local variables

         Expr_Type : Entity_Id := Get_Full_View (Etype (Array_Node));

      --  Start of processing for Write_Bound

      begin
         if Is_Access_Type (Expr_Type) then
            Expr_Type := Get_Full_View (Designated_Type (Expr_Type));
         end if;

         --  Annoying special case of string literal

         if Ekind (Expr_Type) = E_String_Literal_Subtype then
            if Bound = Low then
               Write_Uint
                 (Intval (String_Literal_Low_Bound (Expr_Type)));
            else
               Write_Uint
                 (String_Literal_Length (Expr_Type) -
                   Intval (String_Literal_Low_Bound (Expr_Type)) + 1);
            end if;

            return;
         end if;

         if Nkind (Array_Node) in N_Has_Entity
           and then Present (Entity (Array_Node))
         then
            declare
               E   : constant Entity_Id := Entity (Array_Node);
               Typ : constant Entity_Id := Get_Full_View (Etype (E));

            begin
               if Ekind (E) = E_Variable then

                  --  Retrieve the bounds from the fat pointer

                  if Is_Access_Type (Typ) then

                     --  Retrieve the bounds from the fat pointer

                     if not Is_Constrained (Designated_Type (Typ)) then
                        Write_Fatptr_Bounds (Array_Node);
                     else
                        Write_Type_Bounds (Designated_Type (Typ));
                     end if;

                  else
                     Write_Type_Bounds (Typ);
                  end if;

               elsif Is_Formal (E)
                 and then not Is_Constrained (Typ)
               then
                  Write_Fatptr_Bounds (Array_Node);

               else
                  Write_Type_Bounds (Expr_Type);
               end if;
            end;

         else
            case Nkind (Array_Node) is
               when N_Slice =>
                  declare
                     Rng : constant Node_Id := Discrete_Range (Array_Node);

                  begin
                     if Nkind (Rng) = N_Range then
                        Write_Range_Bounds (Rng);
                     else
                        Write_Type_Bounds (Etype (Rng));
                     end if;
                  end;

               when N_Null =>

                  --  The bounds of null are 0 when initializing fat pointers

                  Write_Char ('0');

               when N_Qualified_Expression
                  | N_Selected_Component
               =>
                  if not Is_Constrained (Expr_Type) then
                     Write_Fatptr_Bounds (Array_Node);
                  else
                     Write_Type_Bounds (Expr_Type);
                  end if;

               when others =>

                  --  Get index subtype bounds

                  Write_Type_Bounds (Expr_Type);
            end case;
         end if;
      end Write_Bound;

      --  Local variables

      Expr_Type  : constant Entity_Id := Get_Full_View (Etype (Expr));
      Array_Node : Node_Id := Expr;
      Array_Type : Entity_Id;

   --  Start of processing for Write_Array_Bound

   begin
      if Is_Access_Type (Expr_Type) then
         Array_Type := Get_Full_View (Designated_Type (Expr_Type));
      else
         Array_Type := Expr_Type;
      end if;

      pragma Assert (Is_Array_Type (Array_Type));

      if not Is_Constrained (Array_Type) then
         case Nkind (Array_Node) is
            when N_Attribute_Reference =>
               declare
                  Attr_Name   : constant Name_Id := Attribute_Name (Expr);
                  Attr_Id     : constant Attribute_Id :=
                                  Get_Attribute_Id (Attr_Name);
                  Attr_Prefix : constant Node_Id := Prefix (Expr);
                  Prefix_Type : constant Entity_Id :=
                                  Get_Full_View (Etype (Attr_Prefix));
               begin
                  pragma Assert
                    (Attr_Id = Attribute_Access
                      or else Attr_Id = Attribute_Unchecked_Access
                      or else Attr_Id = Attribute_Unrestricted_Access);
                  pragma Assert (Is_Array_Type (Prefix_Type));

                  Array_Node := Attr_Prefix;
               end;

            when N_Type_Conversion =>
               Array_Node := Expression (Array_Node);

            when N_Identifier
               | N_Null
               | N_Selected_Component
            =>
               null;

            when N_Allocator =>
               Array_Node := Expression (Array_Node);

               if Nkind (Array_Node) = N_Qualified_Expression then
                  Array_Node := Expression (Array_Node);
               end if;

            --  Play it safe and generate an error for other cases we haven't
            --  tested.

            when others =>
               declare
                  S : constant String := Node_Kind'Image (Nkind (Array_Node));
               begin
                  Error_Msg_Strlen := S'Length;
                  Error_Msg_String (1 .. Error_Msg_Strlen) := S;
                  Error_Msg_N
                    ("unsupported access to unconstrained array (~)",
                     Array_Node);
               end;
         end case;
      end if;

      Write_Bound (Array_Node);
   end Write_Array_Bound;

   -----------------------
   -- Write_C_Char_Code --
   -----------------------

   Hex : constant array (Char_Code range 0 .. 15) of Character :=
     "0123456789abcdef";

   procedure Write_C_Char_Code (CC : Char_Code) is
      C : Character;
   begin
      --  For now, output wide characters simply as ?

      if CC > 255 then
         Write_Char ('?');
         return;
      end if;

      C := Character'Val (CC);

      --  Remaining characters in range 0 .. 255, output with most appropriate
      --  C (escape) sequence.

      case C is
         when ASCII.BS =>
            Write_Str ("\b");

         when ASCII.FF =>
            Write_Str ("\f");

         when ASCII.LF =>
            Write_Str ("\n");

         when ASCII.CR =>
            Write_Str ("\r");

         when ASCII.HT =>
            Write_Str ("\t");

         when ASCII.VT =>
            Write_Str ("\v");

         when ' ' .. '~' =>
            if C = '\' or C = '"' or C = ''' then
               Write_Char ('\');
            end if;

            Write_Char (C);

         when others =>
            Write_Str ("\x");
            Write_Char (Hex (CC / 16));
            Write_Char (Hex (CC mod 16));
      end case;
   end Write_C_Char_Code;

   --------------
   -- Write_Id --
   --------------

   procedure Write_Id (N : Node_Id) is
      function Is_C_Keyword (Name : Name_Id) return Boolean;
      --  Return True if Name is a C keyword

      function Is_Qualified (Name : Name_Id) return Boolean;
      --  Return True if Name is already fully qualified

      ------------------
      -- Is_C_Keyword --
      ------------------

      function Is_C_Keyword (Name : Name_Id) return Boolean is
      begin
         Get_Name_String (Name);

         for J in 1 .. Name_Len loop
            Name_Buffer (J) := Fold_Lower (Name_Buffer (J));
         end loop;

         declare
            Str_Name : String renames Name_Buffer (1 .. Name_Len);
         begin
            --  No need to check C keywords which are also Ada reserved words
            --  since (if present) they were rejected by the Ada front end.
            --  Those keywords are: case do else for goto if return while.

            return Str_Name = "auto"
              or else Str_Name = "bool"
              or else Str_Name = "break"
              or else Str_Name = "char"
              or else Str_Name = "const"
              or else Str_Name = "continue"
              or else Str_Name = "default"
              or else Str_Name = "double"
              or else Str_Name = "enum"
              or else Str_Name = "extern"
              or else Str_Name = "float"
              or else Str_Name = "int"
              or else Str_Name = "long"
              or else Str_Name = "register"
              or else Str_Name = "short"
              or else Str_Name = "signed"
              or else Str_Name = "sizeof"
              or else Str_Name = "static"
              or else Str_Name = "struct"
              or else Str_Name = "switch"
              or else Str_Name = "typedef"
              or else Str_Name = "union"
              or else Str_Name = "unsigned"
              or else Str_Name = "void"
              or else Str_Name = "volatile";
         end;
      end Is_C_Keyword;

      ------------------
      -- Is_Qualified --
      ------------------

      function Is_Qualified (Name : Name_Id) return Boolean is
      begin
         Get_Name_String (Name);

         --  Names starting with an upper-case letter are not qualified

         if Name_Buffer (1) in 'A' .. 'Z' then
            return False;

         else
            --  Names containing __ are qualified, others aren't

            for J in 2 .. Name_Len loop
               if Name_Buffer (J) = '_' and then Name_Buffer (J - 1) = '_' then
                  return True;
               end if;
            end loop;

            return False;
         end if;
      end Is_Qualified;

   --  Start of processing for Write_Id

   begin
      --  Case of a defining identifier

      if Nkind (N) = N_Defining_Identifier then

         if Is_Type (N) then
            Register_Type_Reference (N);

         elsif Ekind (N) in E_Constant | E_Variable then
            Register_Object_Reference (N);
         end if;

         --  Itypes defined in package specs are propagated to the units
         --  depending on them through with clauses and do not always have
         --  a fully expanded name. This looks like a bug in the front end,
         --  which we workaround here for now???

         if Is_Itype (N) then

            --  Minimize cases where we add a prefix explicitly, to avoid
            --  generating pkg__pkg__Txxs instead of pkg__Txxs when the
            --  name has already been expanded.

            if not Is_Qualified (Chars (N)) then
               Write_Name (Chars (Enclosing_Package_Or_Subprogram (N)));
               Write_Str ("__");
            end if;

            Write_Name (Chars (N));

         --  If defining identifier has an interface name (and no address
         --  clause), then we output the interface name.

         elsif (Is_Imported (N) or else Is_Exported (N))
           and then Present (Interface_Name (N))
           and then No (Address_Clause (N))
         then
            String_To_Name_Buffer (Strval (Interface_Name (N)));
            Write_Str (Name_Buffer (1 .. Name_Len));

         --  Handle renamings of enumeration literals

         elsif Ekind (N) = E_Enumeration_Literal then
            Write_Name (Chars (Ultimate_Alias (N)));

         --  Change names that match C keywords except when the reference
         --  an entity defined in Standard (i.e. Float or Unsigned) since
         --  they correspond exactly with the C types with such name.

         elsif Scope (N) /= Standard_Standard
           and then Is_C_Keyword (Chars (N))
         then
            Write_Name (Chars (N));
            Write_Str ("_");

         --  If no interface name (or inactive because there was an address
         --  clause), then just output the Chars name.

         else
            Write_Name (Chars (N));
         end if;

      --  Case of selector of an expanded name where the expanded name has
      --  an associated entity, output this entity. Check that the entity
      --  or associated node is of the right kind, see above.

      elsif Nkind (Parent (N)) = N_Expanded_Name
        and then Selector_Name (Parent (N)) = N
        and then Present (Entity_Or_Associated_Node (Parent (N)))
        and then Nkind (Entity (Parent (N))) in N_Entity
      then
         Write_Id (Entity (Parent (N)));

      --  For enumeration literal, print representation value

      elsif Nkind (N) in N_Has_Entity
        and then Present (Entity (N))
        and then Ekind (Entity (N)) = E_Enumeration_Literal
      then
         Write_Uint (Enumeration_Rep (Entity (N)), Column_Check => False);

      --  For any other node with an associated entity, output entity name

      elsif Nkind (N) in N_Has_Entity
        and then Present (Entity_Or_Associated_Node (N))
        and then Nkind (Entity_Or_Associated_Node (N)) in N_Entity
      then
         if In_Search_Type_Ref
           and then Nkind (N) = N_Identifier
           and then Present (Associated_Node (N))
         then
            Check_Definition (Entity (N));
         end if;

         if Is_Private_Type (Entity (N)) then
            Write_Id (Get_Full_View (Entity (N)));
         else
            Write_Id (Entity (N));
         end if;

      --  All other cases, we just print the Chars field

      else
         Write_Name (Chars (N));
      end if;
   end Write_Id;

   ------------------
   -- Write_Indent --
   ------------------

   procedure Write_Indent is
   begin
      if Column > 1 then
         Write_Eol;
      end if;

      for J in 1 .. Indent loop
         Write_Char (' ');
      end loop;
   end Write_Indent;

   ----------------------
   -- Write_Indent_Str --
   ----------------------

   procedure Write_Indent_Str (S : String) is
   begin
      Write_Indent;
      Write_Str (S);
   end Write_Indent_Str;

   ------------------------
   -- Write_Integer_Type --
   ------------------------

   procedure Write_Integer_Type
     (Siz : Int; Signed : Boolean; Typ : Entity_Id := Empty)
   is
      Done : Boolean := False;

      procedure Dump_Type (S : String);
      --  Dump the given type string and set Done to True

      ---------------
      -- Dump_Type --
      ---------------

      procedure Dump_Type (S : String) is
      begin
         Write_Str_Col_Check (S);
         Done := True;
      end Dump_Type;

   begin
      if Present (Typ)
        and then Get_Name_String (Chars (Scope (Typ))) = "interfaces__c"
      then
         --  Map some types in Interfaces.C directly to correspond C types.
         --  This is important in particular since "int" and "long" are
         --  incompatible types, so we want e.g. "unsigned_long" to map to
         --  "unsigned long" and not to "unsigned_32".

         declare
            Name : constant String := Get_Name_String (Chars (Typ));
         begin
            if Name = "interfaces__c__long" then
               Dump_Type ("long");
            elsif Name = "interfaces__c__long_long" then
               Dump_Type ("long long");
            elsif Name = "interfaces__c__unsigned_short" then
               Dump_Type ("unsigned short");
            elsif Name = "interfaces__c__unsigned_long" then
               Dump_Type ("unsigned long");
            elsif Name = "interfaces__c__unsigned_long_long" then
               Dump_Type ("unsigned long long");
            end if;

            if Done then
               return;
            end if;
         end;
      end if;

      if Signed then
         Write_Str_Col_Check ("integer_");
      else
         Write_Str_Col_Check ("unsigned_");
      end if;

      if Siz <= 8 then
         Write_Int (8);
      elsif Siz <= 16 then
         Write_Int (16);
      elsif Siz <= 32 then
         Write_Int (32);
      else
         Write_Int (64);
      end if;
   end Write_Integer_Type;

   --------------------------
   -- Write_Name_Col_Check --
   --------------------------

   procedure Write_Name_Col_Check (N : Name_Id) is
   begin
      Get_Name_String (N);
      Write_Str_Col_Check (Name_Buffer (1 .. Name_Len));
   end Write_Name_Col_Check;

   ---------------------
   -- Write_Binary_Op --
   ---------------------

   procedure Write_Binary_Op (Node : Node_Id; Op : String) is
   begin
      if Non_Standard_Modular_Type (Etype (Node)) then

         --  Nonbinary modulus operands are expanded by the front end

         if Non_Binary_Modulus (Etype (Node)) then
            raise Program_Error;
         end if;

         Write_Str ("(");
         Cprint_Node (Etype (Node));
         Write_Str (")((");

         if Nkind (Left_Opnd (Node)) /= N_Integer_Literal then
            Write_Str ("(unsigned_32)");
         end if;

         Cprint_Left_Opnd (Node);
         Write_Str (Op);

         if Nkind (Right_Opnd (Node)) = N_Integer_Literal then
            Write_Uint (Intval (Right_Opnd (Node)));
         else
            Write_Str ("(unsigned_32)");
            Cprint_Right_Opnd (Node);
         end if;

         Write_Str (") & ");
         Write_Uint (Modulus (Etype (Node)) - Uint_1);
         Write_Str (")");

      else
         Cprint_Left_Opnd (Node);
         Write_Str (Op);
         Cprint_Right_Opnd (Node);
      end if;
   end Write_Binary_Op;

   -----------------------
   -- Write_Param_Specs --
   -----------------------

   procedure Write_Param_Specs (N : Node_Id) is
      Formal : Node_Id;

   begin
      Write_Char ('(');

      --  Loop through formals (including any Extra_Formals)

      if Nkind (N) in N_Entity and then Is_Itype (N) then
         Formal := First_Formal_With_Extras (N);
      else
         Formal := First_Formal_With_Extras (Unique_Defining_Entity (N));
      end if;

      if No (Formal) then
         Write_Str ("void");
      else
         --  Handle dispatch table entities since the are itypes whose formals
         --  have no parent attribute.

         if Nkind (N) = N_Defining_Identifier
           and then Is_Dispatch_Table_Entity (N)
         then
            loop
               --  Output next formal

               declare
                  Typ : constant Entity_Id := Get_Full_View (Etype (Formal));
                  Ign : Boolean;
                  pragma Unreferenced (Ign);

               begin
                  pragma Assert (Is_Itype (N)
                    and then Ekind (N) = E_Subprogram_Type
                    and then No (Parent (Formal)));

                  if (Is_Record_Type (Typ)
                       or else Is_Array_Type (Typ)
                       or else Is_Descendant_Of_Address (Typ))
                    and then Ekind (Formal) = E_In_Parameter
                  then
                     Write_Str ("const ");
                  end if;

                  Ign :=
                    Cprint_Object_Reference
                      (Formal, Add_Access => Pass_Pointer (Formal));
               end;

               Next_Formal_With_Extras (Formal);
               exit when No (Formal);

               Write_Str (", ");
            end loop;

         --  General case

         else
            loop
               --  If the parent of this formal is an N_Parameter_Specification
               --  node then we just print that node, and that takes care of
               --  dealing with * for IN OUT and several other issues of
               --  complex parameters.

               if Nkind (Parent (Formal)) = N_Parameter_Specification then
                  Cprint_Node (Parent (Formal));

               --  Otherwise we have a normal IN parameter (typically an extra
               --  formal case), and we print the type and the parameter name
               --  in C style.

               else
                  Check_Definition (Etype (Formal), Error_Node => Formal);

                  --  Tagged types are passed by reference

                  if Is_Tagged_Type (Etype (Formal)) then
                     Cprint_Type_Name (Etype (Formal));
                     Write_Char ('*');
                  else
                     Cprint_Type_Name (Etype (Formal));
                  end if;

                  Write_Char (' ');
                  Write_Name_Col_Check (Chars (Formal));
               end if;

               --  Move to next formal

               Next_Formal_With_Extras (Formal);
               exit when No (Formal);

               Write_Str (", ");
            end loop;
         end if;
      end if;

      Write_Char (')');
   end Write_Param_Specs;

   ------------------------
   -- Write_Source_Lines --
   ------------------------

   procedure Write_Source_Lines (N : Node_Id) is
   begin
      if not Check_Sloc (Sloc (N)) then
         return;
      end if;

      Write_Source_Lines (First_Line (N), Last_Line (N));
   end Write_Source_Lines;

   procedure Write_Source_Lines (S : Source_Ptr) is
      SS : Source_Ptr := S;
   begin
      --  Don't go past end of file

      if SS > Source_Text (Current_Source_File)'Last then
         SS := Source_Text (Current_Source_File)'Last;
      end if;

      if not Check_Sloc (SS) then
         return;
      end if;

      declare
         L : constant Physical_Line_Number := Get_Physical_Line_Number (SS);
      begin
         Write_Source_Lines (L, L);
      end;
   end Write_Source_Lines;

   procedure Write_Source_Lines
     (From : Source_Ptr;
      To   : Physical_Line_Number) is
   begin
      if not Check_Sloc (From) then
         return;
      end if;

      Write_Source_Lines (Get_Physical_Line_Number (From), To);
   end Write_Source_Lines;

   procedure Write_Source_Lines (From, To : Physical_Line_Number) is
      Src : constant Source_Buffer_Ptr := Source_Text (Current_Source_File);

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean;
      --  Returns true if line L is a comment line or blank line

      procedure Write_Line_Directive (L : Physical_Line_Number);
      --  Write line directive for line L, no effect if L is a comment line

      procedure Write_Source_Line (L : Physical_Line_Number);
      --  Write source line L as C comment, no effect if L is a comment line.

      ---------------------
      -- Is_Comment_Line --
      ---------------------

      function Is_Comment_Line (L : Physical_Line_Number) return Boolean is
         Scn : Source_Ptr;

      begin
         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
            Scn := Scn + 1;
         end loop;

         return Src (Scn) in Line_Terminator
           or else Src (Scn .. Scn + 1) = "--";
      end Is_Comment_Line;

      --------------------------
      -- Write_Line_Directive --
      --------------------------

      procedure Write_Line_Directive (L : Physical_Line_Number) is
      begin
         --  No #line directives for comments or if no -g set

         if Debugger_Level = 0 or else Is_Comment_Line (L) then
            return;
         end if;

         if Column /= 1 then
            Write_Eol;
         end if;

         Write_Str ("#line ");
         Write_Int (Int (L));
         Write_Str (" """);
         Write_Str (Get_Name_String (File_Name (Current_Source_File)));
         Write_Char ('"');
         Write_Eol;
      end Write_Line_Directive;

      -----------------------
      -- Write_Source_Line --
      -----------------------

      First_Call : Boolean := True;

      procedure Write_Source_Line (L : Physical_Line_Number) is
         Scn : Source_Ptr;

      begin
         if Is_Comment_Line (L) then
            return;
         end if;

         if First_Call then
            if Column /= 1 then
               Write_Eol;
            end if;

            First_Call := False;
         end if;

         Write_Eol;
         Write_Str ("/* ");
         Write_Int (Int (L));
         Write_Str (": ");

         Scn := Line_Start (L, Current_Source_File);
         while Src (Scn) not in Line_Terminator loop
            Write_Char (Src (Scn));
            Scn := Scn + 1;
         end loop;

         Write_Str (" */");
      end Write_Source_Line;

      --  Local Variables

      From_Line : Physical_Line_Number := From;
      To_Line   : Physical_Line_Number := To;
      --  Effective from and to lines as adjusted below

   --  Start of processing for Write_Source_Lines

   begin
      --  Deal with no line number values

      if From_Line = No_Physical_Line_Number then
         if To_Line = No_Physical_Line_Number then
            return;
         else
            From_Line := To_Line;
         end if;
      end if;

      if To_Line = No_Physical_Line_Number then
         To_Line := From_Line;
      end if;

      --  If some lines already dealt with, adjust From_Line

      if Next_Line_To_Print > From_Line then
         From_Line := Next_Line_To_Print;
      end if;

      --  Return if all lines already printed. Adjust #line directive before
      --  to ensure that we resync the #line info.

      if From_Line > To_Line then
         Write_Line_Directive (To_Line);
         return;
      end if;

      --  If we are in Dump_Source_Text mode, and there are unprinted source
      --  lines before the first line for the current construct, print these
      --  source lines, but without line directives.

      if Dump_Source_Text and then Next_Line_To_Print < From_Line then
         loop
            Write_Source_Line (Next_Line_To_Print);
            Next_Line_To_Print := Next_Line_To_Print + 1;
            exit when Next_Line_To_Print = From_Line;
         end loop;
      end if;

      --  If we are in Dump_Source_Text mode, then print the source lines for
      --  the current construct, preceded by a blank line.

      if Dump_Source_Text then
         for J in From_Line .. To_Line loop
            Write_Source_Line (J);
         end loop;
      end if;

      --  Write line directive for the last line, no need to output multiple
      --  line directives.

      Write_Line_Directive (To_Line);

      --  Note all lines up to To processed and we are done

      Next_Line_To_Print := To_Line + 1;
      return;
   end Write_Source_Lines;

   -------------------------
   -- Write_Str_Col_Check --
   -------------------------

   procedure Write_Str_Col_Check (S : String) is
   begin
      if Int (S'Last) + Column > Sprint_Line_Limit then
         Write_Indent_Str ("  ");

         if S (S'First) = ' ' then
            Write_Str (S (S'First + 1 .. S'Last));
         else
            Write_Str (S);
         end if;

      else
         Write_Str (S);
      end if;
   end Write_Str_Col_Check;

   ----------------
   -- Write_Uint --
   ----------------

   --  Note: we go out of our way to be compatible with ancient versions of C
   --  here, since we anticipate the output being compiled on such compilers.

   procedure Write_Uint
     (U            : Uint;
      Column_Check : Boolean := True;
      Modular      : Boolean := False)
   is
      DDH : constant Nat := UI_Decimal_Digits_Hi (U);

      procedure Check_Column (Val : Nat);
      pragma Inline (Check_Column);
      --  Call Col_Check if Column_Check is True, otherwise do nothing

      ------------------
      -- Check_Column --
      ------------------

      procedure Check_Column (Val : Nat) is
      begin
         if Column_Check then
            Col_Check (Val);
         end if;
      end Check_Column;

   --  Start of processing for Write_Uint

   begin
      --  Output largest negative int value as (-X-1) where X is largest
      --  positive int value, to avoid generating out of range int value.

      if U = LNegInt then
         Check_Column (DDH + 4);
         Write_Char ('(');
         UI_Write (U + 1, Decimal);
         Write_Str ("-1)");

      --  Most common case of in int range other than largest neg number

      elsif LNegInt < U and then U <= LPosInt then
         Check_Column (DDH);
         UI_Write (U, Decimal);

         if Modular then
            Write_Char ('U');
         end if;

      --  Output largest negative long value as (-XL-1) where X is largest
      --  positive long value, to avoid generating out of range long value.

      elsif U = LNegLong then
         Check_Column (DDH + 5);
         Write_Char ('(');
         UI_Write (U + 1, Decimal);
         Write_Str ("L-1)");

      --  Same as before for 32-bit architectures; the above comparison is
      --  False on these architectures because the number of digits used to
      --  represent both quantities differ and therefore the result of their
      --  multiword comparison is False (see Uintp.UI_Ne).

      elsif ints = longs and then U = LNegLL then
         Check_Column (DDH + 5);
         Write_Char ('(');
         UI_Write (U + 1, Decimal);
         Write_Str ("L-1)");

      --  If in range of unsigned but not int, output with suffix U

      elsif LNegU <= U and then U <= LPosU then
         Check_Column (DDH + 1);
         UI_Write (U, Decimal);
         Write_Char ('U');

      --  If in range of long then output with suffix L

      elsif LNegLong < U and then U <= LPosLong then
         Check_Column (DDH + 1);
         UI_Write (U, Decimal);
         Write_Char ('L');

         if Modular then
            Write_Char ('U');
         end if;

      --  Remaining processing depends on whether we are allowing long long,
      --  which is controlled by restriction No_Long_Long_Integers.

      else
         --  Long_Long_Integer not allowed

         if Restriction_Active (No_Long_Long_Integers) then

            --  We must be in range of long unsigned, output with suffix LU

            if LNegUL <= U and then U <= LPosUL then
               Check_Column (DDH + 2);
               UI_Write (U, Decimal);
               Write_Str ("LU");

            --  Anything else should be impossible!

            else
               raise Program_Error;
            end if;

         --  Long_Long_Integer is allowed

         else
            --  If in range of long long, output with suffix LL. Note that we
            --  do not bother with largest negative number case here. We assume
            --  that if long long is allowed, the compiler is more modern.

            if LNegLL <= U and then U <= LPosLL then
               Check_Column (DDH + 2);
               UI_Write (U, Decimal);
               Write_Str ("LL");

               if Modular then
                  Write_Char ('U');
               end if;

            --  If in range of long long unsigned, output with suffix LLU

            elsif LNegULL <= U and then U <= LPosULL then
               Check_Column (DDH + 3);
               UI_Write (U, Decimal);
               Write_Str ("LLU");

            --  Anything else is capped to LPosULL. This can happen when
            --  outputing an unconstrained array indexed by Long_Long_Integer,
            --  see e.g. Ada.Streams.Stream_Element_Array

            else
               Check_Column (DDH + 2);
               UI_Write (LPosULL, Decimal);
               Write_Str ("LLU");
            end if;
         end if;
      end if;
   end Write_Uint;

   --------------------------------------
   -- Write_Unconstrained_Array_Prefix --
   --------------------------------------

   procedure Write_Unconstrained_Array_Prefix (N : Node_Id) is
   begin
      if Is_Unidimensional_Array_Type (Etype (N)) then
         Write_Str ("((");
         Cprint_Node (Component_Type (Etype (N)));
         Write_Str ("*)");

         Write_Char ('(');

         if Nkind (N) = N_Explicit_Dereference then
            Cprint_Node (Prefix (N));
         else
            Cprint_Node (N);
         end if;

         Write_Fatptr_Dereference;
         Write_Str ("))");

      elsif Nkind (N) in N_Has_Entity
        and then Present (Actual_Subtype (Entity (N)))
      then
         Write_Str ("(*(");
         Write_Id (Actual_Subtype (Entity (N)));
         Write_Str ("*) ");
         Cprint_Node (N);
         Write_Fatptr_Dereference;
         Write_Str (")");

      elsif Is_Array_Formal (N)
        and then Nkind (N) = N_Explicit_Dereference
        and then Has_Back_End_Itype (Entity (Prefix (N)))
      then
         Write_Str ("(*(");
         Write_Back_End_Itype_Id (Entity (Prefix (N)));
         Write_Str ("*) ");
         Write_Id (Entity (Prefix (N)));
         Write_Fatptr_Dereference;
         Write_Str (")");

      else
         declare
            S : constant String := Node_Kind'Image (Nkind (N));
         begin
            Error_Msg_Strlen := S'Length;
            Error_Msg_String (1 .. Error_Msg_Strlen) := S;
            Error_Msg_N ("unsupported unconstrained array access (~)", N);
         end;
      end if;
   end Write_Unconstrained_Array_Prefix;

   ---------------------------
   -- Write_Ureal_Col_Check --
   ---------------------------

   procedure Write_Ureal_Col_Check (Node : Node_Id) is
      type T_Mode is (Check_Orig_Expr, Output_Orig_Expr, Output_Number);

      function Has_Original_Node (N : Node_Id) return Boolean
        renames Is_Rewrite_Substitution;
      --  Return True if the original node of N is available

      procedure Output_Original_Expression
        (Expr    : Node_Id;
         Success : out Boolean;
         Mode    : T_Mode);
      --  Traverse the Original_Node chain to generate the original expression
      --  associated with Expr. When Mode is Check_Orig_Expr it checks if the
      --  original expression can be successfully generated and reports such
      --  result on Success; when Mode is Output_Orig_Expr it takes care of
      --  sending to the output the original expression.

      procedure Write_Real_Number_Col_Check
        (Node    : Node_Id;
         Success : out Boolean;
         Mode    : T_Mode);
      --  When Mode is Check_Orig_Expr this routine checks if the real number
      --  in Node can be directly output in C (that is, without normalization
      --  or dependency on pow()) and reports it on Success; when Mode is set
      --  to Output the number is sent to the output.

      --------------------------------
      -- Output_Original_Expression --
      --------------------------------

      procedure Output_Original_Expression
        (Expr    : Node_Id;
         Success : out Boolean;
         Mode    : T_Mode)
     is
         procedure Output_Close_Parenthesis (Node : Node_Id);
         --  Output all the close parenthesis associated with Node

         procedure Output_Open_Parenthesis (Node : Node_Id);
         --  Output all the opening parenthesis associated with Node

         function Stop_Searching_Original_Node (N : Node_Id) return Boolean;
         --  Return True if N is a node that we need to process to output
         --  the original expression.

         ------------------------------
         -- Output_Close_Parenthesis --
         ------------------------------

         procedure Output_Close_Parenthesis (Node : Node_Id) is
         begin
            if Mode /= Check_Orig_Expr then
               for J in 1 .. Paren_Count (Node) loop
                  Write_Char (')');
               end loop;
            end if;
         end Output_Close_Parenthesis;

         -----------------------------
         -- Output_Open_Parenthesis --
         -----------------------------

         procedure Output_Open_Parenthesis (Node : Node_Id) is
         begin
            if Mode /= Check_Orig_Expr then
               for J in 1 .. Paren_Count (Node) loop
                  Write_Char ('(');
               end loop;
            end if;
         end Output_Open_Parenthesis;

         ----------------------------------
         -- Stop_Searching_Original_Node --
         ----------------------------------

         function Stop_Searching_Original_Node (N : Node_Id) return Boolean is
         begin
            return Nkind (N) in N_Binary_Op
              or else Nkind (N) in N_Attribute_Reference
                                 | N_Identifier
                                 | N_Type_Conversion
              or else (Nkind (N) = N_Op_Minus
                        and then Nkind (Right_Opnd (N)) = N_Real_Literal);
         end Stop_Searching_Original_Node;

         --  Local variables

         Nod : Node_Id := Expr;

      --  Start of processing for Output_Original_Expression

      begin
         pragma Assert (Mode = Check_Orig_Expr or Mode = Output_Orig_Expr);

         --  Handle constant-folded named numbers

         if Nkind (Nod) = N_Real_Literal
           and then Present (Original_Entity (Nod))
         then
            Nod := Expression (Parent (Original_Entity (Nod)));
         end if;

         --  Traverse back the chain of original nodes until we find a node
         --  that must be processed to generate the original expression.

         while Has_Original_Node (Nod)
           and then not Stop_Searching_Original_Node (Nod)
         loop
            Nod := Original_Node (Nod);
         end loop;

         --  Process the node (which may require recursion)

         Success := True;

         if Comes_From_Source (Nod)
           and then Nkind (Nod) = N_Real_Literal
         then
            Write_Real_Number_Col_Check (Nod, Success, Mode);

         elsif Nkind (Nod) = N_Op_Minus
           and then Nkind (Right_Opnd (Nod)) = N_Real_Literal
         then
            Output_Open_Parenthesis (Nod);

            if Mode /= Check_Orig_Expr then
               Write_Char ('-');
            end if;

            Output_Original_Expression (Right_Opnd (Nod), Success, Mode);
            Output_Close_Parenthesis (Nod);

         elsif Nkind (Nod) in N_Binary_Op then
            declare
               Left_Success    : Boolean;
               Operand_Success : Boolean := True;
               Right_Success   : Boolean := True;

            begin
               Output_Open_Parenthesis (Nod);
               Output_Original_Expression
                 (Left_Opnd (Nod), Left_Success, Mode);

               if Left_Success then
                  case Nkind (Nod) is
                     when N_Op_Add =>
                        if Mode /= Check_Orig_Expr then
                           Write_Str (" + ");
                        end if;

                     when N_Op_Subtract =>
                        if Mode /= Check_Orig_Expr then
                           Write_Str (" - ");
                        end if;

                     when N_Op_Multiply =>
                        if Mode /= Check_Orig_Expr then
                           Write_Str (" * ");
                        end if;

                     when N_Op_Divide =>
                        if Mode /= Check_Orig_Expr then
                           Write_Str (" / ");
                        end if;

                     when others =>
                        if Mode = Check_Orig_Expr then
                           Operand_Success := False;
                        else
                           pragma Assert (False);
                           raise Program_Error;
                        end if;
                  end case;

                  if Operand_Success then
                     Output_Original_Expression
                       (Right_Opnd (Nod), Right_Success, Mode);
                  end if;
               end if;

               Output_Close_Parenthesis (Nod);

               Success :=
                 Left_Success and Operand_Success and Right_Success;
            end;

         --  For type conversions we continue iterating trying to locate
         --  the ultimate original expression.

         elsif Nkind (Nod) = N_Type_Conversion then
            declare
               Typ     : constant Entity_Id := Entity (Subtype_Mark (Nod));
               Src_Typ : constant Entity_Id :=
                           Get_Full_View (Etype (Expression (Nod)));
            begin
               --  Do not generate the original expression when it requires
               --  conversions from other types (i.e. integer or fixed-point
               --  types).

               if not Is_Floating_Point_Type (Typ)
                 or else not Is_Floating_Point_Type (Src_Typ)
               then
                  Success := False;

               else
                  if Mode /= Check_Orig_Expr then
                     Write_Str ("((");
                     Cprint_Type_Name (Typ);
                     Write_Char (')');
                  end if;

                  Output_Original_Expression
                    (Expression (Nod), Success, Mode);

                  if Mode /= Check_Orig_Expr then
                     Write_Char (')');
                  end if;
               end if;
            end;

         elsif Nkind (Nod) = N_Identifier then
            declare
               Decl : constant Node_Id := Parent (Entity (Nod));

            begin
               if Nkind (Decl) = N_Object_Renaming_Declaration then
                  Output_Original_Expression
                    (Name (Decl), Success, Mode);

               elsif Nkind (Expression (Decl)) = N_Real_Literal then
                  Output_Original_Expression
                    (Expression (Decl), Success, Mode);
               else
                  --  The identifier may reference another constant that is
                  --  initialized by means of an expression. We do not support
                  --  such cases.

                  Success := False;
               end if;
            end;

         --  Ada attributes are obviously not directly supported by C

         elsif Nkind (Nod) = N_Attribute_Reference then
            Success := False;

         --  Cannot output the original real number

         else
            Success := False;
         end if;
      end Output_Original_Expression;

      ---------------------------------
      -- Write_Real_Number_Col_Check --
      ---------------------------------

      procedure Write_Real_Number_Col_Check
        (Node    : Node_Id;
         Success : out Boolean;
         Mode    : T_Mode)
      is
         U     : constant Ureal     := Realval (Node);
         U_Typ : constant Entity_Id := Etype (Node);

         procedure Append_Suffix;
         --  Append the suffix associated with the literal to ensure that its
         --  computation is performed by C with the right arithmetic: no
         --  suffix defines in C a literal of a double, 'F' defines a literal
         --  of a float number, and 'L' defines a literal of a long double.

         function Is_Big_Number (Number : Uint) return Boolean;
         --  Return True if abs (Number) is larger or equal to 2**128

         procedure UI_Write (Input : Uint; Format : UI_Format := Auto);
         --  Writes a representation of Uint, consisting of a possible minus
         --  sign, followed by the value to the output file. The form of the
         --  value is an integer literal in either decimal (no base) or
         --  hexadecimal (base 16) format as appropriate. UI_Format shows which
         --  format to use. Auto, the default, asks UI_Write to make a guess at
         --  which output format will be more convenient to read.

         procedure Write (Real : Ureal);
         --  Writes value of Real to standard output. As a result of evaluation
         --  of static expressions, it is possible to generate constants (e.g.
         --  1/13) which have no such representation.

         -------------------
         -- Append_Suffix --
         -------------------

         procedure Append_Suffix is
            Typ : constant Entity_Id := Matching_Standard_Type (U_Typ);

         begin
            pragma Assert (Mode /= Check_Orig_Expr);

            if Typ = Standard_Short_Float or else Typ = Standard_Float then
               Write_Char ('F');

            elsif Typ = Standard_Long_Float then
               null;

            elsif U_Typ = Standard_Long_Long_Float then
               Write_Char ('L');
            end if;
         end Append_Suffix;

         -------------------
         -- Is_Big_Number --
         -------------------

         function Is_Big_Number (Number : Uint) return Boolean is
         begin
            return abs Number >= Uint_2 ** Uint_128;
         end Is_Big_Number;

         --------------
         -- UI_Write --
         --------------

         procedure UI_Write (Input : Uint; Format : UI_Format := Auto) is
         begin
            pragma Assert (Mode /= Check_Orig_Expr);

            --  For large numbers the routine Uintp.UI_Write outputs all its
            --  digits, which may result in a number too long for C.

            pragma Assert (not Is_Big_Number (Input));
            Write_Str (UI_Image (Input, Format));
         end UI_Write;

         -----------
         -- Write --
         -----------

         procedure Write (Real : Ureal) is
            T : Uint;

         begin
            Success := True;

            --  If value is negative, we precede the constant by a minus sign

            if UR_Is_Negative (Real) and then Mode /= Check_Orig_Expr then
               Write_Char ('-');
            end if;

            --  Zero is zero

            if UR_Is_Zero (Real) then
               if Mode /= Check_Orig_Expr then
                  Write_Str ("0.0");
                  Append_Suffix;
               end if;

            --  For constants with a denominator of zero, the value is simply
            --  the numerator value, since we are dividing by base**0, which
            --  is 1.

            elsif Denominator (Real) = 0 then
               if Mode /= Check_Orig_Expr then
                  UI_Write (Numerator (Real), Decimal);
                  Write_Str (".0");
                  Append_Suffix;
               end if;

            --  Small powers of 2 get written in decimal fixed-point format

            elsif Rbase (Real) = 2
              and then Denominator (Real) >= 1
              and then Denominator (Real) <= 3
            then
               if Mode /= Check_Orig_Expr then
                  if Denominator (Real) = 1 then
                     T := Numerator (Real) * (10 / 2);
                     UI_Write (T / 10, Decimal);
                     Write_Char ('.');
                     UI_Write (T mod 10, Decimal);

                  elsif Denominator (Real) = 2 then
                     T := Numerator (Real) * (100 / 4);
                     UI_Write (T / 100, Decimal);
                     Write_Char ('.');
                     UI_Write (T mod 100 / 10, Decimal);

                     if T mod 10 /= 0 then
                        UI_Write (T mod 10, Decimal);
                     end if;

                  elsif Denominator (Real) = 3 then
                     T := Numerator (Real) * (1000 / 8);
                     UI_Write (T / 1000, Decimal);
                     Write_Char ('.');
                     UI_Write (T mod 1000 / 100, Decimal);

                     if T mod 100 /= 0 then
                        UI_Write (T mod 100 / 10, Decimal);

                        if T mod 10 /= 0 then
                           UI_Write (T mod 10, Decimal);
                        end if;
                     end if;
                  end if;

                  Append_Suffix;
               end if;

            --  Constants in base 10 can be written in normal Ada literal style

            elsif Rbase (Real) = 10 then

               if Mode = Check_Orig_Expr then

                  --  Cannot output large numbers

                  if Is_Big_Number (Numerator (Real)) then
                     Success := False;
                     return;
                  end if;

               else

                  --  Use fixed-point format for small scaling values

                  if Denominator (Real) < 0
                    and then Denominator (Real) > -3
                  then
                     UI_Write
                       (Numerator (Real) * Rbase (Real)**(-Denominator (Real)),
                        Decimal);
                     Write_Str (".0");

                  elsif Denominator (Real) = 1 then
                     UI_Write (Numerator (Real) / 10, Decimal);
                     Write_Char ('.');
                     UI_Write (Numerator (Real) mod 10, Decimal);

                  elsif Denominator (Real) = 2 then
                     UI_Write (Numerator (Real) / 100, Decimal);
                     Write_Char ('.');
                     UI_Write (Numerator (Real) / 10 mod 10, Decimal);
                     UI_Write (Numerator (Real) mod 10, Decimal);

                  --  Else use decimal exponential format

                  else
                     pragma Assert (not Is_Big_Number (Numerator (Real)));

                     --  Write decimal constants with a nonzero unit digit.
                     --  This matches usual scientific notation.

                     UI_Image (Numerator (Real), Decimal);
                     Write_Char (UI_Image_Buffer (1));
                     Write_Char ('.');

                     if UI_Image_Length = 1 then
                        Write_Char ('0');
                     else
                        Write_Str (UI_Image_Buffer (2 .. UI_Image_Length));
                     end if;

                     Write_Char ('E');
                     UI_Write
                       (Int (UI_Image_Length - 1) - Denominator (Real),
                        Decimal);
                  end if;

                  Append_Suffix;
               end if;

            --  For numbers with nonzero base we should output the expression
            --  numerator/base**exponent. However this is not feasible for the
            --  general case because C90 does not have a builtin exponentiation
            --  operator. Therefore we handle the number as follows:

            --    * For regular numbers we normalize the real number and
            --      output the resulting rational number (numerator /
            --      denominator).

            --    * For large numbers we rely on the C pow() function and
            --      output the number numerator/pow(base,exponent). This
            --      approach cannot be used with numbers that are part of
            --      a constant expression since it is rejected by the C
            --      compiler.

            elsif Rbase (Real) /= 0 then
               if Mode = Check_Orig_Expr then
                  Success := False;
                  return;
               end if;

               declare
                  Norm_Real : Ureal;

               begin
                  --  Note that we do not propagate the negative sign to the
                  --  normalized number since the minus character was already
                  --  sent to the output.

                  Norm_Real :=
                    UR_From_Components
                      (Num      => Norm_Num (Real),
                       Den      => Norm_Den (Real),
                       Negative => False);

                  if not Is_Big_Number (Numerator (Norm_Real))
                    and then not Is_Big_Number (Denominator (Norm_Real))
                  then
                     Write (Norm_Real);

                  elsif In_Object_Decl_Init_Expr then
                     Error_Msg_N ("number too large", Node);
                     Write_Str ("1.0/0.0 /* Number too large */");

                  --  Instead of generating numerator/pow(base,exponent), we
                  --  generate numerator*pow(base,-exponent). Thus, for large
                  --  numbers where the denominator is very small we avoid
                  --  computing a division by 0 if the number is rounded.

                  else
                     UI_Write (Numerator (Real), Decimal);
                     Write_Str (".0L");

                     --  An exponent of 0 can be omitted

                     if Denominator (Real) /= 0 then
                        Error_Msg_N
                          ("computation requires C99 powl()??", Node);

                        Write_Str (" * powl(");
                        Write_Int (Rbase (Real));
                        Write_Char (',');

                        if Denominator (Real) <= 0 then
                           UI_Write (-Denominator (Real), Decimal);
                        else
                           Write_Str ("(-");
                           UI_Write (Denominator (Real), Decimal);
                           Write_Char (')');
                        end if;

                        Write_Char (')');
                     end if;
                  end if;
               end;

            --  Rationals where numerator is divisible by denominator can be
            --  output as literals after we do the division. This includes the
            --  common case where the denominator is 1.

            elsif Numerator (Real) mod Denominator (Real) = 0 then
               if Mode /= Check_Orig_Expr then
                  UI_Write (Numerator (Real) / Denominator (Real), Decimal);
                  Write_Str (".0");
                  Append_Suffix;
               end if;

            --  Other non-based (rational) constants are written in num/den
            --  style

            else
               Success := False;

               if Mode /= Check_Orig_Expr then
                  UI_Write (Numerator (Real), Decimal);
                  Write_Str (".0");
                  Append_Suffix;

                  Write_Char ('/');

                  UI_Write (Denominator (Real), Decimal);
                  Write_Str (".0");
                  Append_Suffix;
               end if;
            end if;
         end Write;

         --  Local variables

         D : constant Uint := Denominator (U);
         N : constant Uint := Numerator (U);

      --  Start of processing for Write_Real_Number_Col_Check

      begin
         if Mode /= Check_Orig_Expr then
            Col_Check
              (UI_Decimal_Digits_Hi (D) + UI_Decimal_Digits_Hi (N) + 4);
         end if;

         Write (U);
      end Write_Real_Number_Col_Check;

      --  Local variables

      Success : Boolean;

   --  Start of processing for Write_Ureal_Col_Check

   begin
      --  In order to improve the readability of the generated code check if
      --  we can output in C the original Ada expression associated with this
      --  real number.

      Output_Original_Expression (Node, Success, Mode => Check_Orig_Expr);

      --  If we can successfully generate the original expression the we repeat
      --  the same tree traversal to generate such output.

      if Success then
         Output_Original_Expression (Node, Success, Mode => Output_Orig_Expr);

      --  Otherwise output this number (which may imply generating it as a
      --  rational number).

      else
         Write_Real_Number_Col_Check (Node, Success, Mode => Output_Number);
      end if;
   end Write_Ureal_Col_Check;

end Cprint;
