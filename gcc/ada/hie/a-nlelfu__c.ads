------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.NUMERICS.LONG_ELEMENTARY_FUNCTIONS                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This is a simplified version for SPARK-to-C which maps directly to the
--  underlying math.h library.

package Ada.Numerics.Long_Elementary_Functions is
   pragma Pure;

   function Sqrt (X : Long_Float) return Long_Float;

   function Log (X : Long_Float) return Long_Float
     with Convention => C, Import, External_Name => "log";

   function Log (X, Base : Long_Float) return Long_Float is
     (Log (X) / Log (Base))
     with Inline_Always;

   function Exp (X : Long_Float) return Long_Float;

   function Pow (Left, Right : Long_Float) return Long_Float;
   function "**" (Left, Right : Long_Float) return Long_Float renames Pow;

   function Sin (X : Long_Float) return Long_Float;

   function Cos (X : Long_Float) return Long_Float;

   function Tan (X : Long_Float) return Long_Float;

   function Arcsin (X : Long_Float) return Long_Float;

   function Arccos (X : Long_Float) return Long_Float;

   function Arctan (Y : Long_Float) return Long_Float;

   function Sinh (X : Long_Float) return Long_Float;

   function Cosh (X : Long_Float) return Long_Float;

   function Tanh (X : Long_Float) return Long_Float;

private

   pragma Import (C, Sqrt, "sqrt");
   pragma Import (C, Exp, "exp");
   pragma Import (C, Pow, "pow");
   pragma Import (C, Sin, "sin");
   pragma Import (C, Cos, "cos");
   pragma Import (C, Tan, "tan");
   pragma Import (C, Arcsin, "asin");
   pragma Import (C, Arccos, "acos");
   pragma Import (C, Arctan, "atan");
   pragma Import (C, Sinh, "sinh");
   pragma Import (C, Cosh, "cosh");
   pragma Import (C, Tanh, "tanh");

end Ada.Numerics.Long_Elementary_Functions;
