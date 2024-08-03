------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     A D A . N U M E R I C S . E L E M E N T A R Y _ F U N C T I O N S    --
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

package Ada.Numerics.Elementary_Functions is
   pragma Pure;

   function Sqrt (X : Float) return Float;

   function Log (X : Float) return Float
     with Convention => C, Import, External_Name => "logf";

   function Log (X, Base : Float) return Float is
     (Log (X) / Log (Base))
     with Inline_Always;

   function Exp (X : Float) return Float;

   function Pow (Left, Right : Float) return Float;
   function "**" (Left, Right : Float) return Float renames Pow;

   function Sin (X : Float) return Float;

   function Cos (X : Float) return Float;

   function Tan (X : Float) return Float;

   function Arcsin (X : Float) return Float;

   function Arccos (X : Float) return Float;

   function Arctan (Y : Float) return Float;

   function Sinh (X : Float) return Float;

   function Cosh (X : Float) return Float;

   function Tanh (X : Float) return Float;

private

   pragma Import (C, Sqrt, "sqrtf");
   pragma Import (C, Exp, "expf");
   pragma Import (C, Pow, "powf");
   pragma Import (C, Sin, "sinf");
   pragma Import (C, Cos, "cosf");
   pragma Import (C, Tan, "tanf");
   pragma Import (C, Arcsin, "asinf");
   pragma Import (C, Arccos, "acosf");
   pragma Import (C, Arctan, "atanf");
   pragma Import (C, Sinh, "sinhf");
   pragma Import (C, Cosh, "coshf");
   pragma Import (C, Tanh, "tanhf");

end Ada.Numerics.Elementary_Functions;
