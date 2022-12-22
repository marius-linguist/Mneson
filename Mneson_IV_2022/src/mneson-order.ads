-- GENERIC PACKAGE MNESON.ORDER (SPEC)
-- Order relations for element and one-dimensional array types.
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

generic

   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;

package Mneson.Order is

   type One_Dimensional_Array is
      array (Positive range <>) of Element_Type;

   function ">" (Left, Right : Element_Type) return Boolean;
   function "<=" (Left, Right : Element_Type) return Boolean;
   function ">=" (Left, Right : Element_Type) return Boolean;

   function "<" (Left, Right : One_Dimensional_Array) return Boolean;
   function ">" (Left, Right : One_Dimensional_Array) return Boolean;
   function "<=" (Left, Right : One_Dimensional_Array) return Boolean;
   function ">=" (Left, Right : One_Dimensional_Array) return Boolean;

end;