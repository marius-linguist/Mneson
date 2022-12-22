-- PACKAGE INTEGER_TO_ACCESS_CONVERSIONS (GENERIC SPEC)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

generic

  type Object (<>) is limited private;

package Integer_To_Access_Conversions is

  type Object_Pointer is access all Object;
  function To_Pointer (Value : Integer) return Object_Pointer;
  function To_Integer (Value : Object_Pointer) return Integer;

end;