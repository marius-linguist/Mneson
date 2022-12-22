-- PACKAGE INTEGER_TO_ACCESS_CONVERSIONS (GENERIC BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Integer_To_Access_Conversions is

  package Address_To_Pointer_Conversions is
    new System.Address_To_Access_Conversions
      (Object);

  function To_Pointer (Value : Integer) return Object_Pointer is
  begin
    return
      Object_Pointer
       (Address_To_Pointer_Conversions.To_Pointer
        (System.Storage_Elements.To_Address
         (System.Storage_Elements.Integer_Address
          (Value))));
  end To_Pointer;

  function To_Integer (Value : Object_Pointer) return Integer is
  begin
    return
      Integer
       (System.Storage_Elements.To_Integer
        (Address_To_Pointer_Conversions.To_Address
         (Address_To_Pointer_Conversions.Object_Pointer
          (Value))));
  end To_Integer;

end;