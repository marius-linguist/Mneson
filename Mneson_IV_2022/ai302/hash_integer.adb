with Ada.Unchecked_Conversion;

function Hash_Integer (I : Integer'Base) return Hash_Type is

   function To_Hash is
      new Ada.Unchecked_Conversion (Integer'Base, Hash_Type);

begin

   return To_Hash (I);

end;



