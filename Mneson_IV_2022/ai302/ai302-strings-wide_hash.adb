--source of this algorithm: GNAT.HTable.Hash (g-htable.adb)

function AI302.Strings.Wide_Hash
  (Key : Wide_String) return Containers.Hash_Type is

   use AI302.Containers;

   function Rotate_Left
     (Value  : Hash_Type;
      Amount : Natural) return Hash_Type;

   pragma Import (Intrinsic, Rotate_Left);

   Tmp : Hash_Type := 0;

begin

   for J in Key'Range loop
      Tmp := Rotate_Left (Tmp, 1) + Wide_Character'Pos (Key (J));
   end loop;

   return Tmp;

end AI302.Strings.Wide_Hash;


