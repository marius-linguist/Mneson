with AI302.Strings.Case_Insensitive;
with Ada.Characters.Handling;         use Ada.Characters.Handling;

package body AI302.Strings.Wide_Case_Insensitive is

   function "=" (Left, Right : Wide_String) return Boolean is
   begin
      return Case_Insensitive."=" (To_String (Left), To_String (Right));
   end;
     

   function "<" (Left, Right : Wide_String) return Boolean is
   begin
      return Case_Insensitive."<" (To_String (Left), To_String (Right));
   end;
   

   function ">" (Left, Right : Wide_String) return Boolean is
   begin
      return Wide_Case_Insensitive."<" (Right, Left);
   end;
   
   

   function "<=" (Left, Right : Wide_String) return Boolean is
   begin
      return not Wide_Case_Insensitive.">" (Left, Right);
   end;


   function ">=" (Left, Right : Wide_String) return Boolean is
   begin
      return not Wide_Case_Insensitive."<" (Left, Right);
   end;


   function Hash (Key : Wide_String) return Containers.Hash_Type is

      --source of this algorithm: GNAT.HTable.Hash (g-htable.adb)
        
      use AI302.Containers;

      function Rotate_Left 
        (Value  : Hash_Type;
         Amount : Natural) return Hash_Type;
      
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Hash_Type := 0;

   begin

      for J in Key'Range loop
         declare
            WC : constant Wide_Character := Key (J);
            C  : constant Character := To_Character (WC);
         begin
            Tmp := Rotate_Left (Tmp, 1) + Character'Pos (To_Upper (C));
         end;
      end loop;

      return Tmp;
      
   end Hash;
   

end AI302.Strings.Wide_Case_Insensitive;

