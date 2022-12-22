with Ada.Characters.Handling;  use Ada.Characters.Handling;

package body AI302.Strings.Case_Insensitive is

   function "=" (Left, Right : String) return Boolean is
   begin
      return Standard."=" (To_Upper (Left), To_Upper (Right));
   end;
     

   function "<" (Left, Right : String) return Boolean is
   begin
      return Standard."<" (To_Upper (Left), To_Upper (Right));
   end;
   

   function ">" (Left, Right : String) return Boolean is
   begin
      return Case_Insensitive."<" (Right, Left);
   end;
   
   

   function "<=" (Left, Right : String) return Boolean is
   begin
      return not Case_Insensitive.">" (Left, Right);
   end;


   function ">=" (Left, Right : String) return Boolean is
   begin
      return not Case_Insensitive."<" (Left, Right);
   end;


   function Hash (Key : String) return Containers.Hash_Type is

      --source of this algorithm: GNAT.HTable.Hash (g-htable.adb)
        
      use AI302.Containers;

      function Rotate_Left 
        (Value  : Hash_Type;
         Amount : Natural) return Hash_Type;
      
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Hash_Type := 0;

   begin

      for J in Key'Range loop
         Tmp := Rotate_Left (Tmp, 1) + Character'Pos (To_Upper (Key (J)));
      end loop;

      return Tmp;
      
   end Hash;
   

end AI302.Strings.Case_Insensitive;





