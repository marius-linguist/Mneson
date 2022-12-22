package body Anagram_Types is

   function "<" (L, R : Entry_Type) return Boolean is
   begin
      return L.Index < R.Index;
   end;
   

   function "<" (L : Map_Index_Subtype;
                 R : Entry_Type) return Boolean is
   begin
      return L < R.Index;
   end;
   

   function ">" (L : Map_Index_Subtype;
                 R : Entry_Type) return Boolean is
   begin
      return L > R.Index;
   end;
   

   procedure Set_Key (E : in out Entry_Type;
                      K : in     Map_Index_Subtype) is
   begin
      E.Index := K;
   end;
   

end Anagram_Types;




