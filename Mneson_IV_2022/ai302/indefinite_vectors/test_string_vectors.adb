with String_Vectors;  use String_Vectors;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_String_Vectors is

   procedure Print (V : Vector_Type) is

      procedure Process (S : String) is
      begin
         Put_Line (S);
      end;

      procedure Iterate is
         new String_Vectors.Generic_Constant_Iteration;
   begin
      Iterate (V);
      New_Line;
   end;

   V : Vector_Type;

   procedure Sort is
      new String_Vectors.Generic_Sort;

--     procedure Sort2 is
--        new String_Vectors.Generic_Sort2 (">");

--     procedure Sort3 is
--        new String_Vectors.Generic_Sort3 (">");

begin

   Append (V, "matthew");
   pragma Assert (Last_Element (V) = "matthew");
   Print (V);

   Append (V, "joseph");
   pragma Assert (Last_Element (V) = "joseph");
   Print (V);

   Append (V, "heaney");
   pragma Assert (Last_Element (V) = "heaney");
   Print (V);

   Insert (V, Before => 1, New_Item => "janet");
   pragma Assert (First_Element (V) = "janet");
   Print (V);

   Replace_Element (V, Index => 1, By => "good");
   pragma Assert (First_Element (V) = "good");
   Print (V);

   Insert_N (V, Before => 2, Count => 5, New_Item => "five");
   Print (V);

   Append (V, New_Item => "Z");
   Append (V, New_Item => "Y");
   Append (V, New_Item => "X");
   Print (V);

   Sort (V);
   Print (V);

   --Sort2 (V);
   --Print (V);

end Test_String_Vectors;
