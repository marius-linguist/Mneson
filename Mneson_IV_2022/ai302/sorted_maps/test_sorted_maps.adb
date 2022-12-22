with Integer_Character_Maps;  use Integer_Character_Maps;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure Test_Sorted_Maps is

   procedure Print (M : Map_Type) is

      procedure Process (C : Cursor_Type) is
      begin
         Put (Key (C), Width => 0);
         Put (Element (C));
         Put (' ');
      end;

      procedure Iterate is new Generic_Iteration;
   begin
      Iterate (M);
      New_Line;
   end;

   M, M2 : Map_Type;

   C : Cursor_Type;
   B : Boolean;

begin

   Print (M);

   Insert (M, 42, 'a', C, B);
   pragma Assert (B);
   pragma Assert (First_Key (M) = 42);
   pragma Assert (First_Element (M) = 'a');
   Print (M);

   M2 := M;
   pragma Assert (M2 = M);

   Insert (M, 99, 'b', C, B);
   pragma Assert (B);
   pragma Assert (Last_Key (M) = 99);
   pragma Assert (Last_Element (M) = 'b');
   Print (M);

   M2 := M;
   pragma Assert (M2 = M);

end Test_Sorted_Maps;

