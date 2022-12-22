with String_Sets;  use String_Sets;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_String_Sets is

   S : Set_Type;

   C : Cursor_Type;
   B : Boolean;

begin

   Insert (S, "matthew", C, B);
   pragma Assert (B);
   pragma Assert (Element (C) = "matthew");
   pragma Assert (Is_In ("matthew", S));

   Insert (S, "joseph", C, B);
   pragma Assert (B);
   pragma Assert (Element (C) = "joseph");
   pragma Assert (Is_In ("joseph", S));

   Insert (S, "heaney", C, B);
   pragma Assert (B);
   pragma Assert (Element (C) = "heaney");
   pragma Assert (Is_In ("heaney", S));

   C := First (S);

   while C /= Back (S) loop
      Put_Line (Element (C));
      Increment (S, C);
   end loop;

end Test_String_Sets;



