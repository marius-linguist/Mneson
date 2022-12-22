with Wordcount_Maps;     use Wordcount_Maps;
with Wordcount_Vectors;  use Wordcount_Vectors;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with AI302.Strings.Case_Insensitive;  use AI302.Strings;
with AI302.Containers;  use AI302.Containers;

procedure Wordcount is

   Map : Map_Type;

   procedure Insert (Word : String) is

      C : Cursor_Type;
      B : Boolean;

      --NOTE:
      --It might be nice to have a renaming of the generic formal
      --Element_Type, so we could do this:
      --
      --   type Count_Access is access all Wordcount_Maps.Element_Subtype;
      --END NOTE.
      --
      type Count_Access is access all Natural;
      for Count_Access'Storage_Size use 0;

      function To_Access is
         new Wordcount_Maps.Generic_Element (Count_Access);

   begin -- Insert

      Insert (Map, Word, 0, C, B);

      declare
         N : Integer renames To_Access (C).all;
      begin
         N := N + 1;
      end;

--        Put ("map.length=");
--        Put (Length (Map), Width => 0);
--        New_Line;

      declare
         M2 : constant Map_Type := Map;  --for test only
         pragma Assert (M2 = Map);
      begin
         null;
      end;

   end Insert;

   File : File_Type;

   Word_Chars : constant Character_Set :=
     To_Set (Character_Ranges'(('a', 'z'), ('A', 'Z')));

   Line : String (1 .. 2000);

   Line_First : Positive;
   Line_Last : Natural;

   Word_First : Positive;
   Word_Last : Natural;


   procedure Print (C : Cursor_Type) is
   begin
      Put (To_Lower (Key (C)));
      Put (' ');
      Put (Element (C), Width => 0);
      New_Line;
   end;

   procedure Print (V : in Vector_Type) is
   begin
      for I in First (V) .. Last (V) loop
         Print (Element (V, I));
      end loop;
   end;

   V : Vector_Type;

begin -- Wordcount

   if Argument_Count = 0 then
      Put_Line (Command_Name & " <file>");
      return;
   end if;

   if Argument_Count > 1 then
      Put_Line ("too many command-line arguments");
      return;
   end if;

   begin
      Open (File, In_File, Name => Argument (1));
   exception
      when Name_Error =>
         Put_Line ("unable to open file");
         return;
   end;

   while not End_Of_File (File) loop

      Get_Line (File, Line, Line_Last);
      pragma Assert (Line_Last < Line'Last);

      Line_First := Line'First;

      loop

         Find_Token
           (Source => Line (Line_First .. Line_Last),
            Set    => Word_Chars,
            Test   => Ada.Strings.Inside,
            First  => Word_First,
            Last   => Word_Last);

         exit when Word_Last = 0;

         Insert (Word => Line (Word_First .. Word_Last));

         Line_First := Word_Last + 1;

      end loop;

   end loop;

   New_Line;

   Put_Line ("map entries (active):");
   New_Line;

   Print_Map :
   declare
      I : Cursor_Type := First (Map);
      J : constant Cursor_Type := Back (Map); --NOTE: keep this?
   begin
      while I /= J loop
         Print (I);
         Increment (Map, I);
      end loop;
   end Print_Map;

   New_Line;
   Put_Line ("map entries (passive):");
   New_Line;

   Print_Map2 :
   declare
      procedure Iterate is
         new Wordcount_Maps.Generic_Iteration (Print);
   begin
      Iterate (Map);
   end Print_Map2;

   New_Line;

   Populate_Vector :
   declare
      procedure Process (C : Cursor_Type) is
      begin
         Append (V, C);
      end;

      procedure Iterate is
         new Wordcount_Maps.Generic_Iteration;

      N : constant Size_Type := Length (Map);
   begin
      Resize (V, Size => N);
      Iterate (Map);
   end Populate_Vector;

   New_Line;
   Put_Line ("before vector sort:");
   New_Line;
   Print (V);
   New_Line;

   Sort_Vector :
   declare
      function "<" (L, R : Cursor_Type) return Boolean is
      begin
         if Element (L) > Element (R) then
            return True;
         end if;

         if Element (L) < Element (R) then
            return False;
         end if;

         --NOTE:
         --This feels like a kludge.  I'm not sure how the ARG subcommittee
         --intended a case insensitive string compare to work.  I don't think
         --simply use'ing that package is good enough to get a case insensitive
         --string compare (B.E.) -- you need explicit qualification as below.
         --
         --My original proposal called for something like this:
         --   Equal_String_Case_Insensitive (LS, RS);
         --
         --END NOTE.
         --
         return Case_Insensitive."<" (Key (L), Key (R));
      end "<";

      procedure Sort is
         new Wordcount_Vectors.Generic_Sort;
   begin
      Sort (V);
   end Sort_Vector;

   New_Line;
   Put_Line ("after vector sort:");
   New_Line;
   Print (V);
   New_Line;

   Put_Line ("top 10 words:");

   for I in First (V) .. First (V) + 9 loop
      Print (Element (V, I));
   end loop;

end Wordcount;

