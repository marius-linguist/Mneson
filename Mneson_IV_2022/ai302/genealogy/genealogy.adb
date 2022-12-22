--This genealogy program is based on the example in Chapter 18 of:
--  STL Tutorial and Reference Guide, 2nd ed.
--  Musser, Derge, and Saini
--
--The genealogy file for this program can be found at Musser's web site:
--
--  <http://www.cs.rpi.edu/~musser/stl-book/source/>
--
--The file name is TCS-genealogy.txt .
--


with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;

with Parser;

with Data_Maps;  use Data_Maps;
with Relation_Maps;

with AI302.Strings.Case_Insensitive;  use AI302.Strings;
with AI302.Containers.Size_IO;
--with AI302.Containers.Generic_Sort_Unconstrained_Array;

with String_Vectors;  use String_Vectors;


procedure Genealogy is

   Places : Data_Maps.Map_Type renames Relation_Maps.Places;
   Dates  : Data_Maps.Map_Type renames Relation_Maps.Dates;

   Students : Relation_Maps.Date_Ordered_Set_Maps.Map_Type;
   Advisors : Relation_Maps.String_Vector_Maps.Map_Type; --Relation_Maps.Map_Types.Map_Type;

   use Relation_Maps.Date_Ordered_Set_Maps;
   use Relation_Maps.String_Vector_Maps;
   --use Relation_Maps.Name_Insertion;
   use Relation_Maps.Date_Ordered_Sets;
   use Relation_Maps;
   use AI302.Containers.Size_IO;
   use AI302.Containers;

begin

   if Argument_Count = 0 then
      Put_Line ("no genealogy file specified");
      return;
   end if;

   Parser.Initialize (File => Argument (1));

   while Parser.Get_Record loop

      declare

         Student : constant String := Parser.Student;
         Advisor : constant String := Parser.Advisor;

         C : Data_Maps.Cursor_Type;
         MC : Relation_Maps.Date_Ordered_Set_Maps.Cursor_Type;
         --SC : Relation_Maps.Date_Ordered_Sets.Cursor_Type;
         AMC : Relation_Maps.String_Vector_Maps.Cursor_Type;

         B : Boolean;

      begin
--           Put (Student); Put (':');
--           Put (Advisor); Put (':');
--           Put (Parser.Place); Put (':');
--           Put (Parser.Date); Put (':');
--           New_Line;

         --Replace (Places, Student, Parser.Place);

         Insert (Places, Student, Parser.Place, C, B);

         if not B
           and then not Case_Insensitive."=" (Element (C), Parser.Place)
         then
            Put_Line ("duplicate student; place mismatch");
            Put ("student=");
            Put (Student);
            Put (" places.element=");
            Put (Element (C));
            Put (" place=");
            Put (Parser.Place);
            New_Line (2);
            Replace_Element (C, By => Parser.Place);
         end if;

         --Replace (Dates, Student, Parser.Date);

         Insert (Dates, Student, Parser.Date, C, B);

         if not B
           and then Element (C) /= Parser.Date
         then

            Put_Line ("duplicate student; date mismatch");
            Put ("student=");
            Put (Student);
            Put (" dates.element=");
            Put (Element (C));
            Put (" date=");
            Put (Parser.Date);
            Put (" advisor=");
            Put (Advisor);
            New_Line (2);

            --NOTE:
            --This changes the key value of a student,
            --because students are ordered by date in the
            --Date_Ordered_Set.  Allowing the student's date
            --to be changed makes it hard to reason whether
            --this program is correct, or fully general wrt
            --input.  However, I do it here because that's
            --what the ex in Chap 18 does.
            --
            --What would probably be safer is to use a
            --map instead of a set, and let the key be the
            --date.  The map key is immutable so there would
            --be no problem.  If you're reading this then
            --check to see if there's a genealogy2 or whatever,
            --that corrects this "feature" of the ex.
            --
            --(Using a map might be better anyway, since
            --using a set of vectors as a multiset is
            --kind of awkward.)
            --END NOTE.

            Replace_Element (C, By => Parser.Date);

         end if;

         Insert
           (Map     => Students,
            Key     => Advisor,
            Cursor  => MC,
            Success => B);

         Insert (Set => To_Access (MC).all, Name  => Student);

         Insert
           (Map => Advisors,
            Key => Student,
            Cursor => AMC,
            Success => B);

         declare
            V : String_Vectors.Vector_Type renames To_Access (AMC).all;
         begin
            --pragma Assert (not Is_In (Advisor, V));
            Append (V, New_Item => Advisor);
         end;

      end;

   end loop;

   New_Line (2);

   declare
      procedure Process (S : String) is
      begin
         Put (" """);
         Put (S);
         Put ('"');
      end;

      procedure Iterate is
         new String_Vectors.Generic_Constant_Iteration;

      procedure Process (I : String_Vector_Maps.Cursor_Type) is
         V : String_Vectors.Vector_Type renames To_Access (I).all;
         N : constant Size_Type := Length (V);
      begin
         if N <= 1 then
            return;
         end if;

         Put ("Student """);
         Put (Key (I));
         Put (""" has ");
         Put (N, Width => 0);
         Put (" advisors:");

         Iterate (V);
         New_Line;
      end;

      procedure Iterate is
         new String_Vector_Maps.Generic_Iteration;
   begin
      Iterate (Advisors);
      New_Line (2);
   end;


   declare
      procedure Process (J : Date_Ordered_Sets.Cursor_Type) is
         V : String_Vectors.Vector_Type renames To_Access (J).all;
         S : String renames To_Access (V, First (V)).all;
         D : String renames To_Access (Find (Dates, Key => S)).all;
      begin
         Put (' ');
         Put (D);
         Put ('/');
         Put (Length (V), Width => 0);
      end;

      procedure Iterate is
         new Date_Ordered_Sets.Generic_Iteration;

      procedure Process (I : Date_Ordered_Set_Maps.Cursor_Type) is
      begin
         Put ("Advisor """);
         Put (Key (I));
         Put (""":");
         Iterate (Set => To_Access (I).all);
         New_Line;
      end;

      procedure Iterate is
         new Date_Ordered_Set_Maps.Generic_Iteration;
   begin
      Iterate (Map => Students);
      New_Line (2);
   end;


   Find_Roots :
   declare

      procedure Handle_No_Advisor (Advisor : in String);

      procedure Process (I : in String_Vector_Maps.Cursor_Type) is

         V : String_Vectors.Vector_Type renames To_Access (I).all;
         --V holds all the advisors for student Key (I)

      begin

         for Index in First (V) .. Last (V) loop

            declare
               Advisor : constant String := Element (V, Index);
            begin
               if Advisor = "---" then
                  Put ("student=");
                  Put (Key (I));
                  Put (" advisor=""---""");
                  New_Line (2);

                  return;
               end if;

               if Is_In (Advisor, Advisors) then
                  Put ("student=");
                  Put (Key (I));
                  Put (" advisor=");
                  Put (Advisor);
                  Put (" is already member of advisors");
                  New_Line (2);

                  return;
               end if;
            end;

         end loop;

         Put ("student=");
         Put (Key (I));
         Put (" first_advisor=");

         declare
            First_Advisor : constant String := First_Element (V);
         begin
            Put_Line (First_Advisor);
            Handle_No_Advisor (First_Advisor);
         end;

      end Process;

      procedure Iterate is
         new String_Vector_Maps.Generic_Iteration;

      Roots : Date_Ordered_Sets.Set_Type renames
        To_Access (Find (Students, Key => "---")).all;
      --
      --This is the set of all students whose advisor is unknown.
      --We now have to find these students' advisors, and among
      --those advisors find those that don't have a database
      --record of their own.  Those advisors are then added to
      --the set of students with unknown advisors.

      procedure Handle_No_Advisor (Advisor : in String) is

         K : Date_Ordered_Sets.Cursor_Type := First (Roots);
         KK : constant Date_Ordered_Sets.Cursor_Type := Back (Roots);

         C : constant Data_Maps.Cursor_Type :=
           Find (Dates, Key => Advisor);

      begin

         if C /= Back (Dates) then

            Put ("advisor.date=");
            Put (Element (C));
            Put_Line ("; no search required");
            New_Line;

            pragma Assert (Is_In (Roots, Name => Advisor));

            return;

         end if;

         Put_Line ("no date for advisor; searching database");

         --NOTE:
         --It's kind of pointless to search, since we know
         --that without a date then the advisor won't be
         --found.  It would be faster to assign the advisor
         --the date "!", and then append this advisor to the
         --the vector in roots["!"].
         --
         --But I'll leave this here for now, since this is
         --basically what the original example does.
         --END NOTE.

         Search_Vectors :
         while K /= KK loop

            if Is_In (Advisor, Vector => To_Access (K).all) then

               Put ("found first advisor=");
               Put (Advisor);
               Put (" in roots set; date=");
               Put (Element (Dates, Advisor));
               New_Line (2);

               return;

            end if;

            Increment (K);

         end loop Search_Vectors;

         Put ("inserting first_advisor=");
         Put (Advisor);
         Put_Line (" in roots set");
         New_Line;

         Insert (Set => Roots, Name => Advisor);

      end Handle_No_Advisor;

   begin

      Iterate (Map => Advisors);

   end Find_Roots;


   Output_Roots :
   declare

      procedure Output_Tree (Name  : in String;
                             Level : in Natural);

      procedure Iterate (J     : in Date_Ordered_Sets.Cursor_Type;
                         Level : in Natural) is

         procedure Process (Name : in String) is
         begin
            Output_Tree (Name, Level);
         end;

         procedure Iterate is
            new String_Vectors.Generic_Constant_Iteration;

         V : String_Vectors.Vector_Type renames To_Access (J).all;
      begin
         Iterate (V);
      end;

      procedure Output_Tree (Name  : in String;
                             Level : in Natural) is
      begin

         for I in 1 .. Level loop
            Put ("   ");
         end loop;

         Put (Name);
         Put (" (");
         Put (Element (Places, Name));
         Put (" ");
         Put (Element (Dates, Name));
         Put_Line (")");

         declare
            MC : constant Date_Ordered_Set_Maps.Cursor_Type :=
              Find (Students, Name);
         begin
            if MC = Back (Students) then
               return;
            end if;

            declare
               procedure Process (J : in Date_Ordered_Sets.Cursor_Type) is
               begin
                  Iterate (J, Level => Level + 1);
               end;

               procedure Iterate is
                  new Date_Ordered_Sets.Generic_Iteration;
            begin
               Iterate (Set => To_Access (MC).all);
            end;
         end;

      end Output_Tree;

      procedure Process (J : in Date_Ordered_Sets.Cursor_Type) is
      begin
         Iterate (J, Level => 0);
      end;

      procedure Iterate is
         new Date_Ordered_Sets.Generic_Iteration;

      Roots : Date_Ordered_Sets.Set_Type renames
        To_Access (Find (Students, Key => "---")).all;

   begin

      Iterate (Set => Roots);

   end Output_Roots;



end Genealogy;

