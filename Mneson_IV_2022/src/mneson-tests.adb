-- PACKAGE MNESON.TESTS (BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

--with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Mneson.Tests is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   Current_Level : Level := Level'First;
   Count : array (Level'Range) of Natural := (others => 0);
   Total : Natural := 0;
   Asserted : Natural := 0;
   Failed : Natural := 0;

   procedure Inc (X : in out Integer) is begin X := X + 1; end;
   procedure Dec (X : in out Integer) is begin X := X - 1; end;

   function Img (X : Integer) return String is
      S : String := Integer'Image (X);
   begin
      return S (2 .. S'Length);
   end;
   
   procedure Increase_Level is
   begin
      Inc (Current_Level);
      Count (Current_Level) := 0;
   end;
   
   procedure Decrease_Level is
   begin
      Dec (Current_Level);
   end;
   
   procedure New_Test (Name : String := "(unamed)") is
   begin
      Inc (Total);
      Inc (Count (Current_Level));
      New_Line;
      Put ("Test ");
      for I in 1 .. Current_Level loop
         if I > 1 then Put ('.'); end if;
         Put (Img (Count (I)));
      end loop;
      Put ("  " & Name & " " );
   end;
   
   procedure Fail is
   begin
      Put ("(FAIL)");
      Inc (Failed);
      Inc (Asserted);
   end;
   
   procedure Fatal is
   begin
      Fail;
      New_Line;
      Put ("***FATAL FAILURE--TESTS TERMINATED***");
      raise Done;
   end;
   
   procedure Pass is
   begin
      Put ("(PASS)");
      Inc (Asserted);
   end;
   
   procedure Report is
   begin
      New_Line;
      Put_Line (Img (Total) & " tests and test groups");
      Put_Line (Img (Asserted) & " tests");
      Put_Line (Img (Asserted - Failed) & " passed");
      Put (Img (Failed) & " failed :-");
      if Failed = 0 then Put (")"); else Put ("("); end if;
      New_Line;
   end;

end;
