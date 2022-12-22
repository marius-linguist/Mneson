with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Latin_1;  use Ada.Characters;

package body Parser is

   File : File_Type;
   
   Line : String (1 .. 256); --?
   
   type Info_Type is 
      record
         First, Last : Positive;
      end record;
   
   Student_Info, Advisor_Info, Place_Info, Date_Info : Info_Type;
   

   procedure Initialize (File : in String) is
   begin
      Open (Parser.File, Mode => In_File, Name => File);
   end;
   

   procedure Get_Info 
     (Info  :    out Info_Type;
      First : in out Positive;
      Last  : in     Positive) is
      
      pragma Assert (First <= Last);

   begin
      
      Info.First := First;
      
      loop
         
         Info.Last := First;
         
         First := First + 1;

         exit when First > Last or else Line (First) = Latin_1.HT;
         
      end loop;
      
      while First <= Last and then Line (First) = Latin_1.HT loop

         First := First + 1;

      end loop;

   end Get_Info;
   

   function Get_Record return Boolean is
      
      Last  : Natural;
      Index : Positive := Line'First;

   begin
      
      loop

         if End_Of_File (File) then
            return False;
         end if;
         
         Get_Line (File, Line, Last);
         
         if Last >= Line'First 
           and then Line (Line'First) /= '#'
         then
            exit;
         end if;
         
      end loop;
      
      Get_Info (Student_Info, Index, Last);
      Get_Info (Advisor_Info, Index, Last);
      Get_Info (Place_Info, Index, Last);
      Get_Info (Date_Info, Index, Last);
      
      return True;
      
   end Get_Record;
   

   function Student return String is
   begin
      return Line (Student_Info.First .. Student_Info.Last);
   end;
   

   function Advisor return String is
      Result : String renames 
        Line (Advisor_Info.First .. Advisor_Info.Last);
   begin
      if Result = "?" then
         return "---";
      end if;

      return Result;
   end;
   

   function Place return String is
   begin
      return Line (Place_Info.First .. Place_Info.Last);
   end;
   

   function Date return String is
   begin
      return Line (Date_Info.First .. Date_Info.Last);
   end;
   

end Parser;

