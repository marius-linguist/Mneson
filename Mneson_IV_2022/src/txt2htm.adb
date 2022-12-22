-- PROCEDURE TXT2HTM (BODY ONLY)
-- Creates HTML with numbered headings and paragraphs from text.
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.IO_Aux;

procedure Txt2htm is

   use Ada.Strings.Unbounded;

   Level : Positive := 1;
   Prefix : Unbounded_String;
   Separator : Unbounded_String := To_Unbounded_String (".");
   Paragraph : Unbounded_String;
   Section_Count : Natural := 0;
   Paragraph_Count : Natural := 0;
   Newline : String := (1 => ASCII.LF);

   function Img (X : Integer) return String is
      S : String := Integer'Image (X);
   begin
      return S (2 .. S'Last);
   end;

   procedure Inc (X : in out Integer) is begin X := X + 1; end;

   use Ada.Text_IO;

   procedure Put_Block (Number, Text: String) is
   begin
      if Line > 1 then New_Line; end if;
      Put_Line
        ("<table width=""100%""><tr valign=""top"">" &
         "<td width=""7%"" align=""right"">" &
         "<small>" & Number & "</small>&emsp;&emsp;" &
--         "<td width=""4%"">" &
         "<td width=""93%"">" & Text & "</table>");
   end;

   procedure Put_Heading (Level : Positive; S : String) is
   begin
      Put_Block
        ("", "<H" & Img (Level) & ">" & S & "</H" & Img (Level) & ">");
      Paragraph_Count := 0;
   end;

   procedure Put_Paragraph (S : String) is
   begin
      Inc (Paragraph_Count);
      Put_Block
         (Img (Paragraph_Count), S);
   end;

   procedure Flush_Paragraph is
   begin
     if Paragraph /= Null_Unbounded_String then
        Put_Paragraph (To_String (Paragraph));
        Paragraph := Null_Unbounded_String;
     end if;
   end;

   procedure Put_Headword (S : String) is
   begin
      Append (Paragraph, "<b>" & S & "</b><br />");
   end;

   type Position is (Aligned, Indented, Centred);

   procedure Put_Unumbered_Heading (S : String; Pos : Position) is
      P : Unbounded_String := To_Unbounded_String ("<p");
   begin
      if Pos = Centred then Append (P, " align=""center"""); end if;
      Append (P, ">");
      if Pos = Indented then Append (P, "&emsp;&emsp;"); end if;
      Append (P, "<i><small>" & S & "</small></i>");
      Put_Block ("", To_String (P));
   end;

begin

   declare
      use Ada.Command_Line;
   begin
      Level := Positive'Value (Argument (1));
      Prefix := To_Unbounded_String (Argument (2));
      Separator := To_Unbounded_String (Argument (3));
   exception
      when Constraint_Error => Null;
   end;

   while not End_Of_File loop
      declare
         S : String := GNAT.IO_Aux.Get_Line;
      begin
         if Line = 1 then
            declare
               P : Unbounded_String := Prefix;
            begin
               if P /= Null_Unbounded_String then
                  Append (P, "&emsp;");
               end if;
               Put_Heading (Level, To_String (P) & S);
            end;
         else
            if S'Length = 0 then
               Flush_Paragraph;
            else
               if S (1) = '*' and S (S'Last) = '*' then
                  Inc (Section_Count);
                  Paragraph_Count := 0;
                  declare
                     P : Unbounded_String := Prefix;
                  begin
                     if P /= Null_Unbounded_String then
                        Append (P, Separator);
                     end if;
                     Put_Heading
                       (Level + 1,
                        To_String (P) &
                        Img (Section_Count) &
                        "&emsp;" &
                        S (3 .. S'Last - 2));
                  end;
               elsif S'Length >= 2
                  and then (S (1) = ':' and S (S'Last) =  ':') then
                     Put_Headword (S (2 .. S'Last - 1));
               elsif S'Length >= 2
                  and then (S (1) = '(' and S (S'Last) = ')') then
                     Put_Unumbered_Heading
                       (S (2 .. S'Last - 1), Aligned);
               elsif S'Length >= 3
                  and then (S (1 .. 2) = " (" and S (S'Last) = ')') then
                     Put_Unumbered_Heading
                       (S (3 .. S'Last - 1), Indented);
               elsif S'Length >= 4
                  and then (S (1 .. 3) = "  (" and S (S'Last) = ')') then
                     Put_Unumbered_Heading
                       (S (4 .. S'Last - 1), Centred);
               else
                  if Paragraph /= Null_Unbounded_String then
                     Append (Paragraph, Newline);
                  end if;
                  Append (Paragraph, S);
               end if;
            end if;
         end if;
      end;
   end loop;
   Flush_Paragraph;

end;

-- NOTES
-- See file "txt2htm.txt", which is in Txt2htm format.

-- REVISION HISTORY
-- 20041124 created and tested
-- 20041125 improved, tested
