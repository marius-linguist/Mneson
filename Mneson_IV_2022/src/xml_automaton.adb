-- PACKAGE XML_AUTOMATON (BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Xml_Automaton is

   function Is_White_Space (C: in Character) return Boolean is
      use Ada.Characters.Latin_1;
   begin
      return (C = ' ' or C = CR or C = LF or C = HT);
   end;

   function Is_White_Space (U : Unbounded_String) return Boolean is
   begin
      for I in 1 .. Length (U) loop
         if not Is_White_Space (Element (U, I)) then return False; end if;
      end loop;
      return True;
   end;

procedure Finish_Element_Name is
begin
  if Length(Element_Name) = 0 then
    raise Null_Name_Error;
  end if;
end Finish_Element_Name;

procedure Prepare_New_Attribute is
begin
  begin
    New_Attribute := Attribute_Count + 1;
  exception
    when Constraint_Error =>
    raise Too_Many_Attributes;
  end;
  Attribute_Name(New_Attribute) := Null_Unbounded_String;
  Attribute_Value(New_Attribute) := Null_Unbounded_String;
end Prepare_New_Attribute;

procedure Finish_Attribute is
begin
  if Length(Attribute_Name(New_Attribute)) > 0 then
    Attribute_Count := Attribute_Count + 1;
  else
    raise Null_Name_Error;
  end if;
end Finish_Attribute;

procedure Finish is
begin
  if State = Reading_Content then
    Output;
    Reset;
  elsif State = In_A_Limbo then
    null;
  else
    raise Error;
  end if;
end Finish;

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-------------------------------           ---------------------------------
-------------------------------   Input   ---------------------------------
-------------------------------           ---------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------

procedure Input(C: in Character) is
   procedure Output_Content is
   begin
      if Is_White_Space (Content) then
         Token_Kind := White_Space;
      end if;
      Output;
   end;
begin
  if State = In_A_Limbo then
    if C = '<' then
      Token_Kind := Tag;
      State := Expecting_Element_Name_Or_Special_Character;
    elsif C = '>' then
      raise Error;
    else
      Token_Kind := Content_Token;
      Content := To_Unbounded_String(C & "");
      State := Reading_Content;
    end if;
  elsif State = Reading_Content then
    if C = '<' then
      Output_Content;
      Reset;
      Token_Kind := Tag;
      State := Expecting_Element_Name_Or_Special_Character;
    elsif C = '>' then
      raise Error;
    else
      Append(Content, C);
    end if;
  elsif State = Expecting_Element_Name_Or_Special_Character then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      raise Null_Name_Error;
    elsif C = '/' then
      Token_Kind := End_Tag;
      State := Reading_Element_Name_Of_End_Tag;
    elsif C = '!' then
      Token_Kind := Markup_Declaration;
      Content := To_Unbounded_String("!");
      State := Reading_Tag_Content;
    elsif C = '?' then
      Token_Kind := Processing_Instruction;
      Content := To_Unbounded_String("?");
      State := Reading_Tag_Content;
    elsif Is_White_Space(C) then
      raise Error;
    else
      Element_Name := To_Unbounded_String(C & "");
      Token_Kind := Non_End_Tag;
      State := Reading_Element_Name_Of_Non_End_Tag;
    end if;
  elsif State = Reading_Element_Name_Of_End_Tag then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Finish_Element_Name;
      Output;
      Reset;
    elsif C = '/' then
      raise Error;
    elsif Is_White_Space(C) then
      Finish_Element_Name;
      State := Require_WS_GT;
    else
      Append(Element_Name, C);
    end if;
  elsif State =  Reading_Element_Name_Of_Non_End_Tag then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Finish_Element_Name;
      Token_Kind := Start_Tag;
      Output;
      Reset;
    elsif C = '/' then
      Finish_Element_Name;
      Token_Kind := Empty_Element_Tag;
      State := Require_GT;
    elsif Is_White_Space(C) then
      Finish_Element_Name;
      State := Expecting_Start_Of_Attribute_Or_End_Of_Tag;
    else
      Append(Element_Name, C);
    end if;
  elsif State = Expecting_Start_Of_Attribute_Or_End_Of_Tag then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Token_Kind := Start_Tag;
      Output;
      Reset;
    elsif C = '/' then
      Token_Kind := Empty_Element_Tag;
      State := Require_GT;
    elsif C = '=' then
      raise Error;
    elsif Is_White_Space(C) then
      null;
    else
      Prepare_New_Attribute;
      Attribute_Name(New_Attribute) := To_Unbounded_String(C & "");
      State := Reading_Attribute_Name;
    end if;
  elsif State = Reading_Attribute_Name then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Finish_Attribute;
      Token_Kind := Start_Tag;
      Output;
      Reset;
    elsif C = '/' then
      Finish_Attribute;
      Token_Kind := Empty_Element_Tag;
      State := Require_Gt;
    elsif C = '=' then
      State := Expecting_Attribute_Value_Delimiter;
    elsif Is_White_Space(C) then
      State := Expecting_Equal_Sign_Or_Start_Of_Attribute_Or_End_Of_Tag;
    else
      Append(Attribute_Name(New_Attribute), C);
    end if;
  elsif State = Expecting_Equal_Sign_Or_Start_Of_Attribute_Or_End_Of_Tag then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Finish_Attribute;
      Token_Kind := Start_Tag;
      Output;
      Reset;
    elsif C = '/' then
      Finish_Attribute;
      Token_Kind := Empty_Element_Tag;
      State := Require_Gt;
    elsif C = '=' then
      State := Expecting_Attribute_Value_Delimiter;
    elsif Is_White_Space(C) then
      null;
    else
      Finish_Attribute;
      Prepare_New_Attribute;
      Attribute_Name(New_Attribute) := To_Unbounded_String(C & "");
      State := Reading_Attribute_Name;
    end if;
  elsif State = Expecting_Attribute_Value_Delimiter then
    if C = '"' or C = ''' then
      Attribute_Value_Delimiter(New_Attribute) := C;
      State := Reading_Attribute_Value;
    elsif Is_White_Space(C) then
      null;
    else
      raise Error;
    end if;
  elsif State = Reading_Attribute_Value then
    if C = Attribute_Value_Delimiter(New_Attribute) then
      Finish_Attribute;
      State := Expecting_Start_Of_Attribute_Or_End_Of_Tag;
    else
      Append(Attribute_Value(New_Attribute), C);
    end if;
  elsif State = Require_Ws_Gt then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Output;
      Reset;
    elsif Is_White_Space(C) then
      null;
    else
      raise Error;
    end if;
  elsif State = Require_Gt then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Output;
      Reset;
    else
      raise Error;
    end if;
  elsif State = Reading_Tag_Content then
    if C = '<' then
      raise Error;
    elsif C = '>' then
      Output;
      Reset;
    else
      Append(Content, C);
    end if;
  else
    raise Program_Error;
  end if;
end Input;

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-------------------------------           ---------------------------------
-------------------------------   Reset   ---------------------------------
-------------------------------           ---------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------

procedure Reset is
begin
  State := In_A_Limbo;
  Token_Kind := Null_Token_Kind;
  Element_Name := Null_Unbounded_String;
  Content := Null_Unbounded_String;
  Attribute_Count := 0;
  New_Attribute := 0;
end Reset;

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-------------------------------           ---------------------------------
-------------------------------   Format  ---------------------------------
-------------------------------           ---------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------

function Format return String is
  U: Unbounded_String;
begin
  if Token_Kind = Content_Token then
    return(To_String(Content));
  else
    Append(U, '<');
    if Token_Kind = Processing_Instruction
    or Token_Kind = Markup_Declaration then
      Append(U, Content);
    else
      if Token_Kind = End_Tag then
        Append(U, '/');
      end if;
      if Token_Kind = Start_Tag
      or Token_Kind = End_Tag
      or Token_Kind = Empty_Element_Tag then
        Append(U, Element_Name);
        if Token_Kind = Start_Tag
        or Token_Kind = Empty_Element_Tag then
          for I in 1..Attribute_Count loop
            Append(U,
              " " & Attribute_Name(I) & "=" &
              Attribute_Value_Delimiter(I) &
              Attribute_Value(I) &
              Attribute_Value_Delimiter(I));
          end loop;
          if Token_Kind = Empty_Element_Tag then
            Append(U, '/');
          end if;
        end if;
      end if;
    end if;
    Append(U, '>');
    return(To_String(U));
  end if;
end Format;

begin

  Reset;

end Xml_Automaton;
