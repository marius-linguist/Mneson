-- PACKAGE XML_AUTOMATON (SPEC)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic

  with procedure Output is <>;

package Xml_Automaton is

  type Token_Kind_Type is (
    Start_Tag,
    End_Tag,
    Empty_Element_Tag,
    Markup_Declaration,
    Processing_Instruction,
    Content_Token,
    White_Space,
    Tag,
    Non_End_Tag,
    Null_Token_Kind);

  type State_Type is (
    In_A_Limbo,
    Reading_Content,
    Expecting_Element_Name_Or_Special_Character,
    Reading_Element_Name_Of_End_Tag,
    Reading_Element_Name_Of_Non_End_Tag,
    Expecting_Start_Of_Attribute_Or_End_Of_Tag,
    Reading_Attribute_Name,
    Expecting_Equal_Sign_Or_Start_Of_Attribute_Or_End_Of_Tag,
    Expecting_Attribute_Value_Delimiter,
    Reading_Attribute_Value,
    Require_WS_GT,
    Require_GT,
    Reading_Tag_Content,
    Null_State);

  Token_Kind: Token_Kind_Type := Null_Token_Kind;
  State: State_Type := In_A_Limbo;
  Return_State: State_Type := Null_State;
  Element_Name: Unbounded_String;
  Content: Unbounded_String;
  Attribute_Name: array(1..1000) of Unbounded_String;
  Attribute_Value: array(Attribute_Name'Range) of Unbounded_String;
  Attribute_Value_Delimiter: array(Attribute_Name'Range) of Character;
  Attribute_Count: Natural range 0..Attribute_Name'Last := 0;
  New_Attribute: Natural range 0..Attribute_Name'Last := 0;

  procedure Input(C: in Character);
  procedure Reset;
  procedure Finish;
  function Format return String;

  Null_Name_Error: exception;
  Too_Many_Attributes: exception;
  Error: exception;

end Xml_Automaton;
