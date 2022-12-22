package Parser is

   pragma Elaborate_Body;

   procedure Initialize (File : in String);

   function Get_Record return Boolean;

   function Student return String;

   function Advisor return String;

   function Place return String;

   function Date return String;

end Parser;

