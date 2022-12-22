-- PACKAGE MNESON.MONITORS (BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Text_IO;

package body Mneson.Monitors is

   procedure Null_Monitor (Message : String) is
   begin
      null;
   end;

   procedure Standard_Error_Monitor (Message : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Message);
   end;

end;
