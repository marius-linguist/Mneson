-- PACKAGE MNESON.TESTS (SPEC)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Mneson.Base;

package Mneson.Tests is

   package Base is new Mneson.Base;

   subtype Level is Positive range 1 .. 10;
   Done : exception;
      
   procedure Increase_Level;
   procedure Decrease_Level;
   procedure New_Test (Name : String := "(unamed)");
   procedure Fail;
   procedure Fatal;
   procedure Pass;
   procedure Report;
   
end;
