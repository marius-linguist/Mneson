-- PACKAGE MNESON.ORDER (GENERIC BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

package body Mneson.Order is

   function ">" (Left, Right : Element_Type) return Boolean is
   begin
      return Right < Left;
   end;

   function "<=" (Left, Right : Element_Type) return Boolean is
   begin
      return Left < Right or Left = Right;
   end;

   function ">=" (Left, Right : Element_Type) return Boolean is
   begin
      return Left > Right or Left = Right;
   end;

   function "<" (Left, Right : One_Dimensional_Array) return Boolean is
   begin
      if Left = Right then return False;
      elsif Left'Length = 0 then return True;
      elsif Right'Length = 0 then return False;
      elsif Left (Left'First) < Right (Right'First) then return True;
      elsif Left (Left'First) > Right (Right'First) then return False;
      else return
         Left (Left'First + 1 .. Left'Last) <
         Right (Right'First + 1 .. Right'Last);
      end if;
   end;

   function ">" (Left, Right : One_Dimensional_Array) return Boolean is
   begin
      return Right < Left;
   end;

   function "<=" (Left, Right : One_Dimensional_Array) return Boolean is
   begin
      return Left < Right or Left = Right;
   end;

   function ">=" (Left, Right : One_Dimensional_Array) return Boolean is
   begin
      return Left < Right or Left = Right;
   end;

end;