function To_Hex_Key (I : Positive) return String is

   Result : String (1 .. Integer'Width);
   Index : Natural := Result'Last;

   D : Natural := I;
   R : Natural;
   
   Map : constant array (0 .. 15) of Character := "0123456789abcedf";
       
begin

   loop
   
      R := D rem 16;
      
      Result (Index) := Map (R);
      
      D := D / 16;
      
      exit when D = 0;
      
      Index := Index - 1;
      
   end loop;
   
   return Result (Index .. Result'Last);
   
end To_Hex_Key;

   
   
         