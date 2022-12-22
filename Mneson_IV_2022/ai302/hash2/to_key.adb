function To_Key 
  (Item : Natural;
   Base : Positive) return String is

   Result : String (1 .. 64);  --max for base 2, 64-bit integer
   Index : Natural := Result'Last;

   D : Natural := Item;
   R : Natural;
   
   Map : constant array (0 .. 15) of Character := "0123456789abcdef";
       
begin

   loop
   
      R := D rem Base;
      
      Result (Index) := Map (R);
      
      D := D / Base;
      
      exit when D = 0;
      
      Index := Index - 1;
      
   end loop;
   
   return Result (Index .. Result'Last);

end To_Key;
   
