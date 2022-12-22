with AI302.Containers;
with AI302.Containers.Sorted_Sets;

package Set_Algorithms_Shootout is

   type Integer is range 1 .. 1_000_000_000_000;

   package Integer_Sets is new AI302.Containers.Sorted_Sets (Integer);
   use Integer_Sets;
   use AI302.Containers;

   procedure Generate_Random_Set
      (Set : in out Set_Type;
       Size : Size_Type;
       Density : Float := 0.1);

   type Set_Array is array (Positive range <>) of Integer_Sets.Set_Type;

   generic
      with procedure Process (Element : Integer);
   procedure Standard_Intersect (S : Set_Array);

   generic
      with procedure Process (Element : Integer);
   procedure Jackpot_Intersect (S : Set_Array);

end;
