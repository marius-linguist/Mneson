with Charles.Algorithms.Generic_Lower_Bound;
pragma Elaborate_All (Charles.Algorithms.Generic_Lower_Bound);  --necessary?

package body AI302.Containers.Prime_Numbers is


   function Is_Less
     (Index : in Positive;
      Item  : in Size_Type) return Boolean is

      pragma Inline (Is_Less);
   begin
      return Primes (Index) < Hash_Type (Item);
   end;


   function Lower_Bound is
      new Charles.Algorithms.Generic_Lower_Bound
        (Iterator_Type   => Positive,
         Difference_Type => Integer,
         Element_Type    => Size_Type,
         Is_Less         => Is_Less);


   function To_Prime (Length : Size_Type) return Hash_Type is

      I : constant Positive :=
        Lower_Bound (Primes'First, Primes'Last, Length);
   begin
      return Primes (I);
   end;


end AI302.Containers.Prime_Numbers;
