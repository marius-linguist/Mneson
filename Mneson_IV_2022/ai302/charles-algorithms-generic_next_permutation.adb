function Charles.Algorithms.Generic_Next_Permutation
  (First, Back : Iterator_Type) return Boolean is

   I, II, J : Iterator_Type;

begin

   if First = Back then
      return False;
   end if;

   I := Succ (First);

   if I = Back then
      return False;
   end if;

   I := Pred (Back);

   loop

      II := I;

      I := Pred (I);

      if Is_Less (I, II) then

         J := Back;

         loop

            J := Pred (J);

            exit when Is_Less (I, J);

         end loop;

         Swap (I, J);

         Reverse_Sequence (II, Back);

         return True;

      end if;

      if I = First then

         Reverse_Sequence (First, Back);

         return False;

      end if;

   end loop;

end Charles.Algorithms.Generic_Next_Permutation;



