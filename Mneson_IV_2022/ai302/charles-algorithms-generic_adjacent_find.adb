function Charles.Algorithms.Generic_Adjacent_Find
  (First, Back : Iterator_Type) return Iterator_Type is

   Iter, Next : Iterator_Type := First;
   
begin
   
   if Iter = Back then
      return Back;
   end if;
   
   loop
      
      Next := Succ (Next);
   
      if Next = Back then
         return Back;
      end if;
      
      if Is_Equal (Iter, Next) then
         return Iter;
      end if;
      
      Iter := Next;
      
   end loop;
   
end Charles.Algorithms.Generic_Adjacent_Find;




