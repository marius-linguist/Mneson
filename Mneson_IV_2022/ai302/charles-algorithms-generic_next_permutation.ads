generic

   type Iterator_Type is private;

   with function Succ (Iterator : Iterator_Type)
      return Iterator_Type is <>;

   with function Pred (Iterator : Iterator_Type)
     return Iterator_Type is <>;

   with function Is_Less (I, J : Iterator_Type)
     return Boolean is <>;

   with procedure Swap (I, J : in Iterator_Type) is <>;

   with procedure Reverse_Sequence (I, J : in Iterator_Type) is <>;

   with function "=" (L, R : Iterator_Type)
     return Boolean is <>;

function Charles.Algorithms.Generic_Next_Permutation
  (First, Back : Iterator_Type) return Boolean;

pragma Pure (Charles.Algorithms.Generic_Next_Permutation);


