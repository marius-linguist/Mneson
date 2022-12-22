generic

   type Iterator_Type is private;

   with function Succ (I : Iterator_Type)
     return Iterator_Type is <>;

   with function Is_Equal (I, J : Iterator_Type)
     return Boolean is <>;

   with function "=" (L, R : Iterator_Type)
     return Boolean is <>;

function Charles.Algorithms.Generic_Adjacent_Find
  (First, Back : Iterator_Type) return Iterator_Type;

pragma Pure (Charles.Algorithms.Generic_Adjacent_Find);

