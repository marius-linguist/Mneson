generic

   type Index_Type is (<>);

   type Element_Type is private;

   type Array_Type is array (Index_Type range <>) of Element_Type;

   with function "<" (Left, Right : Element_Type) return Boolean is <>;

procedure AI302.Containers.Generic_Sort_Unconstrained_Array
  (Source : in out Array_Type);

pragma Pure (AI302.Containers.Generic_Sort_Unconstrained_Array);



