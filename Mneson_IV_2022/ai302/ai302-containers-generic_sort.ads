generic

   type Cursor_Type is private;

   --with procedure Debug (Iter : Iterator_Type; Text : String);
   --with procedure Debug (Text : String);

   with function Is_Less
     (Left, Right : Cursor_Type) return Boolean is <>;

   with procedure Swap
     (Left, Right : Cursor_Type) is <>;

   with function "+"
     (Left  : Cursor_Type;
      Right : Integer'Base) return Cursor_Type is <>;

   with function "-"
     (Left  : Cursor_Type;
      Right : Integer'Base) return Cursor_Type is <>;

   with function "-"
     (Left, Right : Cursor_Type) return Integer'Base is <>;

   with function "<"
     (Left, Right : Cursor_Type) return Boolean is <>;

   with function "="
     (Left, Right : Cursor_Type) return Boolean is <>;

procedure AI302.Containers.Generic_Sort (First, Back : in Cursor_Type);

pragma Pure (AI302.Containers.Generic_Sort);




