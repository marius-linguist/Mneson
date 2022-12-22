package body Sorted_Maps is

   use Rep_Types;

   function Is_Less (L, R : Pair_Type) return Boolean is
   begin
      return L.Key < R.Key;
   end;


   function Is_Equal (L, R : Pair_Type) return Boolean is
   begin
      return L.Element = R.Element;
   end;


   function "<" (L : Key_Type; R : Pair_Type) return Boolean is
   begin
      return L < R.Key;
   end;


   function ">" (L : Key_Type; R : Pair_Type) return Boolean is
   begin
      return R.Key < L;
   end;


   package Key_Types is
      new Rep_Types.Generic_Keys (Key_Type);

   use Key_Types;


   function Null_Cursor return Cursor_Type is
   begin
      return Cursor_Type (Rep_Types.Null_Cursor);
   end;


   function "=" (Left, Right : Map_Type) return Boolean is
   begin
      return ST (Left) = ST (Right);
   end;


   function Length (Map : Map_Type) return Natural is
   begin
      return Natural (Length (ST (Map)));
   end;


   function Is_Empty (Map : Map_Type) return Boolean is
   begin
      return Is_Empty (ST (Map));
   end;


   procedure Clear (Map : in out Map_Type) is
   begin
      Clear (ST (Map));
   end;


   procedure Swap (Left, Right : in out Map_Type) is
   begin
      Swap (ST (Left), ST (Right));
   end;


   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin
      Insert
        (Set      => ST (Map),
         New_Item => Pair_Type'(Key, New_Item),
         Cursor   => Rep_Types.Cursor_Type (Cursor),
         Success  => Success);
   end;


   procedure Insert (Map      : in out Map_Type;
                     Position : in     Cursor_Type;
                     Key      : in     Key_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin
      Insert
        (Set      => ST (Map),
         Position => Rep_Types.Cursor_Type (Position),
         New_Item => Pair_Type'(Key, New_Item),
         Cursor   => Rep_Types.Cursor_Type (Cursor),
         Success  => Success);
   end;



   type Pair_Access is access all Pair_Type;
   for Pair_Access'Storage_Size use 0;

   function To_Access is
      new Rep_Types.Generic_Element (Pair_Access);

   function To_Access
     (Cursor : Cursor_Type) return Pair_Access is
   begin
      return To_Access (Rep_Types.Cursor_Type (Cursor));
   end;


   procedure Replace (Map      : in out Map_Type;
                      Key      : in     Key_Type;
                      New_Item : in     Element_Type) is

      C : Cursor_Type;
      B : Boolean;
   begin
      Insert (Map, Key, New_Item, C, B);

      if not B then
         declare
            P : Pair_Type renames To_Access (C).all;
         begin
            P.Element := New_Item;
         end;
      end if;
   end Replace;


   procedure Set_Key
     (Pair : in out Pair_Type;
      Key  : in     Key_Type) is
   begin
      Pair.Key := Key;
   end;

   package Key_Insertion is
      new Key_Types.Generic_Insertion (Set_Key);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin
      Key_Insertion.Insert
        (Set      => Rep_Types.Set_Type (Map),
         Key      => Key,
         Cursor   => Rep_Types.Cursor_Type (Cursor),
         Success  => Success);
   end;


   procedure Insert (Map      : in out Map_Type;
                     Position : in     Cursor_Type;
                     Key      : in     Key_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin
      Key_Insertion.Insert
        (Set      => Rep_Types.Set_Type (Map),
         Position => Rep_Types.Cursor_Type (Position),
         Key      => Key,
         Cursor   => Rep_Types.Cursor_Type (Cursor),
         Success  => Success);
   end;


   procedure Delete (Map    : in out Map_Type;
                     Cursor : in out Cursor_Type) is
   begin
      Delete (ST (Map), Rep_Types.Cursor_Type (Cursor));
   end;


   procedure Delete (Map : in out Map_Type;
                     Key : in     Key_Type) is

   begin
      Key_Types.Delete (Rep_Types.Set_Type (Map), Key);
   end;



   function Find (Map : Map_Type;
                  Key : Key_Type)
      return Cursor_Type is

      C : constant Rep_Types.Cursor_Type :=
        Key_Types.Find (Rep_Types.Set_Type (Map), Key);
   begin
      return Cursor_Type (C);
   end;

   function Is_In (Key : Key_Type;
                   Map : Map_Type)
      return Boolean is
   begin
      return Find (Map, Key) /= Null_Cursor;
   end;


   function Element (Map : Map_Type;
                     Key : Key_Type)
     return Element_Type is
   begin
      return Element (Find (Map, Key));
   end;


   function First (Map : Map_Type) return Cursor_Type is
   begin
      return Cursor_Type (First (ST (Map)));
   end;


   function Back (Map : Map_Type) return Cursor_Type is
   begin
      return Cursor_Type (Back (ST (Map)));
   end;


   function Succ
     (Cursor : Cursor_Type) return Cursor_Type is

      C : constant Rep_Types.Cursor_Type :=
        Succ (Rep_Types.Cursor_Type (Cursor));
   begin
      return Cursor_Type (C);
   end;


   procedure Increment
     (Cursor : in out Cursor_Type) is
   begin
      Cursor := Succ (Cursor);
   end;


   function Key (Cursor : Cursor_Type) return Key_Type is

      PA : constant Pair_Access := To_Access (Cursor);
   begin
      return PA.Key;
   end;


   function Generic_Key (Cursor : Cursor_Type)
      return Key_Access is

      PA : constant Pair_Access := To_Access (Cursor);
   begin
      return PA.Key'Access;
   end;


   function Element (Cursor : Cursor_Type)
     return Element_Type is

      PA : constant Pair_Access := To_Access (Cursor);
   begin
      return PA.Element;
   end;


   function Generic_Element (Cursor : Cursor_Type)
      return Element_Access is

      PA : constant Pair_Access := To_Access (Cursor);
   begin
      return PA.Element'Access;
   end;


   procedure Replace_Element (Cursor : in Cursor_Type;
                              By     : in Element_Type) is

      PA : constant Pair_Access := To_Access (Cursor);
   begin
      PA.Element := By;
   end;


   procedure Generic_Iteration (Map : in Map_Type) is

      procedure Process (Cursor : in Rep_Types.Cursor_Type) is
         pragma Inline (Process);
      begin
         Process (Cursor_Type (Cursor));
      end;

      procedure Iterate is
         new Rep_Types.Generic_Iteration (Process);
   begin
      Iterate (Rep_Types.Set_Type (Map));
   end;



   procedure Delete_Sans_Increment (Map    : in out Map_Type;
                                    Cursor : in out Cursor_Type) is
   begin
      Delete_Sans_Increment (ST (Map), Rep_Types.Cursor_Type (Cursor));
   end;



   procedure Delete_First (Map : in out Map_Type) is
   begin
      Delete_First (ST (Map));
   end;


   procedure Delete_Last (Map : in out Map_Type) is
   begin
      Delete_Last (ST (Map));
   end;


   function Lower_Bound (Map : Map_Type;
                         Key : Key_Type) return Cursor_Type is

      C : constant Rep_Types.Cursor_Type :=
        Key_Types.Lower_Bound (Rep_Types.Set_Type (Map), Key);
   begin
      return Cursor_Type (C);
   end;



   function Upper_Bound (Map : Map_Type;
                         Key : Key_Type) return Cursor_Type is

      C : constant Rep_Types.Cursor_Type :=
        Key_Types.Upper_Bound (Rep_Types.Set_Type (Map), Key);
   begin
      return Cursor_Type (C);
   end;


   function First_Key (Map : Map_Type) return Key_Type is
      C : constant Cursor_Type := First (Map);
   begin
      return To_Access (C).Key;
   end;


   function First_Element (Map : Map_Type) return Element_Type is
      C : constant Cursor_Type := First (Map);
   begin
      return To_Access (C).Element;
   end;


   function Last (Map : Map_Type) return Cursor_Type is
   begin
      return Cursor_Type (Last (ST (Map)));
   end;


   function Last_Key (Map : Map_Type) return Key_Type is
      C : constant Cursor_Type := Last (Map);
   begin
      return To_Access (C).Key;
   end;



   function Last_Element (Map : Map_Type) return Element_Type is
      C : constant Cursor_Type := Last (Map);
   begin
      return To_Access (C).Element;
   end;


   function Pred (Cursor : Cursor_Type) return Cursor_Type is
   begin
      return Cursor_Type (Pred (Rep_Types.Cursor_Type (Cursor)));
   end;


   procedure Decrement (Cursor : in out Cursor_Type) is
   begin
      Cursor := Pred (Cursor);
   end;


   procedure Generic_Reverse_Iteration (Map : in Map_Type) is

      procedure Process (Cursor : in Rep_Types.Cursor_Type) is
         pragma Inline (Process);
      begin
         Process (Cursor_Type (Cursor));
      end;

      procedure Iterate is
         new Rep_Types.Generic_Reverse_Iteration (Process);
   begin
      Iterate (Rep_Types.Set_Type (Map));
   end;



   function "<" (Left, Right : Cursor_Type) return Boolean is
   begin
      return Rep_Types.Cursor_Type (Left) < Rep_Types.Cursor_Type (Right);
   end;


   function "<" (Left : Cursor_Type; Right : Key_Type)
      return Boolean is
   begin
      return Rep_Types.Cursor_Type (Left) < Right;
   end;


   function "<" (Left : Key_Type; Right : Cursor_Type)
      return Boolean is
   begin
      return Left < Rep_Types.Cursor_Type (Right);
   end;



end Sorted_Maps;



