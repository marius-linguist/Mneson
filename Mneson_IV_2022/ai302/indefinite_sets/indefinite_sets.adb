with Ada.Unchecked_Deallocation;

package body Indefinite_Sets is

   --see note in spec
   function Null_Cursor return Cursor_Type is
   begin
      return Cursor_Type (Rep_Types.Null_Cursor);
   end;


   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);


   procedure Write
     (Stream  : access Root_Stream_Type'Class;
      Control : in     Control_Type) is
   begin
      Element_Type'Output (Stream, Control.X.all);
   end;


   procedure Read
     (Stream  : access Root_Stream_Type'Class;
      Control :    out Control_Type) is

      X : Element_Access := Control.X;
   begin
      Control.X := new Element_Type'(Element_Type'Input (Stream));
      Free (X);
   end;



   function "<"
     (Left : Element_Type; Right : Control_Type) return Boolean is
   begin
      return Left < Right.X.all;
   end;

   function ">"
     (Left : Element_Type; Right : Control_Type) return Boolean is

      -- e > c same as c < e
   begin
      return Right.X.all < Left;
   end;

   package Key_Types is
      new Rep_Types.Generic_Keys (Element_Type);

   use Rep_Types, Key_Types;


   function Is_Less (L, R : Control_Type) return Boolean is
   begin
      return L.X.all < R.X.all;
   end;


   function Is_Equal (L, R : Control_Type) return Boolean is
   begin
      return L.X.all = R.X.all;
   end;


   procedure Adjust (Control : in out Control_Type) is
      X : constant Element_Access := Control.X;
   begin
      Control.X := new Element_Type'(X.all);
   exception
      when others =>
         Control.X := null;
         raise;
   end Adjust;



   procedure Finalize (Control : in out Control_Type) is
   begin
      Free (Control.X);
   exception
      when others =>
         Control.X := null;
         raise;
   end Finalize;


   function "=" (Left, Right : Set_Type) return Boolean is
   begin
      return Rep_Type (Left) = Rep_Type (Right);
   end;


   function "<" (Left, Right : Set_Type) return Boolean is
   begin
      return Rep_Type (Left) < Rep_Type (Right);
   end;


   function "<=" (Left, Right : Set_Type) return Boolean is
   begin
      return Rep_Type (Left) <= Rep_Type (Right);
   end;


   function ">" (Left, Right : Set_Type) return Boolean is
   begin
      return Rep_Type (Left) > Rep_Type (Right);
   end;


   function ">=" (Left, Right : Set_Type) return Boolean is
   begin
      return Rep_Type (Left) >= Rep_Type (Right);
   end;


   function Is_Empty (Set : Set_Type) return Boolean is
   begin
      return Is_Empty (Rep_Type (Set));
   end;


   function Length (Set : Set_Type) return Natural is
   begin
      return Natural (Length (Rep_Type (Set)));
   end;


   procedure Clear (Set : in out Set_Type) is
   begin
      Clear (Rep_Type (Set));
   end;



   procedure Swap (Left, Right : in out Set_Type) is
   begin
      Swap (Rep_Type (Left), Rep_Type (Right));
   end;


   procedure Set_Key
     (Control : in out Control_Type;
      Key     : in     Element_Type) is

      pragma Assert (Control.X = null);
   begin
      Control.X := new Element_Type'(Key);
   end;

   package Key_Insertion is
      new Key_Types.Generic_Insertion (Set_Key);


   procedure Insert (Set      : in out Set_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin
      Key_Insertion.Insert
        (Set      => Rep_Types.Set_Type (Set),
         Key      => New_Item,
         Cursor   => Rep_Types.Cursor_Type (Cursor),
         Success  => Success);
   end;


   procedure Insert (Set      : in out Set_Type;
                     Position : in     Cursor_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin
      Key_Insertion.Insert
        (Set      => Rep_Types.Set_Type (Set),
         Position => Rep_Types.Cursor_Type (Position),
         Key      => New_Item,
         Cursor   => Rep_Types.Cursor_Type (Cursor),
         Success  => Success);
   end;


   procedure Delete (Set  : in out Set_Type;
                     Item : in     Element_Type) is
   begin
      Delete (Rep_Types.Set_Type (Set), Key => Item);
   end;

   procedure Delete (Set    : in out Set_Type;
                     Cursor : in out Cursor_Type) is
   begin
      Delete (Rep_Type (Set), Rep_Types.Cursor_Type (Cursor));
   end;


   procedure Delete_Sans_Increment (Set    : in out Set_Type;
                                    Cursor : in out Cursor_Type) is
   begin
      Delete_Sans_Increment (Rep_Type (Set), Rep_Types.Cursor_Type (Cursor));
   end;


   procedure Delete_First (Set : in out Set_Type) is
   begin
      Delete_First (Rep_Type (Set));
   end;


   procedure Delete_Last (Set : in out Set_Type) is
   begin
      Delete_Last (Rep_Type (Set));
   end;



   function Is_In (Item : Element_Type;
                   Set  : Set_Type) return Boolean is
   begin
      return Is_In (Item, Rep_Types.Set_Type (Set));
   end;


   function Find (Set  : Set_Type;
                  Item : Element_Type) return Cursor_Type is

      Result : constant Rep_Types.Cursor_Type :=
         Find (Rep_Types.Set_Type (Set), Item);
   begin
      return Cursor_Type (Result);
   end;


   function Lower_Bound (Set  : Set_Type;
                         Item : Element_Type) return Cursor_Type is

      Result : constant Rep_Types.Cursor_Type :=
        Lower_Bound (Rep_Types.Set_Type (Set), Item);
   begin
      return Cursor_Type (Result);
   end;


   function Upper_Bound (Set  : Set_Type;
                         Item : Element_Type) return Cursor_Type is

      Result : constant Rep_Types.Cursor_Type :=
         Upper_Bound (Rep_Types.Set_Type (Set), Item);
   begin
      return Cursor_Type (Result);
   end;


   function First (Set : Set_Type) return Cursor_Type is
   begin
      return Cursor_Type (First (Rep_Type (Set)));
   end;


   type Control_Access is access all Control_Type;

   function To_Access is
      new Rep_Types.Generic_Element (Control_Access);


   function First_Element (Set : Set_Type) return Element_Type is

      Cursor : constant Rep_Types.Cursor_Type :=
        First (Rep_Type (Set));

      Control : Control_Type renames
        To_Access (Cursor).all;
   begin
      return Control.X.all;
   end;


   function Last (Set : Set_Type) return Cursor_Type is
   begin
      return Cursor_Type (Last (Rep_Type (Set)));
   end;


   function Last_Element (Set : Set_Type) return Element_Type is

      C : constant Rep_Types.Cursor_Type :=
        Last (Rep_Type (Set));

      CA : constant Control_Access :=
        To_Access (C);
   begin
      return CA.X.all;
   end;


   function Back (Set : Set_Type) return Cursor_Type is
   begin
      return Cursor_Type (Back (Rep_Type (Set)));
   end;


   function Succ (Cursor : Cursor_Type) return Cursor_Type is
   begin
      return Cursor_Type (Succ (Rep_Types.Cursor_Type (Cursor)));
   end;


   function Pred (Cursor : Cursor_Type) return Cursor_Type is
   begin
      return Cursor_Type (Pred (Rep_Types.Cursor_Type (Cursor)));
   end;


   procedure Increment (Cursor : in out Cursor_Type) is
   begin
      Increment (Rep_Types.Cursor_Type (Cursor));
   end;


   procedure Decrement (Cursor : in out Cursor_Type) is
   begin
      Decrement (Rep_Types.Cursor_Type (Cursor));
   end;


   function "<" (Left, Right : Cursor_Type) return Boolean is
   begin
      return Rep_Types.Cursor_Type (Left) < Rep_Types.Cursor_Type (Right);
   end;


   function "<" (Left : Cursor_Type; Right : Element_Type)
      return Boolean is
   begin
      return Rep_Types.Cursor_Type (Left) < Right;
   end;


   function "<" (Left : Element_Type; Right : Cursor_Type)
      return Boolean is
   begin
      return Left < Rep_Types.Cursor_Type (Right);
   end;


   function Element (Cursor : Cursor_Type) return Element_Type is

      Control : Control_Type renames
        To_Access (Rep_Types.Cursor_Type (Cursor)).all;
   begin
      return Control.X.all;
   end;


   procedure Generic_Iteration (Set : in Set_Type) is

      procedure Process (C : Rep_Types.Cursor_Type) is
         pragma Inline (Process);
      begin
         Process (Cursor_Type (C));
      end;

      procedure Iterate is
         new Rep_Types.Generic_Iteration (Process);
   begin
      Iterate (Rep_Types.Set_Type (Set));
   end;


   procedure Generic_Reverse_Iteration (Set : in Set_Type) is

      procedure Process (C : Rep_Types.Cursor_Type) is
         pragma Inline (Process);
      begin
         Process (Cursor_Type (C));
      end;

      procedure Iterate is
         new Rep_Types.Generic_Reverse_Iteration (Process);
   begin
      Iterate (Rep_Types.Set_Type (Set));
   end;



end Indefinite_Sets;


