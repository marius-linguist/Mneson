with AI302.Containers.Sorted_Sets;
pragma Elaborate_All (AI302.Containers.Sorted_Sets);

with Ada.Streams;
with Ada.Finalization;

generic

   type Element_Type (<>) is private;

   with function "<" (L, R : Element_Type) return Boolean is <>;

   with function "=" (L, R : Element_Type) return Boolean is <>;

package Indefinite_Sets is

   pragma Preelaborate;

   type Set_Type is private;

   type Cursor_Type is private;

   --Null_Cursor : constant Cursor_Type;
   --can't preelaborate instantiation if this is a constant,
   --because it's non-static: grrrrr!
   --so do this instead:

   function Null_Cursor return Cursor_Type;
   pragma Inline (Null_Cursor);

   function "=" (Left, Right : Set_Type) return Boolean;

   function "<" (Left, Right : Set_Type) return Boolean;

   function "<=" (Left, Right : Set_Type) return Boolean;

   function ">" (Left, Right : Set_Type) return Boolean;

   function ">=" (Left, Right : Set_Type) return Boolean;

   function Length (Set : Set_Type) return Natural;

   function Is_Empty (Set : Set_Type) return Boolean;

   procedure Clear (Set : in out Set_Type);

   procedure Swap (Left, Right : in out Set_Type);

   procedure Insert (Set      : in out Set_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Insert (Set      : in out Set_Type;
                     Position : in     Cursor_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Delete (Set  : in out Set_Type;
                     Item : in     Element_Type);

   procedure Delete (Set    : in out Set_Type;
                     Cursor : in out Cursor_Type);

   procedure Delete_Sans_Increment (Set    : in out Set_Type;
                                    Cursor : in out Cursor_Type);

   procedure Delete_First (Set : in out Set_Type);

   procedure Delete_Last (Set : in out Set_Type);

   function Is_In (Item : Element_Type;
                   Set  : Set_Type) return Boolean;

   function Find (Set  : Set_Type;
                  Item : Element_Type) return Cursor_Type;

   function Lower_Bound (Set  : Set_Type;
                         Item : Element_Type) return Cursor_Type;

   function Upper_Bound (Set  : Set_Type;
                         Item : Element_Type) return Cursor_Type;

   function First (Set : Set_Type) return Cursor_Type;

   function First_Element (Set : Set_Type) return Element_Type;

   function Last (Set : Set_Type) return Cursor_Type;

   function Last_Element (Set : Set_Type) return Element_Type;

   function Back (Set : Set_Type) return Cursor_Type;

   function Succ (Cursor : Cursor_Type) return Cursor_Type;

   function Pred (Cursor : Cursor_Type) return Cursor_Type;

   procedure Increment (Cursor : in out Cursor_Type);

   procedure Decrement (Cursor : in out Cursor_Type);

   function "<" (Left, Right : Cursor_Type) return Boolean;

   function "<" (Left : Cursor_Type; Right : Element_Type)
      return Boolean;

   function "<" (Left : Element_Type; Right : Cursor_Type)
      return Boolean;

   function Element (Cursor : Cursor_Type) return Element_Type;

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Iteration (Set : in Set_Type);

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Reverse_Iteration (Set : in Set_Type);

private

   type Element_Access is access Element_Type;

   use Ada.Finalization;

   type Control_Type is
     new Controlled with record
        X : Element_Access;
     end record;


   use Ada.Streams;

   procedure Write
     (Stream  : access Root_Stream_Type'Class;
      Control : in     Control_Type);

   for Control_Type'Write use Write;

   procedure Read
     (Stream  : access Root_Stream_Type'Class;
      Control :    out Control_Type);

   for Control_Type'Read use Read;


   procedure Adjust (Control : in out Control_Type);

   procedure Finalize (Control : in out Control_Type);

   function Is_Less (L, R : Control_Type) return Boolean;

   function Is_Equal (L, R : Control_Type) return Boolean;

   package Rep_Types is
      new AI302.Containers.Sorted_Sets
       (Control_Type,
        "<" => Is_Less,
        "=" => Is_Equal);

   type Rep_Type is new Rep_Types.Set_Type;

   type Set_Type is new Rep_Type;


   type Rep_Cursor_Type is new Rep_Types.Cursor_Type;

   type Cursor_Type is new Rep_Cursor_Type;

--see note above
--     Null_Cursor : constant Cursor_Type :=
--       Cursor_Type (Rep_Types.Null_Cursor);

end Indefinite_Sets;
