with AI302.Containers.Sorted_Sets;
pragma Elaborate_All (AI302.Containers.Sorted_Sets);

generic

   type Key_Type is private;

   type Element_Type is private;

   with function "<" (Left, Right : Key_Type)
     return Boolean is <>;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package Sorted_Maps is

   pragma Preelaborate;

   subtype Key_Subtype is Key_Type;
   subtype Element_Subtype is Element_Type;

   type Map_Type is private;

   type Cursor_Type is private;

   function Null_Cursor return Cursor_Type;  --subtle

   function "=" (Left, Right : Map_Type) return Boolean;

   function Length (Map : Map_Type) return Natural;

   function Is_Empty (Map : Map_Type) return Boolean;

   procedure Clear (Map : in out Map_Type);

   procedure Swap (Left, Right : in out Map_Type);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Insert (Map      : in out Map_Type;
                     Position : in     Cursor_Type;
                     Key      : in     Key_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Replace (Map      : in out Map_Type;
                      Key      : in     Key_Type;
                      New_Item : in     Element_Type);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Insert (Map      : in out Map_Type;
                     Position : in     Cursor_Type;
                     Key      : in     Key_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Delete (Map : in out Map_Type;
                     Key : in     Key_Type);

   procedure Delete (Map    : in out Map_Type;
                     Cursor : in out Cursor_Type);

   procedure Delete_Sans_Increment (Map    : in out Map_Type;
                                    Cursor : in out Cursor_Type);

   procedure Delete_First (Map : in out Map_Type);

   procedure Delete_Last (Map : in out Map_Type);

   function Is_In (Key : Key_Type;
                   Map : Map_Type)
      return Boolean;

   function Find (Map : Map_Type;
                  Key : Key_Type)
      return Cursor_Type;

   function Lower_Bound (Map : Map_Type;
                         Key : Key_Type) return Cursor_Type;

   function Upper_Bound (Map : Map_Type;
                         Key : Key_Type) return Cursor_Type;

   function Element (Map : Map_Type;
                     Key : Key_Type)
     return Element_Type;

   function First (Map : Map_Type) return Cursor_Type;

   function First_Key (Map : Map_Type) return Key_Type;

   function First_Element (Map : Map_Type) return Element_Type;

   function Last (Map : Map_Type) return Cursor_Type;

   function Last_Key (Map : Map_Type) return Key_Type;

   function Last_Element (Map : Map_Type) return Element_Type;

   function Back (Map : Map_Type) return Cursor_Type;

   function Succ (Cursor : Cursor_Type) return Cursor_Type;

   function Pred (Cursor : Cursor_Type) return Cursor_Type;

   procedure Increment (Cursor : in out Cursor_Type);

   procedure Decrement (Cursor : in out Cursor_Type);

   function Key (Cursor : Cursor_Type) return Key_Type;

   generic
      type Key_Access is access constant Key_Type;
   function Generic_Key (Cursor : Cursor_Type)
      return Key_Access;

   function "<" (Left, Right : Cursor_Type) return Boolean;

   function "<" (Left : Cursor_Type; Right : Key_Type)
      return Boolean;

   function "<" (Left : Key_Type; Right : Cursor_Type)
      return Boolean;

   function Element (Cursor : Cursor_Type)
     return Element_Type;

   generic
      type Element_Access is access all Element_Type;
   function Generic_Element (Cursor : Cursor_Type)
      return Element_Access;

   procedure Replace_Element (Cursor : in Cursor_Type;
                              By     : in Element_Type);

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Iteration (Map : in Map_Type);

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Reverse_Iteration (Map : in Map_Type);

private

   type Pair_Type is record
      Key     : aliased Key_Type;
      Element : aliased Element_Type;
   end record;

   function "=" (L, R : Pair_Type) return Boolean is abstract;

   function Is_Less (L, R : Pair_Type) return Boolean;

   function Is_Equal (L, R : Pair_Type) return Boolean;

   package Rep_Types is
      new AI302.Containers.Sorted_Sets (Pair_Type, Is_Less, Is_Equal);

   type ST is new Rep_Types.Set_Type;

   type Map_Type is new ST;

   type Cursor_Type is new Rep_Types.Cursor_Type;

end Sorted_Maps;



