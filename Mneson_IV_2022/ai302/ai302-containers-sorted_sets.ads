with AI302.Containers.Red_Black_Trees;
pragma Elaborate_All (AI302.Containers.Red_Black_Trees);

with Ada.Finalization;
with Ada.Streams;

generic

   type Element_Type is private;

   with function "<" (Left, Right : Element_Type)
      return Boolean is <>;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package AI302.Containers.Sorted_Sets is
   pragma Preelaborate (Sorted_Sets);

   type Set_Type is private;

   type Cursor_Type is private;

   Null_Cursor : constant Cursor_Type;

   function "=" (Left, Right : Set_Type) return Boolean;

   function "<" (Left, Right : Set_Type) return Boolean;

   function "<=" (Left, Right : Set_Type) return Boolean;

   function ">" (Left, Right : Set_Type) return Boolean;

   function ">=" (Left, Right : Set_Type) return Boolean;

   function Length (Set : Set_Type) return Size_Type;

   function Is_Empty (Set : Set_Type) return Boolean;

   procedure Clear (Set : in out Set_Type);

   procedure Swap (Left, Right : in out Set_Type);

   procedure Insert (Set      : in out Set_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   --NOTE:
   --A nice function might be:
   --procedure Insert (Set      : in out Set_Type;
   --                  New_Item : in     Element_Type);
   --This is a convenience function that omits the last two params.
   --END NOTE.


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

   --NOTE: I added this
   function ">" (Left : Cursor_Type; Right : Element_Type)
      return Boolean;

   function "<" (Left : Element_Type; Right : Cursor_Type)
      return Boolean;

   --NOTE: I added this
   function ">" (Left : Element_Type; Right : Cursor_Type)
      return Boolean;

   function Element (Cursor : Cursor_Type) return Element_Type;

   generic
      type Element_Access is access all Element_Type;
   function Generic_Element
     (Cursor : Cursor_Type) return Element_Access;

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Iteration (Set : in Set_Type);

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Reverse_Iteration (Set : in Set_Type);


   generic

      type Key_Type (<>) is limited private;

      with function "<" (Left : Key_Type; Right : Element_Type)
          return Boolean is <>;

      with function ">" (Left : Key_Type; Right : Element_Type)
          return Boolean is <>;

   package Generic_Keys is

      subtype Key_Subtype is Key_Type;


      function Is_In (Key : Key_Type;
                      Set : Set_Type)
         return Boolean;

      function Find (Set : Set_Type;
                     Key : Key_Type)
        return Cursor_Type;

      function Element (Set : Set_Type;
                        Key : Key_Type)
        return Element_Type;

      function Lower_Bound (Set : Set_Type;
                            Key : Key_Type)
        return Cursor_Type;

      function Upper_Bound (Set : Set_Type;
                            Key : Key_Type)
        return Cursor_Type;

      procedure Delete (Set : in out Set_Type;
                        Key : in     Key_Type);

      function "<" (Left : Cursor_Type; Right : Key_Type)
        return Boolean;

      --NOTE: I added this
      function ">" (Left : Cursor_Type; Right : Key_Type)
        return Boolean;

      function "<" (Left : Key_Type; Right : Cursor_Type)
        return Boolean;

      --NOTE: I added this
      function ">" (Left : Key_Type; Right : Cursor_Type)
        return Boolean;

      generic

         --NOTE:
         --This is here, in a nested generic, because I didn't want to
         --punish instantiators of Generic_Key who didn't happen to
         --want to do any insertion by key.
         --
         --I think the ARG is toying with the idea of having generic
         --formal subprograms that can default to a "do nothing."
         --Which means we could get rid of this nested generic
         --and just move Set_Key up into the formal region of
         --Generic_Keys, and with a noop-style default.
         --END NOTE.

         with procedure Set_Key
           (Element : in out Element_Type;
            Key     : in     Key_Type) is <>;

      package Generic_Insertion is

         procedure Insert (Set     : in out Set_Type;
                           Key     : in     Key_Type;
                           Cursor  :    out Cursor_Type;
                           Success :    out Boolean);

         procedure Insert (Set      : in out Set_Type;
                           Position : in     Cursor_Type;
                           Key      : in     Key_Type;
                           Cursor   :    out Cursor_Type;
                           Success  :    out Boolean);

      end Generic_Insertion;

   end Generic_Keys;

private

   type Color_Type is (Red, Black, White);

   type Node_Type;
   type Node_Access is access Node_Type;

   function Parent (Node : Node_Access)
      return Node_Access;
   pragma Inline (Parent);

   function Left (Node : Node_Access)
      return Node_Access;
   pragma Inline (Left);

   function Right (Node : Node_Access)
      return Node_Access;
   pragma Inline (Right);

   function Color (Node : Node_Access)
      return Color_Type;
   pragma Inline (Color);

   procedure Set_Parent
     (Node   : Node_Access;
      Parent : Node_Access);
   pragma Inline (Set_Parent);

   procedure Set_Left
     (Node : Node_Access;
      Left : Node_Access);
   pragma Inline (Set_Left);

   procedure Set_Right
     (Node  : Node_Access;
      Right : Node_Access);
   pragma Inline (Set_Right);

   procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type);
   pragma Inline (Set_Color);


   package Tree_Types is
      new Red_Black_Trees
        (Node_Access => Node_Access,
         Color_Type  => Color_Type,
         Null_Node   => null,
         Red         => Red,
         Black       => Black,
         White       => White);

   use Tree_Types;


   function New_Back return Node_Access;

   type Set_Type is new Ada.Finalization.Controlled with record
      Tree : Tree_Type := (Back => New_Back, Length => 0);
   end record;


   --procedure Initialize (Set : in out Set_Type);

   procedure Adjust (Set : in out Set_Type);

   procedure Finalize (Set : in out Set_Type);


   type Cursor_Type is
      record
         Node : Node_Access;
      end record;

   Null_Cursor : constant Cursor_Type := (Node => null);


   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Set    : in     Set_Type);

   for Set_Type'Write use Write;


   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Set    :    out Set_Type);

   for Set_Type'Read use Read;


end AI302.Containers.Sorted_Sets;

