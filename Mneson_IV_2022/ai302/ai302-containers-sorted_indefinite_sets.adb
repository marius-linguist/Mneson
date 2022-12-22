with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body AI302.Containers.Sorted_Indefinite_Sets is

   type Element_Access is access Element_Type;

   type Node_Type is
      record   --make this a limited record for Ada0Y
         Parent  : Node_Access;
         Left    : Node_Access;
         Right   : Node_Access;
         Color   : Color_Type;
         Element : Element_Access;
      end record;

   function "=" (L, R : Node_Type) return Boolean is abstract;
   pragma Warnings (Off, "=");


   function Parent (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Parent;
   end;

   function Left (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Left;
   end;

   function Right (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Right;
   end;

   function Color (Node : Node_Access)
      return Color_Type is
   begin
      return Node.Color;
   end;

   procedure Set_Parent
     (Node   : Node_Access;
      Parent : Node_Access) is
   begin
      Node.Parent := Parent;
   end;

   procedure Set_Left
     (Node : Node_Access;
      Left : Node_Access) is
   begin
      Node.Left := Left;
   end;

   procedure Set_Right
     (Node  : Node_Access;
      Right : Node_Access) is
   begin
      Node.Right := Right;
   end;

   procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type) is
   begin
      Node.Color := Color;
   end;


   function New_Back return Node_Access is

      Back : constant Node_Access := new Node_Type;
   begin
      Back.Color := White;

      Back.Left := Back;
      Back.Right := Back;

      return Back;
   end;


   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Free (X : in out Node_Access) is

      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   begin
      Free (X.Element);
      Deallocate (X);
   end;


   procedure Delete_Tree (X : in out Node_Access) is

      Y : Node_Access;

   begin

      while X /= null loop

         Y := X.Right;

         Delete_Tree (Y);

         Y := X.Left;

         Free (X);

         X := Y;

      end loop;

   end Delete_Tree;


   function Copy_Node (Source : Node_Access) return Node_Access is
      pragma Inline (Copy_Node);

      --NOTE:
      --We could make the decision to allow null elements to be
      --copied.  Here we assume that the source is non-null.
      --END NOTE.

      Element : Element_Access := new Element_Type'(Source.Element.all);
   begin
      return new Node_Type'(Parent  => null,
                            Left    => null,
                            Right   => null,
                            Color   => Source.Color,
                            Element => Element);
   exception
      when others =>
         Free (Element);
         raise;
   end;


   function Copy_Tree (Source_Root : Node_Access) return Node_Access is

      Target_Root : Node_Access := Copy_Node (Source_Root);

      P, X : Node_Access;

   begin

      if Source_Root.Right /= null then

         Target_Root.Right := Copy_Tree (Source_Root.Right);
         Target_Root.Right.Parent := Target_Root; --per Georg Bauhaus

      end if;

      P := Target_Root;
      X := Source_Root.Left;

      while X /= null loop

         declare
            Y : Node_Access := Copy_Node (X);
         begin

            P.Left := Y;
            Y.Parent := P;

            if X.Right /= null then

               Y.Right := Copy_Tree (X.Right);
               Y.Right.Parent := Y; --per Georg Bauhaus

            end if;

            P := Y;
            X := X.Left;

         end;

      end loop;

      return Target_Root;

   exception
      when others =>

         Delete_Tree (Target_Root);
         raise;

   end Copy_Tree;


   procedure Adjust (Set : in out Set_Type) is

      Tree : Tree_Type renames Set.Tree;

      Length : constant Size_Type := Tree.Length;

      X : Node_Access := Root (Tree);
      Y : Node_Access;

   begin -- Adjust

      Tree.Back := null;
      Tree.Length := 0;

      Tree.Back := new Node_Type;
      -- Allocation failure here is the problem case,
      -- since it means we aren't satisfying our
      -- representation invariant.

      Tree.Back.Color := White;

      Initialize (Tree);

      if X = null then
         return;
      end if;

      Y := Copy_Tree (X);
      -- If allocation fails here, it's less of a problem
      -- since we already have a Back node, and hence our
      -- representation invariant is satisfied.

      Y.Parent := Tree.Back;  -- per Georg Bauhaus

      Set_Root (Tree, Y);
      Set_First (Tree, Min (Y));
      Set_Last (Tree, Max (Y));

      Tree.Length := Length;

   end Adjust;


   procedure Finalize (Set : in out Set_Type) is

      Tree : Tree_Type renames Set.Tree;

      Back : Node_Access := Tree.Back;
      Root : Node_Access;

   begin

      if Back = null then
         return;
      end if;

      Root := Tree_Types.Root (Tree);

      Tree.Back := null;
      Tree.Length := 0;

      Delete_Tree (Root);
      Free (Back);

   end Finalize;



   function Is_Equal_Node_Node
     (L, R : Node_Access) return Boolean is

      pragma Inline (Is_Equal_Node_Node);
   begin
      --NOTE:
      --We could liberalize equality, to allow null-elements
      --to be compared.  Here we simply assume elements
      --are non-null.
      --END NOTE.

      return L.Element.all = R.Element.all;
   end;

   function Is_Equal is
      new Tree_Types.Generic_Equal (Is_Equal_Node_Node);

   function "=" (Left, Right : Set_Type) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      return Is_Equal (Left.Tree, Right.Tree);
   end;


   function Is_Less_Node_Node
     (Left, Right : Node_Access) return Boolean is

      pragma Inline (Is_Less_Node_Node);
   begin
      return Left.Element.all < Right.Element.all;
   end;


   function Is_Greater_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean is

      pragma Inline (Is_Greater_Element_Node);

      -- e > node same as node < e
   begin
      return Right.Element.all < Left;
   end;


   function Is_Less_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean is

      pragma Inline (Is_Less_Element_Node);
   begin
      return Left < Right.Element.all;
   end;



   function "<" is
      new Tree_Types.Generic_Less (Is_Less_Node_Node);

   function "<" (Left, Right : Set_Type) return Boolean is
   begin
      if Left'Address = Right'Address then
         return False;
      end if;

      return Left.Tree < Right.Tree;
   end;


   function "<=" (Left, Right : Set_Type) return Boolean is
   begin
      return not (Left > Right);
   end;


   function ">" (Left, Right : Set_Type) return Boolean is
   begin
      return Right < Left;
   end;


   function ">=" (Left, Right : Set_Type) return Boolean is
   begin
      return not (Left < Right);
   end;


   function Length (Set : Set_Type) return Size_Type is
   begin
      return Set.Tree.Length;
   end;


   function Is_Empty (Set : Set_Type) return Boolean is
   begin
      return Length (Set) = 0;
   end;


   procedure Clear (Set : in out Set_Type) is

      X : Node_Access := Root (Set.Tree);

   begin

      Initialize (Set.Tree);

      Delete_Tree (X);

   end Clear;


   procedure Swap (Left, Right : in out Set_Type) is
   begin
      Swap (Left.Tree, Right.Tree);
   end;


   package Element_Keys is
      new Tree_Types.Generic_Keys
        (Element_Type,
         Is_Less_Element_Node,
         Is_Greater_Element_Node);


   procedure Insert
     (Set      : in out Set_Type;
      New_Item : in     Element_Type;
      Cursor   :    out Cursor_Type;
      Success  :    out Boolean) is

      function New_Node return Node_Access is
         pragma Inline (New_Node);

         Element : Element_Access := new Element_Type'(New_Item);
      begin
         return new Node_Type'(Parent  => null,
                               Left    => null,
                               Right   => null,
                               Color   => Red,
                               Element => Element);
      exception
         when others =>
            Free (Element);
            raise;
      end;

      procedure Insert_Post is
         new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
         new Element_Keys.Generic_Conditional_Insert (Insert_Post);

   begin -- Insert

      Insert_Sans_Hint
        (Set.Tree,
         New_Item,
         Cursor.Node,
         Success);

      --pragma Debug (Check_Invariant (Set.Tree));

   end;


   procedure Insert
     (Set      : in out Set_Type;
      Position : in     Cursor_Type;
      New_Item : in     Element_Type;
      Cursor   :    out Cursor_Type;
      Success  :    out Boolean) is

      function New_Node return Node_Access is
         pragma Inline (New_Node);

         Element : Element_Access := new Element_Type'(New_Item);
      begin
         return new Node_Type'(Parent  => null,
                               Left    => null,
                               Right   => null,
                               Color   => Red,
                               Element => Element);
      exception
         when others =>
            Free (Element);
            raise;
      end;

      procedure Insert_Post is
         new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
         new Element_Keys.Generic_Conditional_Insert (Insert_Post);

      procedure Insert_With_Hint is
         new Element_Keys.Generic_Conditional_Insert_With_Hint
            (Insert_Post,
             Insert_Sans_Hint);

   begin -- Insert

      Insert_With_Hint
        (Set.Tree,
         Position.Node,
         New_Item,
         Cursor.Node,
         Success);

      --pragma Debug (Check_Invariant (Set.Tree));

   end Insert;


   procedure Delete (Set    : in out Set_Type;
                     Cursor : in out Cursor_Type) is
   begin

      if Cursor.Node = null
        or else Cursor.Node = Set.Tree.Back
      then
         return;
      end if;

      pragma Assert (Cursor.Node.Color /= White);

      declare
         Next : constant Node_Access := Succ (Cursor.Node);
      begin
         Delete (Set.Tree, Cursor.Node);

         declare
            X : Node_Access := Cursor.Node;
         begin
            Cursor.Node := Next;
            Free (X);
         end;
      end;

      --pragma Debug (Check_Invariant (Set.Tree));

   end Delete;


   procedure Delete_Sans_Increment (Set    : in out Set_Type;
                                    Cursor : in out Cursor_Type) is
   begin

      if Cursor.Node = null
        or else Cursor.Node = Set.Tree.Back
      then
         return;
      end if;

      pragma Assert (Cursor.Node.Color /= White);

      Delete (Set.Tree, Cursor.Node);

      declare
         X : Node_Access := Cursor.Node;
      begin
         Cursor.Node := null; --Set.Tree.Back;
         Free (X);
      end;

      --pragma Debug (Check_Invariant (Set.Tree));

   end Delete_Sans_Increment;


   function Find (Set  : Set_Type;
                  Item : Element_Type) return Cursor_Type is
   begin
      return (Node => Element_Keys.Find (Set.Tree, Item));
   end;


   function Is_In (Item : Element_Type;
                   Set  : Set_Type) return Boolean is
   begin
      return Find (Set, Item) /= Back (Set);
   end;


   function First (Set : Set_Type) return Cursor_Type is
   begin
      return (Node => First (Set.Tree));
   end;


   function First_Element (Set : Set_Type) return Element_Type is
      C : constant Cursor_Type := First (Set);
   begin
      return C.Node.Element.all;
   end;


   function Last (Set : Set_Type) return Cursor_Type is
   begin
      return (Node => Last (Set.Tree));
   end;


   function Last_Element (Set : Set_Type) return Element_Type is
      C : constant Cursor_Type := Last (Set);
   begin
      return C.Node.Element.all;
   end;


   function Back (Set : Set_Type) return Cursor_Type is
   begin
      return (Node => Set.Tree.Back);
   end;


   function Succ (Cursor : Cursor_Type) return Cursor_Type is
   begin
      return (Node => Succ (Cursor.Node));
   end;


   function Pred (Cursor : Cursor_Type) return Cursor_Type is
   begin
      return (Node => Pred (Cursor.Node));
   end;


   procedure Increment (Cursor : in out Cursor_Type) is
   begin
      Cursor := Succ (Cursor);
   end;


   procedure Decrement (Cursor : in out Cursor_Type) is
   begin
      Cursor := Pred (Cursor);
   end;


   function Element (Cursor : Cursor_Type) return Element_Type is
   begin
      return Cursor.Node.Element.all;  --works for Back node, too
   end;


   procedure Delete (Set  : in out Set_Type;
                     Item : in     Element_Type) is

      C : Cursor_Type := Find (Set, Item);
   begin
      Delete_Sans_Increment (Set, C);
   end;


   procedure Delete_First (Set : in out Set_Type) is
      C : Cursor_Type := First (Set);
   begin
      Delete_Sans_Increment (Set, C);
   end;


   procedure Delete_Last (Set : in out Set_Type) is
      C : Cursor_Type := Last (Set);
   begin
      Delete_Sans_Increment (Set, C);
   end;


   function Lower_Bound (Set  : Set_Type;
                         Item : Element_Type) return Cursor_Type is
   begin
      return (Node => Element_Keys.Lower_Bound (Set.Tree, Item));
   end;


   function Upper_Bound (Set  : Set_Type;
                         Item : Element_Type) return Cursor_Type is
   begin
      return (Node => Element_Keys.Upper_Bound (Set.Tree, Item));
   end;


   function "<" (Left, Right : Cursor_Type) return Boolean is
   begin
      return Left.Node.Element.all < Right.Node.Element.all;
   end;


   function "<" (Left : Cursor_Type; Right : Element_Type)
      return Boolean is
   begin
      return Left.Node.Element.all < Right;
   end;

   function ">" (Left : Cursor_Type; Right : Element_Type)
      return Boolean is
   begin
      return Right < Left.Node.Element.all;
   end;

   function "<" (Left : Element_Type; Right : Cursor_Type)
      return Boolean is
   begin
      return Left < Right.Node.Element.all;
   end;

   function ">" (Left : Element_Type; Right : Cursor_Type)
      return Boolean is
   begin
      return Right.Node.Element.all < Left;
   end;


   function Generic_Element
     (Cursor : Cursor_Type) return Element_Access is
   begin

      --NOTE: keep this?
      --
--        if Cursor.Node.Color = White then
--           raise Constraint_Error;
--        end if;

      return Element_Access (Cursor.Node.Element);
   end;


   procedure Generic_Iteration (Set : in Set_Type) is

      procedure Process (Node : Node_Access) is
         pragma Inline (Process);
      begin
         Process (Cursor_Type'(Node => Node));
      end;

      procedure Iterate is
         new Tree_Types.Generic_Iteration (Process);
   begin
      Iterate (Set.Tree);
   end;


   procedure Generic_Reverse_Iteration (Set : in Set_Type) is

      procedure Process (Node : Node_Access) is
         pragma Inline (Process);
      begin
         Process (Cursor_Type'(Node => Node));
      end;

      procedure Iterate is
         new Tree_Types.Generic_Reverse_Iteration (Process);
   begin
      Iterate (Set.Tree);
   end;


   package body Generic_Keys is

      function Is_Less_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean is

         pragma Inline (Is_Less_Key_Node);
      begin
         return Left < Right.Element.all;
      end;

      function Is_Greater_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean is

         pragma Inline (Is_Greater_Key_Node);
      begin
         return Left > Right.Element.all;
      end;


      package Key_Keys is
         new Tree_Types.Generic_Keys
          (Key_Type,
           Is_Less_Key_Node,
           Is_Greater_Key_Node);


      function Find (Set : Set_Type;
                     Key : Key_Type)
        return Cursor_Type is
      begin
         return (Node => Key_Keys.Find (Set.Tree, Key));
      end;


      function Is_In (Key : Key_Type;
                      Set : Set_Type)
         return Boolean is
      begin
         return Find (Set, Key) /= Back (Set);
      end;


      function Element (Set : Set_Type;
                        Key : Key_Type)
        return Element_Type is

         C : constant Cursor_Type := Find (Set, Key);
      begin
         return C.Node.Element.all;
      end;


      function Lower_Bound (Set : Set_Type;
                            Key : Key_Type)
        return Cursor_Type is
      begin
         return (Node => Key_Keys.Lower_Bound (Set.Tree, Key));
      end;


      function Upper_Bound (Set : Set_Type;
                            Key : Key_Type)
        return Cursor_Type is
      begin
         return (Node => Key_Keys.Upper_Bound (Set.Tree, Key));
      end;


      procedure Delete (Set : in out Set_Type;
                        Key : in     Key_Type) is

         C : Cursor_Type := Find (Set, Key);
      begin
         Delete_Sans_Increment (Set, C);
      end;


      function "<" (Left : Key_Type; Right : Cursor_Type)
         return Boolean is
      begin
         return Left < Right.Node.Element.all;
      end;

      function ">" (Left : Key_Type; Right : Cursor_Type)
         return Boolean is
      begin
         return Left > Right.Node.Element.all;
      end;

      function "<" (Left : Cursor_Type; Right : Key_Type)
        return Boolean is
      begin
         return Right > Left.Node.Element.all;
      end;

      function ">" (Left : Cursor_Type; Right : Key_Type)
        return Boolean is
      begin
         return Right < Left.Node.Element.all;
      end;


      package body Generic_Insertion is

         procedure Insert
           (Set     : in out Set_Type;
            Key     : in     Key_Type;
            Cursor  :    out Cursor_Type;
            Success :    out Boolean) is

            function New_Node return Node_Access is
               pragma Inline (New_Node);

               Node : Node_Access := new Node_Type;
            begin
               --Set_Key (Node.Element, Key);
               Node.Element := new Element_Type'(New_Element (Key));

               Node.Color := Red;

               return Node;
            exception
               when others =>
                  Free (Node);
                  raise;
            end;

            procedure Insert_Post is
               new Key_Keys.Generic_Insert_Post (New_Node);

            procedure Insert_Sans_Hint is
               new Key_Keys.Generic_Conditional_Insert (Insert_Post);

         begin -- Insert

            Insert_Sans_Hint
              (Set.Tree,
               Key,
               Cursor.Node,
               Success);

         end Insert;


         procedure Insert
           (Set      : in out Set_Type;
            Position : in     Cursor_Type;
            Key      : in     Key_Type;
            Cursor   :    out Cursor_Type;
            Success  :    out Boolean) is

            function New_Node return Node_Access is
               pragma Inline (New_Node);

               Node : Node_Access := new Node_Type;
            begin
               --Set_Key (Node.Element, Key);
               Node.Element := new Element_Type'(New_Element (Key));

               Node.Color := Red;

               return Node;
            exception
               when others =>
                  Free (Node);
                  raise;
            end;

            procedure Insert_Post is
               new Key_Keys.Generic_Insert_Post (New_Node);

            procedure Insert_Sans_Hint is
               new Key_Keys.Generic_Conditional_Insert (Insert_Post);

            procedure Insert_With_Hint is
               new Key_Keys.Generic_Conditional_Insert_With_Hint
                  (Insert_Post,
                   Insert_Sans_Hint);

         begin -- Insert

            Insert_With_Hint
              (Set.Tree,
               Position.Node,
               Key,
               Cursor.Node,
               Success);

         end Insert;

      end Generic_Insertion;

   end Generic_Keys;


   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Set    : in     Set_Type) is

      procedure Process (Node : Node_Access) is
         pragma Inline (Process);
      begin
         Element_Type'Output (Stream, Node.Element.all);
      end;

      procedure Iterate is
         new Tree_Types.Generic_Iteration (Process);

   begin -- Write

      Size_Type'Base'Write (Stream, Set.Tree.Length);

      Iterate (Set.Tree);

   end Write;


   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Set    :    out Set_Type) is

      function New_Node return Node_Access is

         Node : Node_Access := new Node_Type;
      begin
         Node.Color := Red;
         Node.Element := new Element_Type'(Element_Type'Input (Stream));

         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;

      procedure Read is
         new Tree_Types.Generic_Read (New_Node);

      N : Size_Type'Base;

   begin

      Clear (Set);

      Size_Type'Base'Read (Stream, N);
      pragma Assert (N >= 0);

      Read (Set.Tree, N);

   end Read;


end AI302.Containers.Sorted_Indefinite_Sets;

