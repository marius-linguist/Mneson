with Ada.Unchecked_Deallocation;

package body Indefinite_Maps is

   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);


   procedure Write
     (Stream  : access Root_Stream_Type'Class;
      Control : in     Control_Type) is
   begin

      if Control.X = null then
         Boolean'Write (Stream, False);
      else
         Boolean'Write (Stream, True);
         Element_Type'Output (Stream, Control.X.all);
      end if;

   end Write;


   procedure Read
     (Stream  : access Root_Stream_Type'Class;
      Control :    out Control_Type) is

      pragma Assert (Control.X = null); --?

      B : Boolean;

   begin

      Boolean'Read (Stream, B);

      if B then
         Control.X := new Element_Type'(Element_Type'Input (Stream));
      end if;

   end Read;



   procedure Adjust (Control : in out Control_Type) is
      X : Element_Access := Control.X;
   begin
      if X /= null then
         Control.X := new Element_Type'(X.all);
      end if;
   exception
      when others =>
         Control.X := null;
         raise;
   end;


   procedure Finalize (Control : in out Control_Type) is
   begin
      Free (Control.X);
   exception
      when others =>
         Control.X := null;
         raise;
   end;


   function Is_Equal_Element (L, R : Control_Type)
     return Boolean is
   begin
      if L.X = null then
         return R.X = null;
      end if;

      if R.X = null then
         return False;
      end if;

      return L.X.all = R.X.all;
   end;



   function Null_Cursor return Cursor_Type is
   begin
      return Cursor_Type (Rep_Types.Null_Cursor);
   end;


   function "=" (Left, Right : Map_Type) return Boolean is
   begin
      return MT (Left) = MT (Right);
   end;


   function Length (Map : Map_Type) return Natural is
   begin
      return Natural (Length (MT (Map)));
   end;


   function Is_Empty (Map : Map_Type) return Boolean is
   begin
      return Is_Empty (MT (Map));
   end;


   procedure Clear (Map : in out Map_Type) is
   begin
      Clear (MT (Map));
   end;


   procedure Swap (Left, Right : in out Map_Type) is
   begin
      Swap (MT (Left), MT (Right));
   end;


   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin

      --NOTE:
      --The odd thing here is that the element part is null.
      --END NOTE.

      Insert (MT (Map), Key, Rep_Types.Cursor_Type (Cursor), Success);

   end Insert;


   type Control_Access is access all Control_Type;
   for Control_Access'Storage_Size use 0;

   function To_Access is
      new Rep_Types.Generic_Element (Control_Access);

   function To_Access (Cursor : Cursor_Type) return Control_Access is
      pragma Inline (To_Access);
   begin
      return To_Access (Rep_Types.Cursor_Type (Cursor));
   end;


   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is
   begin

      Insert (Map, Key, Cursor, Success);

      if Success then

         declare
            C : Control_Type renames To_Access (Cursor).all;
            pragma Assert (C.X = null);
         begin
            C.X := new Element_Type'(New_Item);
         exception
            when others =>
               Delete (Map, Cursor);
               raise;
         end;

      end if;

   end Insert;


   procedure Replace (Map      : in out Map_Type;
                      Key      : in     Key_Type;
                      New_Item : in     Element_Type) is

      Cursor  : Cursor_Type;
      Success : Boolean;

   begin

      Insert (Map, Key, Cursor, Success);

      if Success then

         declare
            C : Control_Type renames To_Access (Cursor).all;
            pragma Assert (C.X = null);
         begin
            C.X := new Element_Type'(New_Item);
         exception
            when others =>
               Delete (Map, Cursor);
               raise;
         end;

      else

         declare
            C : Control_Type renames To_Access (Cursor).all;
            X : Element_Access := C.X;
         begin
            C.X := new Element_Type'(New_Item);
            Free (X);
         end;

      end if;

   end Replace;



   procedure Delete (Map : in out Map_Type;
                     Key : in     Key_Type) is
   begin
      Delete (MT (Map), Key);
   end;



   procedure Delete (Map    : in out Map_Type;
                     Cursor : in out Cursor_Type) is
   begin
      Delete (MT (Map), Rep_Types.Cursor_Type (Cursor));
   end;


   function Find (Map : Map_Type;
                  Key : Key_Type)
      return Cursor_Type is
   begin
      return Cursor_Type (Find (MT (Map), Key));
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

      Cursor : constant Cursor_Type := Find (Map, Key);

      C : Control_Type renames To_Access (Cursor).all;
   begin
      return C.X.all;
   end;



   function Size (Map : Map_Type) return Natural is
   begin
      return Natural (Size (MT (Map)));
   end;


   procedure Resize (Map  : in out Map_Type;
                     Size : in     Natural) is
   begin
      Resize (MT (Map), AI302.Containers.Size_Type (Size));
   end;


   function First (Map : Map_Type) return Cursor_Type is
      C : constant Rep_Types.Cursor_Type := First (MT (Map));
   begin
      return Cursor_Type (C);
   end;


   function Back (Map : Map_Type) return Cursor_Type is
      C : constant Rep_Types.Cursor_Type := Back (MT (Map));
   begin
      return Cursor_Type (C);
   end;


   function Succ
     (Map    : Map_Type;
      Cursor : Cursor_Type) return Cursor_Type is

      C : constant Rep_Types.Cursor_Type :=
        Succ (MT (Map), Rep_Types.Cursor_Type (Cursor));
   begin
      return Cursor_Type (C);
   end;


   procedure Increment
     (Map    : in     Map_Type;
      Cursor : in out Cursor_Type) is
   begin
      Cursor := Succ (Map, Cursor);
   end;


   function Key (Cursor : Cursor_Type) return Key_Type is
   begin
      return Key (CT (Cursor));
   end;


   function Generic_Key (Cursor : Cursor_Type)
      return Key_Access is

      function To_Access is
         new Rep_Types.Generic_Key (Key_Access);
   begin
      return To_Access (Rep_Types.Cursor_Type (Cursor));
   end;


   function Is_Equal_Key (Left, Right : Cursor_Type)
     return Boolean is
   begin
      return Is_Equal_Key (CT (Left), CT (Right));
   end;


   function Is_Equal_Key (Left  : Cursor_Type;
                          Right : Key_Type)
     return Boolean is
   begin
      return Is_Equal_Key (CT (Left), Right);
   end;


   function Is_Equal_Key (Left  : Key_Type;
                          Right : Cursor_Type)
     return Boolean is
   begin
      return Is_Equal_Key (Left, CT (Right));
   end;


   function Element (Cursor : Cursor_Type)
     return Element_Type is

      C : Control_Type renames To_Access (Cursor).all;
   begin
      return C.X.all;
   end;


   function Generic_Element (Cursor : Cursor_Type)
      return Element_Access is

      C : Control_Type renames To_Access (Cursor).all;
   begin
      if C.X = null then
         return null;
      end if;

      return C.X.all'Access;
   end;


   procedure Replace_Element (Cursor : in Cursor_Type;
                              By     : in Element_Type) is

      C : Control_Type renames To_Access (Cursor).all;

      X : Element_Access := C.X;
   begin
      C.X := new Element_Type'(By);
      Free (X);
   end;


   procedure Generic_Iteration (Map : in Map_Type) is

      procedure Process (Cursor : Rep_Types.Cursor_Type) is
         pragma Inline (Process);
      begin
         Process (Cursor_Type (Cursor));
      end;

      procedure Iterate is
         new Rep_Types.Generic_Iteration (Process);
   begin
      Iterate (Rep_Types.Map_Type (Map));
   end;


end Indefinite_Maps;
