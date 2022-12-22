with AI302.Containers.Generic_Sort_Unconstrained_Array;
with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body AI302.Containers.Indefinite_Vectors is

   procedure Free is
      new Ada.Unchecked_Deallocation (Elements_Type, Elements_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);


   procedure Adjust (Vector : in out Vector_Type) is
   begin

      if Vector.Elements = null
        or else Vector.Elements'Length = 0
        or else Vector.Last < Index_Type'First
      then
         Vector.Elements := null;
         return;
      end if;

      declare
         E : Elements_Type renames Vector.Elements.all;
         L : constant Index_Type := Vector.Last;
      begin

         begin
            Vector.Elements := new Elements_Type (Index_Type'First .. L);
         exception
            when others =>
               Vector.Elements := null;
               raise;
         end;

         Vector.Last := Index_Type'Pred (Index_Type'First);

         for I in Vector.Elements'Range loop

            --NOTE:
            --In theory we don't need to bother check to see whether the
            --element is null, since non-null is an invariant.  However,
            --we do so here, since there's no real harm in allowing a
            --copying a null during assignment.
            --END NOTE.

            if E (I) /= null then
               Vector.Elements (I) := new Element_Type'(E (I).all);
            end if;

            Vector.Last := I;

         end loop;

      end;

   end Adjust;


   procedure Finalize (Vector : in out Vector_Type) is

      E : Elements_Access := Vector.Elements;
      L : constant Index_Type'Base := Vector.Last;

   begin

      Vector.Elements := null;
      Vector.Last := Index_Type'Pred (Index_Type'First);

      for I in Index_Type'First .. L loop
         Free (E (I));
      end loop;

      Free (E);

   end Finalize;


   procedure Write
     (Stream : access Root_Stream_Type'Class;
      Vector : in     Vector_Type) is

   begin

      Size_Type'Base'Write (Stream, Length (Vector));

      for I in Index_Type'First .. Vector.Last loop
         Element_Type'Output (Stream, Vector.Elements (I).all);
      end loop;

   end Write;


   procedure Read
     (Stream : access Root_Stream_Type'Class;
      Vector :    out Vector_Type) is

      Length : Size_Type'Base;
      Last   : Index_Type'Base := Index_Type'Pred (Index_Type'First);

   begin

      Clear (Vector);

      Size_Type'Base'Read (Stream, Length);

      Resize (Vector, Size => Length);

      for I in Size_Type range 1 .. Length loop

         Last := Index_Type'Succ (Last);

         Vector.Elements (Last) :=
           new Element_Type'(Element_Type'Input (Stream));

         Vector.Last := Last;

      end loop;

   end Read;


   function To_Vector
     (New_Item : Element_Type;
      Count    : Size_Type) return Vector_Type is

   begin

      if Count = 0 then
         return (Controlled with null, 0);
      end if;

      declare

         First : constant Size_Type'Base :=
           Size_Type'Base (Index_Type'First);

         Last_As_Size : constant Size_Type'Base :=
           First + Count - 1;

         Last : constant Index_Type :=
           Index_Type (Last_As_Size);

         Elements : Elements_Access :=
           new Elements_Type (Index_Type'First .. Last);

      begin

         for I in Elements'Range loop

            begin
               Elements (I) := new Element_Type'(New_Item);
            exception
               when others =>
                  for J in Index_Type'First .. Index_Type'Pred (I) loop
                     Free (Elements (J));
                  end loop;

                  Free (Elements);
                  raise;
            end;

         end loop;

         return (Controlled with Elements, Last);

      end;

   end To_Vector;


   function "=" (Left, Right : Vector_Type) return Boolean is
   begin

      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Last /= Right.Last then
         return False;
      end if;

      for I in Index_Type'First .. Left.Last loop

         --NOTE:
         --Again, we relax the precondition and allow vectors
         --with holes to be compared sans exception.
         --END NOTE.

         if Left.Elements (I) = null then

            if Right.Elements (I) /= null then
               return False;
            end if;

         elsif Right.Elements (I) = null then

            return False;

         elsif Left.Elements (I).all /= Right.Elements (I).all then

            return False;

         end if;

      end loop;

      return True;

   end "=";


   function Length (Vector : Vector_Type) return Size_Type is

      L : constant Size_Type'Base := Index_Type'Pos (Vector.Last);
      F : constant Size_Type'Base := Index_Type'Pos (Index_Type'First);

      N : constant Size_Type := L - F + 1;
   begin
      return N;
   end;


   function Is_Empty (Vector : Vector_Type) return Boolean is
   begin
      return Vector.Last < Index_Type'First;
   end;


   procedure Set_Length
     (Vector : in out Vector_Type;
      Length : in     Size_Type) is

      N : constant Size_Type := Size (Vector);

   begin

      if Length > N then
         Resize (Vector, Size => Length);
         return;
      end if;

      for I in Length + 1 .. N loop

         declare
            J : constant Index_Type := Vector.Last;
            X : Element_Access := Vector.Elements (J);
         begin
            Vector.Elements (J) := null;
            Vector.Last := Index_Type'Pred (J);
            Free (X);
         end;

      end loop;

   end Set_Length;


   procedure Clear (Vector : in out Vector_Type) is
   begin
      Set_Length (Vector, Length => 0);
   end;


   procedure Swap (Left, Right : in out Vector_Type) is

      LE : constant Elements_Access := Left.Elements;
      LL : constant Index_Type'Base := Left.Last;

   begin

      Left.Elements := Right.Elements;
      Left.Last := Right.Last;

      Right.Elements := LE;
      Right.Last := LL;

   end Swap;


   procedure Append (Vector   : in out Vector_Type;
                     New_Item : in     Element_Type) is
   begin
      Insert (Vector, Back (Vector), New_Item);
   end;


   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base;
                     New_Item : in     Element_Type) is
   begin
      Insert (Vector, Before, New_Item, Count => 1);
   end;


--     procedure Insert (Vector   : in out Vector_Type;
--                       Before   : in     Index_Type'Base) is
--     begin
--        Insert_N (Vector, Before, Element_Count'(1));
--     end;


   procedure Insert_Internal
     (Vector   : in out Vector_Type;
      Before   : in     Index_Type;
      New_Item : in     Element_Type;
      Count    : in     Size_Type) is

      Last_As_Size : constant Size_Type'Base :=
        Index_Type'Pos (Vector.Last);

      New_Last_As_Size : constant Size_Type'Base :=
        Last_As_Size + Count;

      New_Last : constant Index_Type :=
        Index_Type (New_Last_As_Size);

      Index_As_Size : constant Size_Type'Base :=
        Size_Type'Base (Before) + Count;

      Index : constant Index_Type :=
        Index_Type (Index_As_Size);

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin

      if Vector.Elements = null then

         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. New_Last);
         begin
            Vector.Elements := new Elements_Subtype;
            Vector.Last := Index_Type'Pred (Index_Type'First);

            for I in Vector.Elements'Range loop
               Vector.Elements (I) := new Element_Type'(New_Item);
               Vector.Last := I;
            end loop;
         end;

         return;

      end if;

      if New_Last <= Vector.Elements'Last then

         declare
            E : Elements_Type renames Vector.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Vector.Last);
            Vector.Last := New_Last;

            --NOTE:
            --Now we do the allocation.  If it fails, we can propagate the
            --exception and invariants are more or less satisfied.  The
            --issue is that we have some slots still null, and the client
            --has no way of detecting whether the slot is null (unless we
            --give him a way).
            --
            --Another way is to allocate a subarray on the stack, do the
            --allocation into that array, and if that success then do
            --the insertion proper.  The issue there is that you have to
            --allocate the subarray on the stack, and that may fail if the
            --subarray is long.
            --
            --Or we could try to roll-back the changes: deallocate the
            --elements we have successfully deallocated, and then copy
            --the elements ptrs back to their original posns.
            --END NOTE.

            --NOTE: I have written the loop manually here.  I could
            --have done it this way too:
            --   E (Before .. Index_Type'Pred (Index)) :=
            --      (others => new Element_Type'New_Item);
            --END NOTE.

            for I in Before .. Index_Type'Pred (Index) loop

               begin
                  E (I) := new Element_Type'(New_Item);
               exception
                  when others =>
                     E (I .. Index_Type'Pred (Index)) := (others => null);
                     raise;
               end;

            end loop;
         end;

         return;

      end if;

      declare

         type Int is range System.Min_Int .. System.Max_Int;

         First : constant Int'Base :=
           Index_Type'Pos (Index_Type'First);

         New_Size : constant Int'Base :=
           Int'Base (New_Last_As_Size) - First + 1;

         Max_Size : constant Int'Base :=
           Int'Base (Index_Type'Last) - First + 1;

         Size, Dst_Last_As_Int : Int'Base;

      begin

         if New_Size >= Max_Size / 2 then

            Dst_Last := Index_Type'Last;

         else

            Size := Vector.Elements'Length;

            if Size = 0 then
               Size := 1;
            end if;

            while Size < New_Size loop
               Size := 2 * Size;
            end loop;

            Dst_Last_As_Int := First + Size - 1;
            Dst_Last := Index_Type (Dst_Last_As_Int);

         end if;

      end;

      Dst := new Elements_Type (Index_Type'First .. Dst_Last);

      declare
         Src : Elements_Type renames Vector.Elements.all;
      begin
         Dst (Index_Type'First .. Index_Type'Pred (Before)) :=
           Src (Index_Type'First .. Index_Type'Pred (Before));

         Dst (Index .. New_Last) := Src (Before .. Vector.Last);
      end;

      declare
         X : Elements_Access := Vector.Elements;
      begin
         Vector.Elements := Dst;
         Vector.Last := New_Last;

         Free (X);
      end;

      --NOTE:
      --Now do the allocation.  If the allocation fails,
      --then the worst thing is that we have a few null slots.
      --Our invariants are otherwise satisfied.
      --END NOTE.

      for I in Before .. Index_Type'Pred (Index) loop
         Dst (I) := new Element_Type'(New_Item);
      end loop;

   end Insert_Internal;


   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base;
                     New_Item : in     Element_Type;
                     Count    : in     Size_Type) is
   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype T is Index_Type range Index_Type'First .. Back (Vector);
      begin
         Insert_Internal (Vector, T'(Before), New_Item, Count);
      end;

   end Insert;


   procedure Insert_N_Internal
     (Vector   : in out Vector_Type;
      Before   : in     Index_Type;
      Count    : in     Size_Type) is

      Last_As_Size : constant Size_Type'Base :=
        Index_Type'Pos (Vector.Last);

      New_Last_As_Size : constant Size_Type'Base :=
        Last_As_Size + Count;

      New_Last : constant Index_Type :=
        Index_Type (New_Last_As_Size);

      Index_As_Size : constant Size_Type'Base :=
        Size_Type'Base (Before) + Count;

      Index : constant Index_Type :=
        Index_Type (Index_As_Size);

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin

      if Vector.Elements = null then

         Vector.Elements := new Elements_Type (Index_Type'First .. New_Last);
         Vector.Last := New_Last;

         return;

      end if;

      if New_Last <= Vector.Elements'Last then

         declare
            E : Elements_Type renames Vector.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Vector.Last);
            E (Before .. Index_Type'Pred (Index)) := (others => null);

            Vector.Last := New_Last;
         end;

         return;

      end if;

      declare

         type Int is range System.Min_Int .. System.Max_Int;

         First : constant Int'Base :=
           Index_Type'Pos (Index_Type'First);

         New_Size : constant Int'Base :=
           Int'Base (New_Last_As_Size) - First + 1;

         Max_Size : constant Int'Base :=
           Int'Base (Index_Type'Last) - First + 1;

         Size, Dst_Last_As_Int : Int'Base;

      begin

         if New_Size >= Max_Size / 2 then

            Dst_Last := Index_Type'Last;

         else

            Size := Vector.Elements'Length;

            if Size = 0 then
               Size := 1;
            end if;

            while Size < New_Size loop
               Size := 2 * Size;
            end loop;

            Dst_Last_As_Int := First + Size - 1;
            Dst_Last := Index_Type (Dst_Last_As_Int);

         end if;

      end;

      Dst := new Elements_Type (Index_Type'First .. Dst_Last);

      declare
         Src : Elements_Type renames Vector.Elements.all;
      begin
         Dst (Index_Type'First .. Index_Type'Pred (Before)) :=
           Src (Index_Type'First .. Index_Type'Pred (Before));

         Dst (Index .. New_Last) := Src (Before .. Vector.Last);
      end;

      declare
         X : Elements_Access := Vector.Elements;
      begin
         Vector.Elements := Dst;
         Vector.Last := New_Last;

         Free (X);
      end;

   end Insert_N_Internal;


   procedure Insert_N (Vector : in out Vector_Type;
                       Before : in     Index_Type'Base;
                       Count  : in     Size_Type) is
   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype T is Index_Type range Index_Type'First .. Back (Vector);
      begin
         Insert_N_Internal (Vector, T'(Before), Count);
      end;

   end Insert_N;


   procedure Delete (Vector : in out Vector_Type;
                     Index  : in     Index_Type'Base) is
   begin

      if Index not in Index_Type'First .. Vector.Last then
         return;
      end if;

      declare
         E : Elements_Type renames Vector.Elements.all;
      begin

         --NOTE:
         --We could save the element ptr, slide the remaining
         --ptrs down, and then free the element.  However, we
         --do the Free first, in order to duplicate the
         --behavior of the range-form of delete.
         --END NOTE.

         begin
            Free (E (Index));
         exception
            when others =>
               E (Index) := null;
               raise;
         end;

         E (Index .. Index_Type'Pred (Vector.Last)) :=
           E (Index_Type'Succ (Index) .. Vector.Last);

         E (Vector.Last) := null;

         Vector.Last := Index_Type'Pred (Vector.Last);

      end;

   end Delete;


   procedure Delete_Last (Vector : in out Vector_Type) is
   begin
      Delete (Vector, Last (Vector));
   end;


   procedure Delete_N_Internal (Vector : in out Vector_Type;
                                First  : in     Index_Type;
                                Count  : in     Size_Type) is

      I : constant Size_Type'Base := Index_Type'Pos (First);

      Count2 : constant Size_Type'Base :=
        Size_Type'Base (Back (Vector)) - I;

      N : constant Size_Type'Base :=
        Size_Type'Min (Count, Count2);

      J_As_Size : constant Size_Type'Base := I + N;

      J : constant Index_Type := Index_Type (J_As_Size);

      E : Elements_Type renames Vector.Elements.all;

      New_Last_As_Size : constant Size_Type'Base :=
        Size_Type'Base (Vector.Last) - N;

      New_Last : constant Index_Type'Base :=
        Index_Type'Base (New_Last_As_Size);

   begin

      for I in First .. Index_Type'Pred (J) loop

         begin
            Free (E (I));
         exception
            when others =>
               E (I) := null;
               raise;
         end;

      end loop;

      E (First .. New_Last) := E (J .. Vector.Last);

      Vector.Last := New_Last;

   end Delete_N_Internal;


   procedure Delete_N (Vector : in out Vector_Type;
                       First  : in     Index_Type'Base;
                       Count  : in     Size_Type) is
   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype T is Index_Type range Index_Type'First .. Vector.Last;
      begin
         Delete_N_Internal (Vector, T'(First), Count);
      end;

   end Delete_N;


   procedure Delete_Internal (Vector  : in out Vector_Type;
                              From    : in     Index_Type;
                              Through : in     Index_Type) is

      N : constant Size_Type'Base :=
        Size_Type'Base (Through) - Size_Type'Base (From) + 1;

      E : Elements_Type renames Vector.Elements.all;

      New_Last_As_Size : constant Size_Type'Base :=
        Size_Type'Base (Vector.Last) - N;

      New_Last : constant Index_Type'Base :=
        Index_Type'Base (New_Last_As_Size);

   begin

      for I in From .. Through loop

         begin
            Free (E (I));
         exception
            when others =>
               E (I) := null;
               raise;
         end;

      end loop;

      E (From .. New_Last) :=
        E (Index_Type'Succ (Through) .. Vector.Last);

      Vector.Last := New_Last;

   end Delete_Internal;


   procedure Delete (Vector  : in out Vector_Type;
                     From    : in     Index_Type'Base;
                     Through : in     Index_Type'Base) is
   begin

      if Through < From then
         return;
      end if;

      declare
         subtype T is Index_Type
           range Index_Type'First .. Vector.Last;
      begin
         Delete_Internal (Vector, T'(From), T'(Through));
      end;

   end Delete;



   function Size (Vector : Vector_Type) return Size_Type is
   begin
      if Vector.Elements = null then
         return 0;
      end if;

      return Vector.Elements'Length;
   end;


   procedure Resize (Vector : in out Vector_Type;
                     Size   : in     Size_Type) is

      E : Elements_Access;

   begin

      if Size = 0 then
         return;
      end if;

      if Vector.Elements /= null
        and then Vector.Elements'Length >= Size
      then
         return;
      end if;

      declare
         F : constant Size_Type'Base := Index_Type'Pos (Index_Type'First);
         L : constant Size_Type'Base := F + Size - 1;
      begin
         E := new Elements_Type (Index_Type'First .. Index_Type (L));
      end;

      if Vector.Elements = null then
         Vector.Elements := E;
         return;
      end if;

      E (Index_Type'First .. Vector.Last) :=
         Vector.Elements (Index_Type'First .. Vector.Last);

      declare
         X : Elements_Access := Vector.Elements;
      begin
         Vector.Elements := E;
         Free (X);
      end;

   end Resize;


   function Front (Vector : Vector_Type) return Index_Type'Base is
      pragma Warnings (Off, Vector);
   begin
      return Index_Type'Pred (Index_Type'First);
   end;


   function First (Vector : Vector_Type) return Index_Type is
      pragma Warnings (Off, Vector);
   begin
      return Index_Type'First;
   end;


   function First_Element (Vector : Vector_Type) return Element_Type is
   begin
      return Element (Vector, Index_Type'First);
   end;


   function Last (Vector : Vector_Type) return Index_Type'Base is
   begin
      return Vector.Last;
   end;


   function Last_Element (Vector : Vector_Type) return Element_Type is
   begin
      return Element (Vector, Vector.Last);
   end;


   function Back (Vector : Vector_Type) return Index_Type'Base is
   begin
      return Index_Type'Succ (Vector.Last);
   end;


   function Element (Vector : Vector_Type;
                     Index  : Index_Type'Base)
     return Element_Type is

      subtype T is Index_Type range Index_Type'First .. Vector.Last;
   begin
      return Vector.Elements (T'(Index)).all;
   end;


   function Generic_Element (Vector : Vector_Type;
                             Index  : Index_Type'Base)
     return Element_Access is

      subtype T is Index_Type range Index_Type'First .. Vector.Last;

      X : constant Indefinite_Vectors.Element_Access :=
        Vector.Elements (T'(Index));
   begin
      return Element_Access (X);
   end;


   procedure Replace_Element (Vector : in Vector_Type;
                              Index  : in Index_Type'Base;
                              By     : in Element_Type) is

      subtype T is Index_Type range Index_Type'First .. Vector.Last;

      X : Element_Access := Vector.Elements (T'(Index));
   begin
      Vector.Elements (T'(Index)) := new Element_Type'(By);
      Free (X);
   end;


   procedure Generic_Constant_Iteration (Vector : in Vector_Type) is
   begin
      for I in Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I).all);
      end loop;
   end;


   procedure Generic_Iteration (Vector : in Vector_Type) is
   begin
      for I in Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I).all);
      end loop;
   end;


   procedure Generic_Constant_Reverse_Iteration (Vector : in Vector_Type) is
   begin
      for I in reverse Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I).all);
      end loop;
   end;


   procedure Generic_Reverse_Iteration (Vector : in Vector_Type) is
   begin
      for I in reverse Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I).all);
      end loop;
   end;


   procedure Generic_Sort (Vector : in Vector_Type) is


      function Is_Less (L, R : Element_Access) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.all < R.all;
      end;

      procedure Sort is
         new Generic_Sort_Unconstrained_Array
          (Index_Type,
           Element_Access,
           Elements_Type,
           "<" => Is_Less);

   begin

      if Vector.Elements = null then
         return;
      end if;

      Sort (Vector.Elements (Index_Type'First .. Vector.Last));

   end Generic_Sort;


   function Find
     (Vector : Vector_Type;
      Item   : Element_Type;
      Index  : Index_Type'Base := Index_Type'First)
     return Index_Type'Base is

      subtype T is Index_Type'Base
        range Index_Type'First .. Index_Type'Base'Last;

   begin

      for I in T'(Index) .. Vector.Last loop
         if Vector.Elements (I).all = Item then
            return I;
         end if;
      end loop;

      return Index_Type'Succ (Vector.Last);

   end;


   function Reverse_Find (Vector : Vector_Type;
                          Item   : Element_Type;
                          Index  : Index_Type'Base :=
                            Index_Type'Pred (Index_Type'First))
      return Index_Type'Base is

      subtype T is Index_Type'Base
        range Index_Type'Base'First .. Last (Vector);

      Last : T;

   begin

      if Index < Index_Type'First then
         Last := Vector.Last;
      else
         Last := Index;
      end if;

      for I in reverse Index_Type'First .. Last loop
         if Vector.Elements (I).all = Item then
            return I;
         end if;
      end loop;

      return Index_Type'Succ (Vector.Last);  --NOTE: back?

   end;


   function Is_In (Item   : Element_Type;
                   Vector : Vector_Type)
      return Boolean is
   begin
      return Find (Vector, Item) /= Back (Vector);
   end;



end AI302.Containers.Indefinite_Vectors;

