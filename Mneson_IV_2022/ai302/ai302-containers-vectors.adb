with AI302.Containers.Generic_Sort_Unconstrained_Array;
with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body AI302.Containers.Vectors is

   type Int is range System.Min_Int .. System.Max_Int;

   subtype Natural_Int is Int range 0 .. Int'Last;

   subtype Positive_Int is Int range 1 .. Int'Last;

   subtype Index_Int is Int range Index_Type'Pos (Index_Type'First) ..
                                   Index_Type'Pos (Index_Type'Last);

   procedure Free is
      new Ada.Unchecked_Deallocation (Elements_Type, Elements_Access);



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
         X : constant Elements_Access := Vector.Elements;
         L : constant Index_Type'Base := Vector.Last;
         E : Elements_Type renames X (Index_Type'First .. L);
      begin
         Vector.Elements := null;
         Vector.Last := Index_Type'Pred (Index_Type'First);

         Vector.Elements := new Elements_Type'(E);
         Vector.Last := L;
      end;

   end Adjust;


   procedure Finalize (Vector : in out Vector_Type) is

      X : Elements_Access := Vector.Elements;

   begin

      Vector.Elements := null;
      Vector.Last := Index_Type'Pred (Index_Type'First);

      Free (X);

   end Finalize;


   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Vector : in     Vector_Type) is

   begin

      Size_Type'Base'Write (Stream, Length (Vector));

      for I in Index_Type range Index_Type'First .. Vector.Last loop

         Element_Type'Write (Stream, Vector.Elements (I));

      end loop;

   end Write;


   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Vector :    out Vector_Type) is

      Length : Size_Type'Base;
      Last   : Index_Type'Base := Index_Type'Pred (Index_Type'First);

   begin

      Clear (Vector);

      Size_Type'Base'Read (Stream, Length);

      Resize (Vector, Size => Length);

      for I in Size_Type range 1 .. Length loop

         Last := Index_Type'Succ (Last);

         Element_Type'Read (Stream, Vector.Elements (Last));

         Vector.Last := Last;

      end loop;

   end Read;


--     function New_Vector return Vector_Type is
--     begin
--        return (Controlled with null, Index_Type'Pred (Index_Type'First));
--     end;


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

         Elements : constant Elements_Access :=
           new Elements_Type'(Index_Type'First .. Last => New_Item);

      begin

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

      for I in Index_Type range Index_Type'First .. Left.Last loop

         if Left.Elements (I) /= Right.Elements (I) then
            return False;
         end if;

      end loop;

      return True;

   end "=";



   function Length (Vector : Vector_Type) return Size_Type is

      L : constant Int := Int (Vector.Last);
      F : constant Index_Int := Int (Index_Type'First);

      N : constant Natural_Int := L - F + 1;
   begin
      return Size_Type (N);
   end;


   function Is_Empty (Vector : Vector_Type) return Boolean is
   begin
      return Vector.Last < Index_Type'First;
   end;


   procedure Clear (Vector : in out Vector_Type) is
   begin
      Vector.Last := Index_Type'Pred (Index_Type'First);
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
      Insert (Vector, Before, New_Item, 1);
   end;


   procedure Insert_N_Internal
     (Vector   : in out Vector_Type;
      Before   : in     Index_Type;
      Count    : in     Positive_Int;
      New_Item : in     Element_Type) is

      Old_Last : constant Index_Type'Base := Vector.Last;

      Old_Last_As_Int : constant Int := Int (Old_Last);

      New_Last_As_Int : constant Index_Int := Old_Last_As_Int + Count;

      New_Last : constant Index_Type := Index_Type (New_Last_As_Int);

      Index_As_Int : constant Index_Int := Index_Int (Before) + Count;

      Index : constant Index_Type := Index_Type (Index_As_Int);

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin

      if Vector.Elements = null then

         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. New_Last);
         begin
            Vector.Elements := new Elements_Subtype'(others => New_Item);
         end;

         Vector.Last := New_Last;

         return;

      end if;

      if New_Last <= Vector.Elements'Last then

         declare
            E : Elements_Type renames Vector.Elements.all;
         begin
            E (Index .. New_Last) :=
              E (Before .. Vector.Last);

            E (Before .. Index_Type'Pred (Index)) :=
              (others => New_Item);
         end;

         Vector.Last := New_Last;

         return;

      end if;

      declare

         First_Int : constant Index_Int := Int (Index_Type'First);

         New_Size : constant Positive_Int :=
           New_Last_As_Int - First_Int + 1;

         Max_Size : constant Positive_Int :=
           Index_Int (Index_Type'Last) - First_Int + 1;

         Size : Natural_Int;
         Dst_Last_As_Int : Index_Int;

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

            Dst_Last_As_Int := First_Int + Size - 1;
            Dst_Last := Index_Type (Dst_Last_As_Int);

         end if;

      end;

      Dst := new Elements_Type (Index_Type'First .. Dst_Last);

      declare
         Src : Elements_Type renames Vector.Elements.all;
      begin
         Dst (Index_Type'First .. Index_Type'Pred (Before)) :=
           Src (Index_Type'First .. Index_Type'Pred (Before));

         Dst (Before .. Index_Type'Pred (Index)) :=
           (others => New_Item);

         Dst (Index .. New_Last) := Src (Before .. Vector.Last);
      exception
         when others =>
            Free (Dst);
            raise;
      end;

      declare
         X : Elements_Access := Vector.Elements;
      begin
         Vector.Elements := Dst;
         Vector.Last := New_Last;

         Free (X);
      end;

   end Insert_N_Internal;


--     procedure Insert (Vector   : in out Vector_Type;
--                       Before   : in     Index_Type'Base) is
--     begin
--        Insert_N (Vector, Before, 1);
--     end;


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
         Insert_N_Internal (Vector, T'(Before), Positive_Int (Count), New_Item);
      end;

   end Insert;


   procedure Insert_N_Internal
     (Vector : in out Vector_Type;
      Before : in     Index_Type;     --already vetted
      Count  : in     Positive_Int) is

      New_Last_As_Int : constant Index_Int := Int (Vector.Last) + Count;

      New_Last : constant Index_Type := Index_Type (New_Last_As_Int);

      Index_As_Int : constant Index_Int := Index_Int (Before) + Count;

      Index : constant Index_Type := Index_Type (Index_As_Int);

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
         end;

         Vector.Last := New_Last;

         return;

      end if;

      declare

         First_Int : constant Index_Int := Int (Index_Type'First);

         New_Size : constant Positive_Int :=
           New_Last_As_Int - First_Int + 1;

         Max_Size : constant Positive_Int :=
           Index_Int (Index_Type'Last) - First_Int + 1;

         Size : Natural_Int;
         Dst_Last_As_Int : Index_Int;

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

            Dst_Last_As_Int := First_Int + Size - 1;
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
      exception
         when others =>
            Free (Dst);
            raise;
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
         Insert_N_Internal (Vector, T'(Before), Count => Positive_Int (Count));
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
         E (Index .. Index_Type'Pred (Vector.Last)) :=
           E (Index_Type'Succ (Index) .. Vector.Last);
      end;

      Vector.Last := Index_Type'Pred (Vector.Last);

   end Delete;


   procedure Delete_Last (Vector : in out Vector_Type) is
   begin
      Delete (Vector, Index => Last (Vector));
   end;


--     procedure Delete_N_Internal (Vector : in out Vector_Type;
--                                  First  : in     Index_Type;
--                                  Count  : in     Positive_Element_Count) is

--        Count2 : constant Positive_Element_Count :=
--          Index_Type'Pos (Vector.Last) -
--          Index_Type'Pos (Index_Subtype'(First)) + 1;

--        N : constant Positive_Element_Count :=
--          Element_Count'Min (Count, Count2);

--        New_Last : constant Index_Type'Base :=
--          Vector.Last - Element_Count'Pos (N);

--        E : Elements_Type renames Vector.Elements.all;

--     begin

--        E (First .. New_Last) :=
--          E (First + Element_Count'Pos (N) .. Vector.Last);

--        Vector.Last := New_Last;

--     end Delete_N_Internal;


--     procedure Delete_N (Vector : in out Vector_Type;
--                         First  : in     Index_Type'Base;
--                         Count  : in     Element_Count) is
--     begin

--        if Count = 0 then
--           return;
--        end if;

--        declare
--           subtype T is Index_Type range Index_Type'First .. Vector.Last;
--        begin
--           Delete_N_Internal (Vector, T'(First), Count);
--        end;

--     end Delete_N;


   procedure Delete_Internal (Vector : in out Vector_Type;
                              First  : in     Index_Type;
                              Last   : in     Index_Type) is

      F : constant Index_Int := Int (First);
      L : constant Index_Int := Int (Last);

      N : constant Positive_Int := L - F + 1;

      New_Last_As_Int : constant Int :=
        Index_Int (Vector.Last) - N;

      New_Last : constant Index_Type'Base :=
        Index_Type'Base (New_Last_As_Int);

      E : Elements_Type renames Vector.Elements.all;

   begin

      E (First .. New_Last) :=
        E (Index_Type'Succ (Last) .. Vector.Last);

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
         Delete_Internal (Vector, First => T'(From), Last => T'(Through));
      end;

   end Delete;


   procedure Delete_N (Vector : in out Vector_Type;
                       First  : in     Index_Type'Base;
                       Count  : in     Size_Type) is
   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype T is Index_Type
           range Index_Type'First .. Vector.Last;

         I : constant T := First;

         B : constant Index_Type'Base := Back (Vector);

         Count2 : constant Size_Type :=
           Index_Type'Pos (B) - Index_Type'Pos (I);

         N : constant Size_Type := Size_Type'Min (Count, Count2);

         J_Int : constant Index_Int :=
           Index_Int (I) + Positive_Int (N) - 1;

         J : constant T := Index_Type (J_Int);
      begin
         Delete_Internal (Vector, First => I, Last => J);
      end;

   end Delete_N;


   function Size (Vector : Vector_Type) return Size_Type is
   begin
      if Vector.Elements = null then
         return 0;
      end if;

      return Vector.Elements'Length;
   end;


   procedure Resize (Vector : in out Vector_Type;
                     Size   : in     Size_Type) is

      Last_Int : Index_Int;
      Last     : Index_Type;

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

      Last_Int := Index_Int (Index_Type'First) + Positive_Int (Size) - 1;
      Last := Index_Type (Last_Int);

      E := new Elements_Type (Index_Type'First .. Last);

      if Vector.Elements = null then
         Vector.Elements := E;
         return;
      end if;

      begin
         E (Index_Type'First .. Vector.Last) :=
           Vector.Elements (Index_Type'First .. Vector.Last);
      exception
         when others =>
            Free (E);
            raise;
      end;

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
      return Vector.Elements (T'(Index));
   end;


   function Generic_Element (Vector : Vector_Type;
                             Index  : Index_Type'Base)
     return Element_Access is

      subtype T is Index_Type range Index_Type'First .. Vector.Last;
   begin
      return Vector.Elements (T'(Index))'Access;
   end;


   procedure Replace_Element (Vector : in Vector_Type;
                              Index  : in Index_Type'Base;
                              By     : in Element_Type) is

      subtype T is Index_Type range Index_Type'First .. Vector.Last;
   begin
      Vector.Elements (T'(Index)) := By;
   end;


   procedure Generic_Constant_Iteration (Vector : in Vector_Type) is
   begin
      for I in Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I));
      end loop;
   end;


   procedure Generic_Iteration (Vector : in Vector_Type) is
   begin
      for I in Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I));
      end loop;
   end;


   procedure Generic_Constant_Reverse_Iteration (Vector : in Vector_Type) is
   begin
      for I in reverse Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I));
      end loop;
   end;


   procedure Generic_Reverse_Iteration (Vector : in Vector_Type) is
   begin
      for I in reverse Index_Type'First .. Vector.Last loop
         Process (Vector.Elements (I));
      end loop;
   end;


   procedure Generic_Sort (Vector : in Vector_Type) is

      procedure Sort is
         new Generic_Sort_Unconstrained_Array
          (Index_Type,
           Element_Type,
           Elements_Type,
           "<");

   begin

      if Vector.Elements = null then
         return;
      end if;

      Sort (Vector.Elements (Index_Type'First .. Vector.Last));

   end Generic_Sort;


   function Find
     (Vector : Vector_Type;
      Item   : Element_Type;
      Index  : Index_Type'Base := Index_Type'First) return Index_Type'Base is

      subtype T is Index_Type'Base
        range Index_Type'First .. Index_Type'Base'Last;

   begin

      for I in T'(Index) .. Vector.Last loop
         if Vector.Elements (I) = Item then
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
         if Vector.Elements (I) = Item then
            return I;
         end if;
      end loop;

      return Index_Type'Succ (Vector.Last);  --NOTE: back?

   end;




   function Is_In (Item   : Element_Type;
                   Vector : Vector_Type) return Boolean is
   begin
      return Find (Vector, Item) /= Back (Vector);
   end;



end AI302.Containers.Vectors;

