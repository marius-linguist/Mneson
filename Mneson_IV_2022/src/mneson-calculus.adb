-- PACKAGE MNESON.CALCULUS (GENERIC BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with ada.text_io;use ada.text_io;

with Mneson.Structures;

package body Mneson.Calculus is

   package Structures is new Mneson.Structures (Work);
   use Structures;
   use Work;

   -- Attribute Types
   Element_AT   : constant Vertex := To_Vertex ("element" );
   Intersect_AT : constant Vertex := To_Vertex ("intersct");
   Set_AT       : constant Vertex := To_Vertex ("set"     );
   Set_1_AT     : constant Vertex := To_Vertex ("set_1"   );
   Set_2_AT     : constant Vertex := To_Vertex ("set_2"   );
   Singleton_AT : constant Vertex := To_Vertex ("singletn");
   Sources_AT   : constant Vertex := To_Vertex ("sources" );
   Subtract_AT  : constant Vertex := To_Vertex ("subtract");
   Targets_AT   : constant Vertex := To_Vertex ("targets" );
   List_AT      : constant Vertex := To_Vertex ("list"    );
   Start_Ptr_AT : constant Vertex := To_Vertex ("startptr");
   End_Ptr_AT   : constant Vertex := To_Vertex ("endptr"  );
   Order_Type_AT: constant Vertex := To_Vertex ("ordertyp");
 
   function To_Selection (Set : Vertex) return Selection is
      Y : Selection;
   begin
      Mark (Calculus.Marker);
      Y := New_Record
             ((1 => Set_AT,
               2 => New_Instance (Set_AT, Set)));
      Unmark (Calculus.Marker);
      return Y;
   exception
      when others =>
         Unmark (Calculus.Marker);
         raise;
   end;
-- This function was simply:
--      return New_Transient_Record
--        ((Set_AT,
--          New_Instance (Set_AT, Set)));
-- (with no local variables)
-- before the implementation of Mark.
-- All other Calculus primitives were changed similarly.

   function Singleton (X : Vertex) return Selection is
      Y : Selection;
   begin
--put_Line("SINGLETON enter");
      Mark (Calculus.Marker);
      Y := New_Record
        ((Singleton_AT,
          New_Instance (Element_AT, X)));
      Unmark (Calculus.Marker);
--put_Line("SINGLETON exit");
      return Y;
   exception
      when others =>
         Unmark (Calculus.Marker);
         raise;
   end;

   function Targets (Set : Selection) return Selection is
      Y : Selection;
   begin
      Mark (Calculus.Marker);
      Y := New_Record
        ((Targets_AT,
          New_Instance (Set_AT, Set)));
      Unmark (Calculus.Marker);
      return Y;
   exception
      when others =>
         Unmark (Calculus.Marker);
         raise;
   end;

   function Sources (Set : Selection) return Selection is
      Y : Selection;
   begin
      Mark (Calculus.Marker);
      Y := New_Record
        ((Sources_AT,
          New_Instance (Set_AT, Set)));
      Unmark (Calculus.Marker);
      return Y;
   exception
      when others =>
         Unmark (Calculus.Marker);
         raise;
   end;

--   function To_List
--     (First : Vertex; Order_Type : Vertex := Next) return Selection
--   is
--      return New_Transient_Record
--        ((List_AT),
--         New_Instance (First_AT, First),
--         New_Instance (Order_Type_AT, Order_Type));
--   end;
   
   function Intersect (Set_1, Set_2 : Selection) return Selection is
      Y : Selection;
   begin
      Mark (Calculus.Marker);
      Y := New_Record
        ((Intersect_AT,
          New_Instance (Set_1_AT, Set_1),
          New_Instance (Set_2_AT, Set_2)));
      Unmark (Calculus.Marker);
      return Y;
   exception
      when others =>
         Unmark (Calculus.Marker);
         raise;
   end;

   function Subtract (Set_1, Set_2 : Selection) return Selection is
      Y : Selection;
   begin
      Mark (Calculus.Marker);
      Y := New_Record
        ((Subtract_AT,
          New_Instance (Set_1_AT, Set_1),
          New_Instance (Set_2_AT, Set_2)));
      Unmark (Calculus.Marker);
      return Y;
   exception
      when others =>
         Unmark (Calculus.Marker);
         raise;
   end;

   procedure For_Each (Set : Selection; Process : Process_Vertex) is
   begin
      if Connected (Set, Set_AT) then
         For_Each_Target (Get_Value (Set, Set_AT), Process);
      elsif Connected (Set, Singleton_AT) then
         Process (Get_Value (Set, Element_AT));
      elsif Connected (Set, Targets_AT) then
         declare
            procedure Process_Targets (X : Vertex) is
            begin
               For_Each_Target (X, Process);
            end;
         begin
            For_Each
              (Get_Value (Set, Set_AT),
               Process_Targets'Unrestricted_Access);
         end;
      elsif Connected (Set, Sources_AT) then
         declare
            procedure Process_Sources (X : Vertex) is
            begin
               For_Each_Source (X, Process);
            end;
         begin
            For_Each
              (Get_Value (Set, Set_AT),
               Process_Sources'Unrestricted_Access);
         end;
      elsif Connected (Set, Intersect_AT) then
         declare
            Set_2 : Selection := Get_Value (Set, Set_2_AT);
            procedure Process_If_In_Set_2 (X : Vertex) is
            begin
              if Is_In (X, Set_2) then Process (X); end if;
            end;
         begin
            For_Each
              (Get_Value (Set, Set_1_AT),
               Process_If_In_Set_2'Unrestricted_Access);
         end;
      elsif Connected (Set, Subtract_AT) then
         declare
            Set_2 : Selection := Get_Value (Set, Set_2_AT);
            procedure Process_If_Not_In_Set_2 (X : Vertex) is
            begin
              if not Is_In (X, Set_2) then Process (X); end if;
            end;
         begin
            For_Each
              (Get_Value (Set, Set_1_AT),
               Process_If_Not_In_Set_2'Unrestricted_Access);
         end;
  --    elsif Connected (Set, List_AT) then
  --       declare
  --          Current, Next_AT : Vertex;
  --       begin
  --          Current := Get_Value (Set, First_AT);
  --          Next_AT := Get_Value (Set, Order_Type_AT);
  --          loop
  --             Process (Current);
  --             Current := Get_Value (Current, Next_AT);
  --          end loop;
  --       exception
  --          when Constraint_Error => return;
  --       end;
      else
         raise Type_Error;
      end if;
   end;

   function Is_In (X : Vertex; Set : Selection) return Boolean is -- O(#Set)
      Yes : exception;
      procedure Compare (Element : Vertex) is
      begin
         if X = Element then raise Yes; end if;
      end;
   begin
      For_Each (Set, Compare'Unrestricted_Access);
      return False;
   exception
      when Yes => return True;
   end;

   function Extract
     (Set : Selection; Index : Positive := 1) return Vertex -- O(Index)
   is
      Result : Vertex;
      N : Natural := 0;
      Done : exception;
      procedure Return_Indexed (Element : Vertex) is
      begin
         N := N + 1;
         if N = Index then
            Result := Element;
            raise Done;
         end if;
      end;
   begin
      For_Each (Set, Return_Indexed'Unrestricted_Access);
      raise Constraint_Error;
   exception
      when Done => return Result;
   end;

   procedure Clear is
      Ok, No_More : exception;
      function Extract_Target (Source : Vertex) return Vertex is
         Target : Vertex;
         procedure Get_It_And_Raise_Ok (X : Vertex) is
         begin
            Target := X;
            raise Ok;
         end;
      begin
         For_Each_Target (Source, Get_It_And_Raise_Ok'Unrestricted_Access);
         raise No_More;
      exception
         when Ok => return Target;
      end;
   begin
      loop
         Disconnect (Extract_Target (Calculus.Marker));
      end loop;
   exception
      when No_More => return;
   end;
end;
