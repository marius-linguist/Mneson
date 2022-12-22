package body Relation_Maps is
   
   use Data_Maps;


   function Compare_Dates (L, R : in String) return Boolean is
      
      LC : Data_Maps.Cursor_Type := Find (Dates, Key => L);
      RC : Data_Maps.Cursor_Type := Find (Dates, Key => R);
      B  : Boolean;
   begin
      
      if LC = Back (Dates) then
         --assume L is an advisor
         pragma Assert (RC /= Back (Dates));
         Insert (Dates, L, "!", LC, B);
         pragma Assert (B);
         pragma Assert (LC /= Back (Dates));
         Insert (Places, L, "$", LC, B);
         
      elsif RC = Back (Dates) then
         --assume R is an advisor
         pragma Assert (LC /= Back (Dates));
         Insert (Dates, R, "!", RC, B);
         pragma Assert (B);
         pragma Assert (RC /= Back (Dates));
         Insert (Places, R, "$", RC, B);
         
      end if;
      
      return Element (LC) < Element (RC);
      
   end Compare_Dates;


   function "<" (L, R : Vector_Type) return Boolean is
   begin
      
      --NOTE:
      --I don't think this actually gets called, because we're
      --always inserting names (keys) not vectors, but I'll
      --define it anyway.
      --END NOTE.
      
      if Is_Empty (L) then
         return not Is_Empty (R);
      end if;
      
      if Is_Empty (R) then
         return False;
      end if;
      
      declare
         LN : String renames To_Access (L, First (L)).all;
         RN : String renames To_Access (R, First (R)).all;
      begin
         return Compare_Dates (LN, RN);
      end;

   end "<";
      

   function "<" (L : String;
                 R : Vector_Type) return Boolean is
   begin

      if L'Length = 0 then
         return not Is_Empty (R);
      end if;
      
      if Is_Empty (R) then
         return False;
      end if;
      
      declare
         RN : String renames To_Access (R, First (R)).all;
      begin
         return Compare_Dates (L, RN);
      end;

   end "<";
   

   function ">" (L : String;
                 R : Vector_Type) return Boolean is
   begin
      
      if L'Length = 0 then
         return False;
      end if;
      
      if Is_Empty (R) then
         return True;
      end if;
      
      return Compare_Dates (First_Element (R), L);

   end ">";


   package Name_Keys is
      new Date_Ordered_Sets.Generic_Keys (String);

--to test definite set
   procedure Append_Name
     (Names : in out Vector_Type;
      Name  : in     String) is
   begin
      Append (Vector => Names, New_Item => Name);
   end;

   package Name_Insertion is
      new Name_Keys.Generic_Insertion (Set_Key => Append_Name);
   
--to test indefinite set
--     function To_Vector (Name : String) return Vector_Type is
--     begin
--        return To_Vector (New_Item => Name, Count => 1);
--     end;
   
--     package Name_Insertion is
--        new Name_Keys.Generic_Insertion (New_Element => To_Vector);
   

   procedure Insert
     (Set  : in out Date_Ordered_Sets.Set_Type;
      Name : in     String) is
      
      C : Date_Ordered_Sets.Cursor_Type;
      B : Boolean;
   begin
      Name_Insertion.Insert (Set, Name, C, B);
      
      if not B then
         declare
            V : String_Vectors.Vector_Type renames To_Access (C).all;
         begin
            Append (V, Name);
         end;
      end if;
   end Insert;
   
   
   function Is_Less_Date_Vector
     (D : String;
      V : String_Vectors.Vector_Type) return Boolean is
      
      pragma Assert (D'Length > 0);
      pragma Assert (not Is_Empty (V));
   begin
      return D < Element (Dates, Key => First_Element (V));
   end;
   
   function Is_Greater_Date_Vector
     (D : String;
      V : String_Vectors.Vector_Type) return Boolean is

      pragma Assert (D'Length > 0);
      pragma Assert (not Is_Empty (V));
   begin
      return D > Element (Dates, Key => First_Element (V));
   end;
   
   package Date_Keys is
      new Date_Ordered_Sets.Generic_Keys 
     (Key_Type => String,
      "<" => Is_Less_Date_Vector,
      ">" => Is_Greater_Date_Vector);

   function Find
     (Set  : Date_Ordered_Sets.Set_Type;
      Date : String) return Date_Ordered_Sets.Cursor_Type is
   begin
      return Date_Keys.Find (Set, Key => Date);
   end;
   

--I added this to vectors directly.
--     function Is_In
--       (Vector : String_Vectors.Vector_Type;
--        Name   : String) return Boolean is
      
--     begin

--        for Index in First (Vector) .. Last (Vector) loop

--           declare
--              E : String renames To_Access (Vector, Index).all;
--           begin
--              if Case_Insensitive."=" (Name, E) then
--                 return True;
--              end if;
--           end;

--        end loop;
      
--        return False;

--     end Is_In;
      


   function Is_In
     (Set  : Date_Ordered_Sets.Set_Type;
      Name : String) return Boolean is
      
      DC : constant Data_Maps.Cursor_Type := 
        Find (Map => Dates, Key => Name);

   begin
      
      if DC = Back (Dates) then
         return False;
      end if;
      
      declare 
         Date : constant String := Element (DC);

         SC  : constant Date_Ordered_Sets.Cursor_Type := 
           Find (Set, Date);

         use Date_Ordered_Sets;
      begin
         if SC = Back (Set) then
            return False;
         end if;
         
         return Is_In (Name, Vector => To_Access (SC).all);
      end;
      
   end Is_In;
      
      


end Relation_Maps;

