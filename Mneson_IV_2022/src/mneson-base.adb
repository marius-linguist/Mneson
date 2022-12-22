-- PACKAGE MNESON.BASE (GENERIC BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Direct_IO;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with GNAT.Bubble_Sort_A;
with Mneson.Monitors;

package body Mneson.Base is

   -- GRAPH STATE
   use Ada.Strings.Unbounded;
   Current_Name       : Unbounded_String;
   Links              : Link_Sets.Set_Type;
   Inv_Links          : Link_Sets.Set_Type;
   String_Table       : String_Maps.Map_Type;
   Short_Strings_File : Short_String_IO.File_Type;
   Last_Serial_Number : Vertex_Count;
   Graph_Open         : Boolean := False;
   Monitor            : Process_String :=
                           Mneson.Monitors.Null_Monitor'Access;

   -- FILENAMES
   type Physical_Files is (Structures, Short_Strings);
   Suffix : constant String := ".mn";

   ----------
   -- MISC --
   ----------

   procedure Inc (X : in out Integer) is begin X := X + 1; end;
   Slice_Size_For_Comparison : constant := 20;

   --------------------------------
   -- FILE MANAGEMENT OPERATIONS --
   --------------------------------

   function Filename
     (Name : String ; File : Physical_Files) return String is
   begin
      return Name & "-" & Physical_Files'Image (File) & Suffix;
   end;

   procedure Create (Name: String) is
      Short_Strings_Filename : String := Filename (Name, Short_Strings);
      use Short_String_IO;
      use Ada.Exceptions;
      procedure Close_Any_Open_Files is
      begin
         Monitor ("Closing any open files");
         if Is_Open (Short_Strings_File) then
            Close (Short_Strings_File);
         end if;
      end;
   begin
      if Graph_Open then raise State_Error; end if;
      Monitor ("Creating file " & Short_Strings_Filename);
      begin
         Create (Short_Strings_File, Inout_File, Short_Strings_Filename);
      exception
         when E : others =>
            Monitor (Exception_Name (E));
            Close_Any_Open_Files;
            Monitor ("Create failed; graph not changed");
            raise;
      end;
      Monitor ("Initializing graph with zero elements");
      Link_Sets.Clear (Links);
      Link_Sets.Clear (Inv_Links);
      String_Maps.Clear (String_Table);
      Last_Serial_Number := 0;
      Current_Name := To_Unbounded_String (Name);
      Graph_Open := True;
--      Create_System;
      Monitor ("Create complete");
   end;

   procedure Open (Name : String) is
      Structures_Filename : String := Filename (Name, Structures);
      Short_Strings_Filename : String := Filename (Name, Short_Strings);
      Structures_File : Ada.Streams.Stream_IO.File_Type;
      use Ada.Streams.Stream_IO;
      use Link_Sets;
      use String_Maps;
      use Short_String_IO;
      use Ada.Exceptions;
      procedure Close_Any_Open_Files is
      begin
         Monitor ("Closing any open files");
         if Is_Open (Structures_File) then
            Close (Structures_File);
         end if;
         if Is_Open (Short_Strings_File) then
            Close (Short_Strings_File);
         end if;
      end;
   begin
      if Graph_Open then raise State_Error; end if;
      begin
         Monitor ("Opening file " & Structures_Filename);
         Open (Structures_File, In_File, Structures_Filename);
       exception
         when E : others =>
            Monitor (Exception_Name (E));
            Close_Any_Open_Files;
            Monitor ("Open failed; graph not changed");
            raise;
      end;
      begin
         Monitor ("Opening file " & Short_Strings_Filename);
         Open (Short_Strings_File, Inout_File, Short_Strings_Filename);
      exception
         when E : others =>
            Monitor (Exception_Name (E));
            Close_Any_Open_Files;
            Monitor ("Open failed; graph not changed");
            raise;
      end;
      Monitor ("Reading Links");
      Link_Sets.Set_Type'Read (Stream (Structures_File), Links);
      Monitor ("Reading Inv_Links");
      Link_Sets.Set_Type'Read (Stream (Structures_File), Inv_Links);
      Monitor ("Reading String_Table");
      String_Maps.Map_Type'Read (Stream (Structures_File), String_Table);
      Monitor ("Graph initialization: final details");
      Vertex_Count'Read (Stream (Structures_File), Last_Serial_Number);
      Current_Name := To_Unbounded_String (Name);
      Monitor ("Closing file " & Structures_Filename);
      Close (Structures_File);
      Graph_Open := True;
--      Create_System;
      Monitor ("Open complete");
   end;

   procedure Close is
      Name : String := To_String (Current_Name);
      Structures_Filename : String := Filename (Name, Structures);
      Structures_File : Ada.Streams.Stream_IO.File_Type;
      use Ada.Exceptions;
      use Ada.Streams.Stream_IO;
   begin
      if not Graph_Open then raise State_Error; end if;
      begin
         Monitor ("Opening file " & Structures_Filename & " for writing");
         Create (Structures_File, Out_File, Structures_Filename);
      exception
         when E : others =>
            Monitor (Exception_Name (E));
            if Is_Open (Structures_File) then
               Monitor ("Closing file " & Structures_Filename);
               Close (Structures_File);
            end if;
            Monitor ("Close failed; graph not changed");
            raise;
      end;
      Monitor ("Writing Links");
      Link_Sets.Set_Type'Write (Stream (Structures_File), Links);
      Monitor ("Writing Inv_Links");
      Link_Sets.Set_Type'Write (Stream (Structures_File), Inv_Links);
      Monitor ("Writing String_Table");
      String_Maps.Map_Type'Write (Stream (Structures_File), String_Table);
      Monitor ("Finalizing");
      Vertex_Count'Write (Stream (Structures_File), Last_Serial_Number);
      Close (Structures_File);
      Short_String_IO.Close (Short_Strings_File);
--      Delete_System;
      Graph_Open := False;
      Monitor ("Close complete");
   end;

   procedure Save is
      Name : String := To_String (Current_Name);
   begin
      if not Graph_Open then raise State_Error; end if;
      Monitor ("Saving (Save is implemented as closing and reopening)");
      Close;
      Open (Name);
      Monitor ("Save complete");
   end;

   procedure Save_As (Name : String) is
      procedure Copy_File (From, To : String) is
         use Ada.Text_IO;
         From_File, To_File : File_Type;
         C : Character;
      begin
         Monitor ("Copying file " & From & " to " & To);
         Open (From_File, In_File, From);
         Create (To_File, Out_File, To);
         while not End_Of_File (From_File) Loop
            Get_Immediate (From_File, C);
            Put (To_File, C);
         end loop;
         Close (From_File);
         Close (To_File);
      exception
         when others =>
            if Is_Open (From_File) then Close (From_File); end if;
            if Is_Open (To_File) then Close (To_File); end if;
            raise;
      end;
      procedure Delete_File (Name : String) is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Monitor ("Deleting file " & Name);
         Open (File, Out_File, Name);
         Delete (File);
      end;
   begin
      if not Graph_Open then raise State_Error; end if;
      Monitor ("Saving as " & Name);
      Close;
      for I in Physical_Files loop
         Copy_File
           (From => Filename (To_String (Current_Name), I),
            To => Filename (Name, I));
      end loop;
      for I in Physical_Files loop
         null; --Delete_File (Filename (To_String (Current_Name), I));
         -- Delete or not delete From_File, that's the question.
      end loop;
      Open (Name);
      Monitor ("Save_As complete");
   end;

   procedure Set_Monitor (Monitor : Process_String) is
   begin
      Mneson.Base.Monitor := Monitor;
   end;

   procedure Set_Logging (On : Boolean) is
   begin
      raise Not_Implemented_Yet;
   end;

   function Logging_On return Boolean is
   begin
      raise Not_Implemented_Yet;
      return False;
   end;

   ----------
   -- MARK --
   ----------
   
   procedure Mark (Marker : Vertex) is
   begin
      Connect (Markers, Marker);
   end;
   
   procedure Unmark (Marker : Vertex) is
   begin
      Disconnect (Markers, Marker);
   end;

   ---------------------------------
   -- VALUELESS VERTEX OPERATIONS --
   ---------------------------------

   function New_Serial_Number return Serial_Number is
   begin
      if not Graph_Open then raise State_Error; end if;
      Inc (Last_Serial_Number);
      return Last_Serial_Number;
   end;

   function Valueless_Vertex (Number : Serial_Number) return Vertex is
   begin
      if not Graph_Open then raise State_Error; end if;
      if Number > Last_Serial_Number then
         Last_Serial_Number := Number;
      end if;
      return (Serial_Number_Tip, Modular_64 (Number));
   end;

   function New_Vertex return Vertex is
--use ada.text_io;
      X : Vertex;
      procedure Mark_X (Marker : Vertex) is
      begin
--put_line("MARK_X "&img(Marker)&img(X));
         Connect (Marker, X);
      end;
   begin
--put_line("NEW_VERTEX x := ...");
      X := Valueless_Vertex (New_Serial_Number);
--put_line("NEW_VERTEX for each marker");
      For_Each_Target (Markers, Mark_X'Unrestricted_Access);
--put_line("NEW_VERTEX done");
--declare
-- procedure check(x:vertex) is
-- begin put_line("CHECK"&img(x));
-- end;
--begin
-- for_each_target(markers,check'unrestricted_access);
--end;
      return X;
   end;

   -------------------------------
   -- INTEGER VERTEX OPERATIONS --
   -------------------------------

   function To_Vertex (Value : Integer) return Vertex is
   begin
      return (Integer_Tip, Modular_64 (Value));
   end;

   function Value (X : Vertex) return Integer is
   begin
      if X.Tip = Integer_Tip then
         return Integer (X.Cue);
      else
         raise Type_Error;
      end if;
   end;

   -----------------------------
   -- FLOAT VERTEX OPERATIONS --
   -----------------------------

   function To_Modular_64 is new Ada.Unchecked_Conversion
     (Source => Float, Target => Modular_64);

   function To_Float is new Ada.Unchecked_Conversion
     (Source => Modular_64, Target => Float);

   function To_Vertex (Value : Float) return Vertex is
   begin
      return (Tip => Float_Tip, Cue => To_Modular_64 (Value));
   end;

   function Value (X : Vertex) return Float is
   begin
      if X.Tip = Float_Tip then
         return To_Float (X.Cue);
      else
         raise Type_Error;
      end if;
   end;

   ------------------------------
   -- STRING VERTEX OPERATIONS --
   ------------------------------

   function To_Modular_64 is new Ada.Unchecked_Conversion
     (Source => String_8, Target => Modular_64);

   function To_String_8 is new Ada.Unchecked_Conversion
     (Source => Modular_64, Target => String_8);

   function To_Vertex (Value : String) return Vertex is
      N : Natural_16 := Value'Length;
      X : Vertex;
      use Ada.Characters.Latin_1;
      use Ada.Strings.Fixed;
   begin
      if N <= Tiny_String_Tip'Last then
         X.Tip := Natural_16 (N);
         X.Cue := To_Modular_64 (String_8 (Head (Value, 8, NUL)));
      elsif N <= Short_String_Tip'Last then
         declare
            use String_Maps;
            use Short_String_IO;
            Found, Dont_Need : String_Maps.Cursor_Type;
            Expected_True : Boolean;
         begin
            Found := Find (String_Table, Value);
            if Found /= Null_Cursor then
               X := Element (Found);
            else -- new string
               X.Tip := Natural_16 (N);
               X.Cue := Modular_64 (Size (Short_Strings_File) + 1);
               Set_Index (Short_Strings_File, Positive_Count (X.Cue));
               for I in Value'Range loop
                  Write (Short_Strings_File, Value (I));
               end loop;
               Insert
                  (Map => String_Table,
                   Key => Value,
                   New_Item => X,
                   Cursor => Dont_Need,
                   Success => Expected_True);
            end if;
         end;
      else
         raise Not_Implemented_Yet;
      end if;
      return X;
   end;

   function Value (X : Vertex) return String is
   begin
      return Slice (X, 1, Length (X));
   end;

   function Length (X : Vertex) return Standard.Natural is
   begin
      if X.Tip in Tiny_String_Tip or X.Tip in Short_String_Tip then
         return Standard.Natural (X.Tip);
      elsif X.Tip = Long_String_Tip or X.Tip = Huge_String_Tip then
         raise Not_Implemented_Yet;
      else
         raise Type_Error;
      end if;
   end;

   function Slice
      (X : Vertex;
       Low : Standard.Positive;
       High : Standard.Natural)
      return String is
   begin
      if High < Low then
         return "";
      elsif X.Tip in Tiny_String_Tip then
         return To_String_8 (X.Cue) (1 .. Standard.Natural (X.Tip)) (Low .. High);
      elsif X.Tip in Short_String_Tip then
         declare
            S : String (Low .. High);
            use Short_String_IO;
         begin
            Set_Index
              (Short_Strings_File,
               Positive_Count (X.Cue) + Positive_Count (Low) - 1);
            for I in S'Range loop
               Read (Short_Strings_File, S (I));
            end loop;
            return S;
         end;
      elsif X.Tip = Long_String_Tip then
         raise Not_Implemented_Yet;
      elsif X.Tip = Huge_String_Tip then
         raise Not_Implemented_Yet;
      else
         raise Type_Error;
      end if;
   end;

   -------------------------------
   -- PROCESS_VERTEX OPERATIONS --
   -------------------------------
   
   type Modular_X is mod 2 ** Process_Vertex_Size;
   
   function To_Modular_X is new Ada.Unchecked_Conversion
     (Source => Process_Vertex, Target => Modular_X);

   function To_Process_Vertex is new Ada.Unchecked_Conversion
     (Source => Modular_X, Target => Process_Vertex);

   function To_Vertex (Value : Process_Vertex) return Vertex is
   begin
      return
        (Tip => Process_Vertex_Tip,
         Cue => Modular_64 (To_Modular_X (Value)));
   end;

   function Value (X : Vertex) return Process_Vertex is
   begin
      if X.Tip = Process_Vertex_Tip then
         return To_Process_Vertex (Modular_X (X.Cue));
      else
         raise Type_Error;
      end if;
   end;

   ------------------
   -- VERTEX ORDER --
   ------------------

   function Is_Integer (X : Vertex) return Boolean is
   begin
      return X.Tip = Integer_Tip;
   end;

   function Is_Float (X : Vertex) return Boolean is
   begin
      return X.Tip = Float_Tip;
   end;

   function Is_Process_Vertex (X : Vertex) return Boolean is
   begin
      return X.Tip = Process_Vertex_Tip;
   end;

   function Is_String (X : Vertex) return Boolean is
   begin
      return X.Tip in Tiny_String_Tip or X.Tip in Short_String_Tip;
   end;

   function Is_Valueless (X : Vertex) return Boolean is
   begin
      return X.Tip = Serial_Number_Tip;
   end;

   function LT_For_String_Vertices
     (Left, Right : Vertex) return Boolean
   is
      NL : Standard.Natural := Length (Left);
      NR : Standard.Natural := Length (Right);
      L : Standard.Positive := 1;
      R : Standard.Positive := 1;
      D : Standard.Natural := Slice_Size_For_Comparison;
      L1, R1 : Standard.Natural;
   begin
      while L <= NL and R <= NR loop
         L1 := Standard.Natural'Min (L + D - 1, NL);
         R1 := Standard.Natural'Min (R + D - 1, NR);
         if Slice (Left, L, L1) < Slice (Right, R, R1) then
            return True;
         end if;
         L := L1 + 1;
         R := R1 + 1;
      end loop;
--      return False; - hmmm...
      return NL < NR;
   end;

   --  function LT_For_String_Vertices1 -- faster?
   --    (Left, Right : Vertex) return Boolean
   --  is
   --     NL : Standard.Natural := Length (Left);
   --     NR : Standard.Natural := Length (Right);
   --     L : Standard.Positive := 1;
   --     R : Standard.Positive := 1;
   --     D : Standard.Natural := Slice_Size_For_Comparison;
   --     L1, R1 : Standard.Natural;
   --  begin
   --     if Left.Tip in Tiny_String_Tip
   --     and Right.Tip in Tiny_String_Tip then
   --        return Left.Cue < Right.Cue;
   --           -- this works because tiny strings are padded with NUL
   --     elsif Left.Tip in Tiny_String_Tip then
   --        return String'(Value (Left)) < Slice (Right, 1 .. Left.Tip);
   --     elsif Right.Tip in Tiny_String_Tip then
   --        return Slice (Left, 1 .. Right.Tip) < String'(Value (Right));
   --     else
   --     while L <= NL and R <= NR loop
   --        L1 := Standard.Natural'Min (L + D - 1, NL);
   --        R1 := Standard.Natural'Min (R + D - 1, NR);
   --        if Slice (Left, L, L1) < Slice (Right, R, R1) then
   --           return True;
   --        end if;
   --        L := L1 + 1;
   --        R := R1 + 1;
   --     end loop;
   --     return False;
   --  end;

--   function LT_For_String_Vertices0
--     (Left, Right : Vertex) return Boolean is
--   begin
--      return String'(Value (Left)) < String'(Value (Right));
--      if Left.Tip in Tiny_String_Tip and Right.Tip in Tiny_String_Tip then
--         return String'(Value (Left)) < String'(Value (Right));
--      elsif Left.Tip /= Right.Tip then
--         return Left.Tip < Right.Tip;
--      else
--         return Left.Cue < Right.Cue;
--      end if;
--   end;

   function "<" (Left, Right : Vertex) return Boolean is
   begin
      if Left = Right then
         return False;
      elsif Left = Front_Vertex or Right = Back_Vertex then
         return True;
      elsif Left = Back_Vertex or Right = Front_Vertex then
         return False;
      elsif Is_String (Left) and Is_String (Right) then
         return LT_For_String_Vertices (Left, Right);
      elsif Left.Tip /= Right.Tip then
         return Left.Tip < Right.Tip;
      elsif Is_Float (Left) then
         return To_Float (Left.Cue) < To_Float (Right.Cue);
      elsif Is_Integer (Left) then
         return Integer (Left.Cue) < Integer (Right.Cue);
      else
         return Left.Cue < Right.Cue;
      end if;
   end;

   -------------
   -- EXECUTE --
   -------------

   function First_Target (X : Vertex) return Vertex is
      Result : Vertex;
      Done : exception;
      procedure Return_First (X : Vertex) is
      begin
         Result := X;
         raise Done;
      end;
   begin
      For_Each_Target (X, Return_First'Unrestricted_Access);
   exception
      when Done => return Result;
   end;

   function Attribute_Value
     (Subject, Attribute_Name : Vertex) return Vertex
   is
      Result : Vertex;
      Done : exception;
      procedure Return_First (X : Vertex) is
      begin
         Result := First_Target (X);
         raise Done;
      end;
   begin
      For_Each_Common_Target
        (Subject, Attribute_Name, Return_First'Unrestricted_Access);
   exception
      when Done => return Result;
   end;

   procedure Execute_Process_Vertex (Proc : Process_Vertex; Call : Vertex) is
   begin
      Proc (Call);
   end;

   procedure Execute (X : Vertex) is
   begin
      if Connected (X, Ada_Proc) then
         Execute_Process_Vertex
           (Proc => Value (Attribute_Value (X, Procaddr)),
            Call => X);
      else
         raise Program_Error;
      end if;
   exception
      when others => null; -- ERROR HANDLING MUST BE IMPROVED
   end;
      -- WELL, WHOLE PROCEDURE MUST BE IMPROVED
      -- PENDING DESIGN OF NON-ADA_PROC PROCEDURES

--   procedure Execute_That_Does_Not_Compile (X : Vertex) is
--     begin
--      if Connected (X, Ada_Proc) then
--         Process_Vertex'(Value (Attribute_Value (X, Procaddr)))
--                                                   SAYS MISSING ";" HERE
--            (Args => Attribute_Value (X, Args)));
--      else
--         raise Program_Error;
--      end if;
--   exception
--      when others => null; -- ERROR HANDLING MUST BE IMPROVED
--   end;

   -------------
   -- CONNECT --
   -------------

   procedure Connect (Source, Target : Vertex) is
      use Link_Sets;
      Dont_Need : Cursor_Type;
      Dont_Care : Boolean;
use ada.text_io;
   begin
--put("CONNECT "&img(source)&"->"&img(target));
--if source=target then put(" LOOP!!!!!!!!!!!!!!!!!!!!!"); end if;
--new_line;
      if Source = Exec_Now then
         Execute (Target);
      else
         Insert
           (Set => Links,
            New_Item => (Source, Target),
            Cursor => Dont_Need,
            Success => Dont_Care);
         Insert
           (Set => Inv_Links,
            New_Item => (Target, Source),
            Cursor => Dont_Need,
            Success => Dont_Care);
      end if;
   end;

   ----------------
   -- DISCONNECT --
   ----------------

   procedure Disconnect (Source, Target : Vertex) is
      use Link_Sets;
   begin
      Delete (Links, (Source, Target));
      Delete (Inv_Links, (Target, Source));
   end;

   procedure Disconnect_From_Targets (Source : Vertex) is
      use Link_Sets;
      I : Cursor_Type := Lower_Bound (Links, (Source, Front_Vertex));
      L : Link_Type;
   begin
      while I /= Back (Links) and I /= Null_Cursor loop
         L := Element (I);
         exit when L (1) > Source;
         Delete (Links, I);
         Delete (Inv_Links, (L (2), L (1)));
      end loop;
   end;

   procedure Disconnect_From_Sources (Target : Vertex) is
      use Link_Sets;
      I : Cursor_Type := Lower_Bound (Inv_Links, (Target, Front_Vertex));
      L : Link_Type;
   begin
      while I /= Back (Inv_Links) and I /= Null_Cursor loop
         L := Element (I);
         exit when L (1) > Target;
         Delete (Inv_Links, I);
         Delete (Links, (L (2), L (1)));
      end loop;
   end;

   procedure Disconnect (X : Vertex) is
   begin
      Disconnect_From_Targets (X);
      Disconnect_From_Sources (X);
   end;

   procedure Disconnect_Vertex (X : Vertex) is
   begin
      Disconnect_From_Targets (X);
      Disconnect_From_Sources (X);
   end;
   --  quick hack for GNAT 2022 to compile below

   procedure Disconnect_Targets (Source : Vertex) is
   begin
      For_Each_Target (Source, Disconnect_Vertex'Unrestricted_Access);
      --  was in 2005: For_Each_Target (Source, Disconnect'Access);
      --  to compile in GNAT 2022, changed to Unrestricted_Access,
      --  but GNAT still got confused about it, expecting Disconnect/2, and
      --  even expecting a function!
   end;

   ----------------------------
   -- FOR EACH LINK IN RANGE --
   ----------------------------

   generic
      with procedure Process (Link : Link_Type) is <>;
   procedure For_Each_Link_In_Range
     (Set : Link_Sets.Set_Type; From, To : Link_Type);

   procedure For_Each_Link_In_Range
     (Set : Link_Sets.Set_Type; From, To : Link_Type)
   is
      use Link_Sets;
      I : Cursor_Type := Lower_Bound (Set, From);
      E : Link_Type;
   begin
      I := Lower_Bound (Set, From);
      while I /= Back (Set) loop
         E := Element (I);
         exit when E > To;
         Process (E);
         Increment (I);
      end loop;
   end;

   ----------------------------
   -- FOR EACH TARGET/SOURCE --
   ----------------------------

   generic
      Link_Set : Link_Sets.Set_Type;
      with procedure Process (Vertex_2 : Vertex) is <>;
   procedure For_Each_Vertex_2 (Vertex_1 : Vertex);

   procedure For_Each_Vertex_2 (Vertex_1 : Vertex) is
      procedure Process (Link : Link_Type) is
      begin
         Process (Link (2));
      end;
      procedure Process_Links is new For_Each_Link_In_Range;
   begin
      Process_Links
        (Set => Link_Set,
         From => (Vertex_1, Front_Vertex),
         To => (Vertex_1, Back_Vertex));
   end;

   generic
      with procedure Process (Target : Vertex) is <>;
   procedure Generic_For_Each_Target (Source : Vertex);

   procedure Generic_For_Each_Target (Source : Vertex) is
      procedure Process_Target is new For_Each_Vertex_2 (Links);
   begin
      Process_Target (Source);
   end;   

   generic
      with procedure Process (Source : Vertex) is <>;
   procedure Generic_For_Each_Source (Target : Vertex);

   procedure Generic_For_Each_Source (Target : Vertex) is
      procedure Process_Source is new For_Each_Vertex_2 (Inv_Links);
   begin
      Process_Source (Target);
   end;

   ---------------
   -- RECONNECT --
   ---------------

   procedure Reconnect (Source, Target, New_Target : Vertex) is
   begin
      Disconnect (Source, Target);
      Connect (Source, New_Target);
   end;

   procedure Inv_Reconnect (Target, Source, New_Source : Vertex) is
   begin
      Disconnect (Source, Target);
      Connect (New_Source, Target);
   end;

   ---------------
   -- CONNECTED --
   ---------------

   function Connected (Source, Target : Vertex) return Boolean is
   begin
      return Link_Sets.Is_In ((Source, Target), Links);
   end;

   function Connected (Source : Vertex) return Boolean is
      use Link_Sets;
      X : Cursor_Type := Lower_Bound (Links, (Source, Front_Vertex));
   begin
      return (X /= Null_Cursor and X /= Back (Links)) and then Element (X) (1) = Source;
      --return X /= Null_Cursor and then Element (X) (1) = Source;
   end;

   function Inv_Connected (Target : Vertex) return Boolean is
      use Link_Sets;
      X : Cursor_Type := Lower_Bound (Inv_Links, (Target, Front_Vertex));
   begin
      return X /= Null_Cursor and then Element (X) (1) = Target;
   end;

   ---------------------------
   -- GENERIC FOR EACH LINK --
   ---------------------------

   generic
      with procedure Process (Source, Target : Vertex) is <>;
   procedure Generic_For_Each_Link;

   procedure Generic_For_Each_Link is
      use Link_Sets;
      procedure Process (Link : Cursor_Type) is
         E : Link_Type := Element (Link);
      begin
         Process (E (1), E (2));
      end;
      procedure Traverse is new Generic_Iteration;
   begin
      Traverse (Links);
   end;

   --------------------------
   -- NON GENERIC FOR EACH --
   --------------------------

   procedure For_Each_Target (Source : Vertex; Process: Process_Vertex) is
      use Link_Sets;
      I : Cursor_Type := Lower_Bound (Links, (Source, Front_Vertex));
      L : Link_Type;
   begin
      while I /= Null_Cursor and I /= Back (Links) loop
         L := Element (I);
         exit when L (1) > Source;
         Process (L (2));
         Increment (I);
      end loop;
   end;

   procedure For_Each_Source (Target : Vertex; Process: Process_Vertex) is
      use Link_Sets;
      I : Cursor_Type := Lower_Bound (Inv_Links, (Target, Front_Vertex));
      L : Link_Type;
   begin
      while I /= Null_Cursor and I /= Back (Inv_Links) loop
         L := Element (I);
         exit when L (1) > Target;
         Process (L (2));
         Increment (I);
      end loop;
   end;

   procedure For_Each_Link (Process: Process_Link) is
      procedure Actual_For_Each_Link is
         new Generic_For_Each_Link (Process.all);
   begin
      Actual_For_Each_Link;
   end;

   procedure For_Each_Common_Target
     (Source_1, Source_2 : Vertex; Process : Process_Vertex)
   is
      use Link_Sets;
      Next1 : Cursor_Type := Lower_Bound (Links, (Source_1, Front_Vertex));
      Next2 : Cursor_Type := Lower_Bound (Links, (Source_2, Front_Vertex));
      Back1 : Cursor_Type := Lower_Bound (Links, (Source_1, Back_Vertex));
      Back2 : Cursor_Type := Lower_Bound (Links, (Source_2, Back_Vertex));
      Link1, Link2 : Link_Type;
      Tgt1, Tgt2 : Vertex;
   begin
      while Next1 /= Null_Cursor
      and Next1 /= Back1
      and Next2 /= Null_Cursor
      and Next2 /= Back2 loop
         Link1 := Element (Next1);
         Link2 := Element (Next2);
         exit when Link1 (1) /= Source_1 or Link2 (1) /= Source_2;
         Tgt1 := Link1 (2);
         Tgt2 := Link2 (2);
         if Tgt1 = Tgt2 then
            Process (Tgt1);
            Next1 := Succ (Next1);
            Next2 := Succ (Next2);
         elsif Tgt1 < Tgt2 then
            Next1 := Lower_Bound (Links, (Source_1, Tgt2));
         elsif Tgt2 < Tgt1 then
            Next2 := Lower_Bound (Links, (Source_2, Tgt1));
         end if;
      end loop;
   end;

   --------------------
   -- TEXT FUNCTIONS --
   --------------------

   function Img (X : Natural_16) return String is
      S : String := Natural_16'Image (X);
   begin
      return S (2 .. S'Last);
   end;

   function Img (X : Modular_64) return String is
      S : String := Modular_64'Image (X);
   begin
      return S (2 .. S'Last);
   end;

   function Img (X : Vertex; Max_Length : Img_String_Length := Default_Max_Length) return String is
      use Work;
   begin
      if Is_Integer (X) then return Integer'Image (Value (X));
      elsif Is_Float (X) then return Float'Image (Value (X));
      elsif Is_String (X) and then Length (X) <= Max_Length then
         return '"' & Value (X) & '"';
      else return '#' & Img (X.Tip) & ''' & Img (X.Cue);
      end if;
   end;

   function Val (S : String) return Vertex is
      use Ada.Strings.Fixed;
      use Work;
   begin
      if S (S'First) = '#' then
         declare
            Tick : Standard.Natural := Index (S, "'");
         begin
            if Tick = 0 then return
              (Tip => Serial_Number_Tip,
               Cue => Modular_64'Value (S (S'First + 1 .. S'Last)));
            else return
              (Tip => Natural_16'Value (S (S'First + 1 .. Tick - 1)),
               Cue => Modular_64'Value (S (Tick + 1 .. S'Last)));
            end if;
         end;
      elsif S (S'First) = '"' and then S (S'Last) = '"' then
         return To_Vertex (Unescape (S (S'First + 1 .. S'Last - 1)));
      else
         begin
            return To_Vertex (Integer'Value (S));
         exception
            when Constraint_Error =>
               begin
                  return To_Vertex (Float'Value (S));
               exception
                  when Constraint_Error => return To_Vertex (S);
               end;
         end;
      end if;
   end;

   function Escape (S : String) return String is
      use Ada.Strings.Fixed;
      Special_Chars : String := "\|""";
      U : Unbounded_String;
   begin
      for I in S'Range loop
         if Index (Special_Chars, S (I .. I)) > 0 then
            Append (U, '\');
         end if;
         Append (U, S (I));
      end loop;
      return To_String (U);
   end;

   procedure Inc (X : in out Standard.Integer) is begin X := X + 1; end;

   function Unescape (S : String) return String is
      U : Unbounded_String;
      I : Standard.Integer := S'First;
   begin
      while I <= S'Last loop
         if S (I) = '\' then Inc (I); end if;
         Append (U, S (I));
         Inc (I);
      end loop;
      return To_String (U);
   end;

end;

-- NOTES

-- The non-standard (GNAT-specific) Unrestricted_Access attribute
-- is used to simplify enourmously the callback idiom.
-- Ada 2005 will make this very simple, see AI-254.
