-- PROCEDURE MNESON.TESTS.RUN (BODY ONLY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

--with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
--with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Mneson;
with Mneson.Calculus;
with Mneson.Monitors;
with Mneson.Structures;
with System;

procedure Mneson.Tests.Run is

   package Calculus is new Mneson.Calculus (Base.Work);
   --package Structures is new Mneson.Structures (Base.Work);

   use Base;
   use Calculus;
   --use Structures;

   A, B, C : Vertex;
   
begin

   New_Test ("Representation");
   Increase_Level;
   begin
      New_Test ("Size");
      if Process_Vertex'Size = Process_Vertex_Size then Pass;
      else
         Put_Line (" real size =" & Process_Vertex'Size'Image);
         Fail;
      end if;
      New_Test ("Alignment");
      if Modular_64'Alignment = Integer'Alignment then Pass;
      else Fail;
      end if;
      New_Test ("Position");
      if A.Tip'Position = 0
      and A.Cue'Position = 16 / System.Storage_Unit
      and A'Size = 80
      then Pass;
      else Fail;
      end if;
   exception
      when others => Fatal;
   end;
   Decrease_Level;
   
   New_Test ("Create");
   begin
      Create ("test");
      Pass;
   exception
      when others => Fatal;
   end;
   
   New_Test ("To_Vertex");
   begin
      A := To_Vertex ("A");
      B := To_Vertex ("B2345678");
      C := To_Vertex ("C23456789");
      Pass;
   exception
      when others => Fail;
   end;
   
   New_Test ("Connect");
   begin
      Connect (A, B);
      Connect (A, C);
      Connect (B, C);
      Pass;
   exception
      when others => Fail;
   end;
   
   New_Test ("Connected");
   begin
      if Connected (A, B) and Connected (A, C) then Pass;
      else Fail;
      end if;
   exception
      when others => Fail;
   end;

   New_Test ("Disconnect");
   begin
      Disconnect (A, B);
      Disconnect (A, C);
      Disconnect (B, C);
      Pass;
   exception
      when others => Fail;
   end;
   
   New_Test ("Disconnected");
   begin
      if Connected (A, B) or Connected (A, C) then Fail;
      else Pass;
      end if;
   exception
      when others => Fail;
   end;

   New_Test ("Connect again");
   begin
      Connect (A, B);
      Connect (A, C);
      Connect (B, C);
      Pass;
   exception
      when others => Fail;
   end;
   
   New_Test ("For_Each_Target");
   declare
      Check_Failed : exception;
      procedure Check (X : Vertex) is
      begin
         if X = B or X = C then null;
         else raise Check_Failed;
         end if;
      end;
   begin
      For_Each_Target (A, Check'Unrestricted_Access);
      Pass;
   exception
      when others => Fail;
   end;
   
   New_Test ("For_Each_Common_Target");
   declare
      Check_Failed : exception;
      procedure Check (X : Vertex) is
      begin
         if X = C then null;
         else raise Check_Failed;
         end if;
      end;
   begin
      For_Each_Common_Target (A, B, Check'Unrestricted_Access);
      Pass;
   exception
      when others => Fail;
   end;
   
   New_Test ("Exec_Now");
   declare
      Call, Addr : Vertex;
      Called : Boolean := False;
      procedure Proc (Call : Vertex) is
      begin
         Called := True;
      end;
   begin
      Call := New_Vertex;
      Addr := New_Vertex;
      Connect (Call, Ada_Proc);
      Connect (Call, Addr);
      Connect (Procaddr, Addr);
      Connect (Addr, To_Vertex (Proc'Unrestricted_Access));
      Connect (Exec_Now, Call);
      if Called then Pass; else Fail; end if;
   exception
      when others => Fail;
   end;

   New_Test ("Save");
   begin
      Save;
      Pass;
   exception
      when others => Fail;
   end;

   New_Test ("Close");
   begin
      Close;
      Pass;
   exception
      when others => Fail;
   end;

   New_Test ("Open, verify all is there; Save_As, Open, verify");
   Increase_Level;
   declare
      procedure Verify is
      begin
         if Connected (A, B)
         and Connected (A, C)
         and Connected (B, C)
         then Pass;
         else Fail;
         end if;
      end;
   begin
      --Set_Monitor (Mneson.Monitors.Standard_Error_Monitor'Access);
      New_Test ("Open");
      Open ("test");
      Pass;
      New_Test ("verify all is there");
      Verify;
      New_Test ("Save_As");
      Save_As ("test2");
      Pass;
      New_Test ("verify");
      Verify;
      New_Test ("Close, Open, verify");
      Close;
      Open ("test2");
      Verify;
   exception
      when others => Fail;
   end;
   Decrease_Level;

   New_Test ("Text");
   Increase_Level;
   begin
      New_Test ("Val (Img (X)) = X");
      if Val (Img (A)) = A then Pass; else Fail; end if;
      New_Test ("Val");
      if Val ("xxx") = To_Vertex ("xxx")
      and Val ("xxx xxx") = To_Vertex ("xxx xxx")
         -- passes, but is slightly anomalous
      and Val ("1") = To_Vertex (1)
      and Val ("1.1") = To_Vertex (1.1)
      then Pass;
      else Fail;
      end if;
      New_Test ("Escape");
      if Escape ("xxx\xxx|xxx""") = "xxx\\xxx\|xxx\""" then Pass;
      else Fail;
      end if;
      New_Test ("Unescape");
      if "xxx\xxx|xxx""" = Unescape ("xxx\\xxx\|xxx\""") then Pass;
      else Fail;
      end if;
   exception
      when others => Fail;
   end;
   Decrease_Level;

   New_Test ("Calculus");
   Increase_Level;
   begin
      New_Test ("Extract (Singleton (X)) = X");
      if Extract (Singleton (A)) = A then Pass;
      else Fail;
      end if;
      New_Test ("To_Selection (Set)");
      if Extract (To_Selection (A), 1) = B
      and Extract (To_Selection (A), 2) = C
      then Pass;
      else Fail;
      end if;
      New_Test ("Targets");
      -- A --> B --> C exists
      if Extract (Targets (To_Selection (A)), 1) = C then Pass;
      else Fail;
      end if;
      New_Test ("Sources");
      -- A --> C exists
      if Extract (Sources (To_Selection (A)), 1) = A then Pass;
      else Fail;
      end if;
      New_Test ("Intersect");
      if Extract (Intersect (To_Selection (A), Singleton (B))) = B then Pass;
      else Fail;
      end if;
      New_Test ("Subtract");
      if Extract (To_Selection (A) - Singleton (B)) = C then Pass;
      else Fail;
      end if;
      New_Test ("Checking Calculus.Marker connected (has elements)");
      if Connected (Calculus.Marker) then Pass; else Fail; end if;
      New_Test("Clear");
      Calculus.Clear;
      Pass;
      New_Test ("Checking Calculus.Marker is NOT connected");
      if not Connected (Calculus.Marker) then Pass; else Fail; end if;
   exception
      when others => Fail;
   end;
   Decrease_Level;

   New_Test ("Mark/unmark");
   Increase_Level;
   declare
      M, X, Y, Secure : Vertex;
   begin
      New_Test ("Mark");
      M := New_Vertex;
      Mark (M);
      if Connected (Markers, M) then Pass; else Fail; end if;
      X := New_Vertex;
      Y := New_Vertex;
      New_Test ("Unmark "& img(M));
      Unmark (M);
      if not Connected (Markers, M) then Pass; else Fail; end if;      
      New_Test ("Testing marked");
      if Connected (M, X) and Connected (M, Y) then Pass; else Fail; end if;
      New_Test ("Disconnect marked from mark (preparation)");
      Secure := New_Vertex;
      Connect (Secure, M);
      Pass;
      New_Test ("execution");
      declare
         procedure Disconnect_From_M (Z : Vertex) is begin Disconnect (M, Z); end;
      begin
         --For_Each_Target (M, Disconnect_From_M'Unrestricted_Access);
         Disconnect_Targets (M);
      end;
      Pass;
      New_Test ("testing");
      if Connected (M, X) or Connected (M, Y) then Fail; else Pass; end if;
      New_Test ("Disconnecting M from Secure");
      Disconnect (Secure, M);
      Pass;
      New_Test ("testing M connected " & img (M));
     declare
      B : Boolean;
      use Ada.TExt_IO;
     begin
      begin
       B := Connected (M);
      exception
       when others => Put_Line ("Connected has raised an exception!!!");
      end;
     end;
      if Connected (M) then
         declare
            use Ada.Text_IO;
            procedure Put_Vertex (X : Vertex) is begin Put (" " & Img (X)); end;
         begin
            Put ("M targets:");
            --For_Each_Target (M, Put_Vertex'Unrestricted_Access);
            New_Line;
            Put ("M sources:");
            --For_Each_Source (M, Put_Vertex'Unrestricted_Access);
            New_Line;
            Fail;
         end;
      else Pass; end if;
      New_Test ("End of Mark/unmark tests");
      Pass;
   exception
      when others => Fail;
   end;
   Decrease_Level;

   New_Test ("Close (again)");
   begin
      Close;
      Pass;
   exception
      when others => Fail;
   end;

   New_Test ("Testing for the strange Mark bug (?) shown by the SynNet application");
   Increase_Level;
   
   New_Test ("Creating network");
   Create ("mark_test");
   Pass;
   
   declare
      M : Vertex := New_Vertex;
      S : Vertex := New_Vertex;
      X : Vertex;
   begin      
      New_Test ("Marking");
      Mark (M);
      for I in 1 .. 100_000 loop
         X := New_Vertex;
         Connect (S, X);
      end loop;
      Pass;
      
      New_Test ("Unmark");
      Unmark (M);
      Pass;
      
      New_Test ("Deleting Marked");
      declare
         procedure Print_Vertex (X : Vertex) is
         begin
            ada.text_io.put (img (X));
         end;
      begin
         --For_Each_Target (M, Print_Vertex'Unrestricted_Access);
         null;
      end;
      
      declare
         Ok : exception;
         No_More : exception;
         Return_Value : Vertex;
         function Extract_First_Target (Source : Vertex) return Vertex is
            procedure Return_It (X : Vertex) is
            begin
               Return_Value := X;
               raise Ok;
            end;
         begin
            For_Each_Target (Source, Return_It'Unrestricted_Access);
            raise No_More;
         exception
            when Ok => return Return_Value;
         end;
      begin
         loop
            Disconnect (Extract_First_Target (M));
         end loop;
      exception
         when No_More => null;
      end;
      
      Pass;
      
      New_Test ("Closing");
      Close;
      Pass;
   exception
      when others => Fail;
   end;
   Decrease_Level;
   
   Report;

exception

   when Done => Report;
   
end;
