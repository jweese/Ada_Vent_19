with Ada.Integer_Text_IO;
with IC2;
with Memory;

procedure Day_07 is
   Mem: constant Memory.Block := Memory.Read_Comma_Separated;

   type Amp_Range is range 1 .. 5;
   type Phase is mod 5;
   type Phases is array (Amp_Range) of Phase;

   procedure Increment(X: in out Phases) is
   begin
      for I in X'Range loop
         X(I) := X(I) + 1;
         exit when X(I) /= 0;
      end loop;
   end Increment;

   function Valid(X: Phases) return Boolean is
   begin
      for P in Phase'Range loop
         if not (for some I of X => I = P) then return False; end if;
      end loop;
      return True;
   end Valid; 

   function Amplify(Input: Phases) return Natural is
      Current: Integer := 0;
      I: IC2.Maybe_Integer;
      Amps: array (Amp_Range) of aliased IC2.Machine(Hi_Mem => Mem'Last) :=
         (others => (Hi_Mem => Mem'Last,
                     Mem => Mem,
                     Input => new IC2.Port,
                     Output => new IC2.Port));
      Exec: array (Amp_Range) of access IC2.Executor;
   begin
      -- Startup
      for J in Input'Range loop
         if J /= Input'Last then Amps(J).Output := Amps(J + 1).Input; end if;
         Amps(J).Input.Put(5 + Integer(Input(J)));
         Exec(J) := new IC2.Executor(AM => Amps(J)'Access);
      end loop;

      loop
         Amps(Input'First).Input.Put(Current);
         Amps(Input'Last).Output.Get(I);
         if not I.Present then return Current; end if;
         Current := I.Value;
      end loop;
   end Amplify;

   Max_Output: Natural := 0;
   Curr: Phases := (others => 0);
begin
   loop
      if Valid(Curr) then
         Max_Output := Natural'Max(Max_Output, Amplify(Curr));
      end if;
      Increment(Curr);
      exit when (for all I of Curr => I = 0);
   end loop;
   Ada.Integer_Text_IO.Put(Max_Output);
end Day_07;
