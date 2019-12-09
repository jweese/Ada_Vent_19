with Ada.Integer_Text_IO;
with Intcode;
with Memory;

procedure Day_07 is
   use type Memory.Value;

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
      Current: Memory.Value := 0;
      I: Intcode.Maybe_Memory_Value;
      Amps: array (Amp_Range) of aliased Intcode.Machine(Hi_Mem => Mem'Last) :=
         (others => (Hi_Mem => Mem'Last,
                     Mem => Mem,
                     Input => new Intcode.Port,
                     Output => new Intcode.Port));
      Exec: array (Amp_Range) of access Intcode.Executor;
   begin
      -- Startup
      for J in Input'Range loop
         if J /= Input'Last then Amps(J).Output := Amps(J + 1).Input; end if;
         Amps(J).Input.Put(5 + Memory.Value(Input(J)));
         Exec(J) := new Intcode.Executor(AM => Amps(J)'Access);
      end loop;

      loop
         Amps(Input'First).Input.Put(Current);
         Amps(Input'Last).Output.Get(I);
         if not I.Present then return Natural(Current); end if;
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
