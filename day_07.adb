with Ada.Integer_Text_IO;
with Intcode;
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
      I: Intcode.Output;
      Amps: array (Amp_Range) of Intcode.Machine(Hi_Mem => Mem'Last);
   begin
      -- Startup
      for J in Input'Range loop
         Amps(J).Load_And_Exec(From => Mem);
         Amps(J).Put(5 + Integer(Input(J)));
      end loop;

      loop
         for A of Amps loop
            A.Put(Current);
            A.Get(I);
            case I.Present is
               when True => Current := I.Value;
               when False => return Current;
            end case;
         end loop;
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
