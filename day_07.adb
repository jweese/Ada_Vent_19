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
      I: Natural := 0;
   begin
      for P of Input loop
         declare
            Amp: Intcode.Machine(Hi_Mem => Mem'Last);
         begin
            Amp.Load(From => Mem);
            Amp.Exec;
            Amp.Put(Integer(P));
            Amp.Put(I);
            Amp.Get(I);
         end;
      end loop;
      return I;
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
