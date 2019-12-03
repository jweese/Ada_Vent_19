with Ada.Integer_Text_IO;
with Intcode;
with Memory;

procedure Day_02 is
   Mem: constant Memory.Block := Memory.Read_Comma_Separated;

   use type Memory.Value;
   subtype Input is Memory.Value range 0..99;
begin
   for Noun in Input'Range loop
      for Verb in Input'Range loop
         declare
            M: Intcode.Machine := Intcode.New_Machine(Mem);
         begin
            M.Poke(Addr => 16#1#, Value => Noun);
            M.Poke(Addr => 16#2#, Value => Verb);
            M.Run;
            if M.Peek(0) = 1969_07_20 then
               Ada.Integer_Text_IO.Put(Integer(Noun));
               Ada.Integer_Text_IO.Put(Integer(Verb));
               return;
            end if;
         end;
      end loop;
   end loop;
end Day_02;
