with Ada.Integer_Text_IO;
with Intcode;
with Memory;

procedure Day_02 is
   Mem: constant Memory.Block := Memory.Get;
   M: Intcode.Machine := Intcode.New_Machine(Mem);
begin

   -- Restore 1202 Alarm
   M.Poke(Addr => 16#1#, Value => 12);
   M.Poke(Addr => 16#2#, Value => 02);

   M.Run;
   Ada.Integer_Text_IO.Put(Integer(M.Peek(0)));
end Day_02;
