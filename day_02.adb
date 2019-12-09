with Ada.Integer_Text_IO;
with IC2;
with Memory;

procedure Day_02 is
   Mem: constant Memory.Block := Memory.Read_Comma_Separated;
   M: aliased IC2.Machine(Hi_Mem => Mem'Last);

   use type Memory.Value;
   subtype Input is Memory.Value range 0..99;
begin
   for Noun in Input'Range loop
      for Verb in Input'Range loop
         M.Mem := Mem;
         M.Mem(16#1#) := Noun;
         M.Mem(16#2#) := Verb;
         declare
            E: IC2.Executor(M'Access);
         begin
            null; -- wait for E to terminate
         end;
         if M.Mem(16#0#) = 1969_07_20 then
            Ada.Integer_Text_IO.Put(Integer(Noun));
            Ada.Integer_Text_IO.Put(Integer(Verb));
            return;
         end if;
      end loop;
   end loop;
end Day_02;
