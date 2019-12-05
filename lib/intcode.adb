with Intcode.Op;

package body Intcode is
   function New_Machine(Mem: Memory.Block) return Machine is
      (
         High_Mem => Memory.Address'Pred(Mem'Length),
         PC => 0,
         Mem => Mem
      );

   function Peek(M: Machine; Addr: Memory.Address) return Memory.Value is
      (M.Mem(Addr));

   procedure Poke(M: in out Machine; Addr: Memory.Address; Value: Memory.Value)
   is
   begin
      M.Mem(Addr) := Value;
   end Poke;

   procedure Run(M: in out Machine) is
      use type Intcode.Op.Code;
   begin
      loop
         declare
            Curr_Op: constant Intcode.Op.Schema :=
               Intcode.Op.Decode(M.Mem(M.PC));
         begin
            exit when Curr_Op.Instruction = Op.Halt;
            Intcode.Op.Exec(Curr_Op, M);
         end;
      end loop;
   end Run;
end Intcode;
