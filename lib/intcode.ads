with Memory;

package Intcode is
   type Machine(High_Mem: Memory.Address) is tagged private;

   function New_Machine(Mem: Memory.Block) return Machine;

   function Peek(M: Machine; Addr: Memory.Address) return Memory.Value;

   procedure Poke(M: in out Machine; Addr: Memory.Address; Value: Memory.Value);

   procedure Run(M: in out Machine);

private
   type Machine(High_Mem: Memory.Address) is tagged record
      PC: Memory.Address;
      Mem: Memory.Block(0 .. High_Mem);
   end record;
end Intcode;
