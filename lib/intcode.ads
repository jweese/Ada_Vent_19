with Memory;

package Intcode is
   type Output is record
      Updated: Boolean;
      Value: Integer;
   end record;

   task type Machine(Hi_Mem: Memory.Address) is
      entry Load_And_Exec(From: in Memory.Block);
      entry Save(To: out Memory.Block);
      entry Put(I: in Integer);
      entry Get(I: in out Output);
   end Machine;
end Intcode;
