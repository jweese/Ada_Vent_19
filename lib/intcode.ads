with Memory;

package Intcode is
   task type Machine(Hi_Mem: Memory.Address) is
      entry Load_And_Exec(From: in Memory.Block);
      entry Save(To: out Memory.Block);
      entry Put(I: in Integer);
      entry Get(I: out Integer);
      entry Shutdown;
   end Machine;
end Intcode;
