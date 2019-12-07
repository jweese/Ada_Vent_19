with Memory;

package Intcode is
   task type Machine(Hi_Mem: Memory.Address) is
      entry Load(From: in Memory.Block);
      entry Exec;
      entry Save(To: out Memory.Block);
      entry Put(I: in Integer);
      entry Get(I: out Integer);
      entry Shutdown;
   end Machine;
end Intcode;
