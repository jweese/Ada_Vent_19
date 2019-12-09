with Memory;

package Intcode is
   type Output(Present: Boolean := False) is record
      case Present is
         when False => null;
         when True => Value: Integer;
      end case;
   end record;

   task type Machine(Hi_Mem: Memory.Address) is
      entry Load_And_Exec(From: in Memory.Block);
      entry Save(To: out Memory.Block);
      entry Put(I: in Integer);
      entry Get(I: in out Output);
   end Machine;
end Intcode;
