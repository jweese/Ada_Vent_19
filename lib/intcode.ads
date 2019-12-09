with Memory;

package Intcode is
   type Maybe_Memory_Value(Present: Boolean := False) is
   record
      case Present is
         when False => null;
         when True => Value: Memory.Value;
      end case;
   end record;

   type Port_Status is (Empty, Full, Closed);
   protected type Port is
      entry Put(I: in Memory.Value);
      entry Get(X: out Maybe_Memory_Value);
      entry Close;
   private
      Status: Port_Status := Empty;
      Value: Memory.Value := 0;
   end Port;

   type Machine(Hi_Mem: Memory.Address) is record
      Mem: Memory.Block(0 .. Hi_Mem);
      Input, Output: access Port;
   end record;

   task type Executor(AM: not null access Machine);
end Intcode;
