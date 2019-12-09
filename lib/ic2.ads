with Memory;

package IC2 is
   type Maybe_Integer(Present: Boolean := False) is
   record
      case Present is
         when False => null;
         when True => Value: Integer;
      end case;
   end record;

   type Port_Status is (Empty, Full, Closed);
   protected type Port is
      entry Put(I: in Integer);
      entry Get(X: out Maybe_Integer);
      entry Close;
   private
      Status: Port_Status := Empty;
      Value: Integer := 0;
   end Port;

   type Machine(Hi_Mem: Memory.Address) is record
      Mem: Memory.Block(0 .. Hi_Mem);
      Input, Output: access Port;
   end record;

   task type Executor(AM: not null access Machine);
end IC2;
