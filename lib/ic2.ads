with Memory;

package IC2 is
   type Maybe_Integer(Present: Boolean := False) is
   record
      case Present is
         when False => null;
         when True => Value: Integer;
      end case;
   end record;

   type Port_Status is private;
   protected type Port is
      entry Put(I: in Integer);
      entry Get(X: out Maybe_Integer);
      entry Close;
   private
      Status: Port_Status;
      Value: Integer;
   end Port;

   type M(Hi_Mem: Memory.Address) is record
      Mem: Memory.Block(1 .. Hi_Mem);
      Input, Output: Port;
   end record;

   task type T(AM: not null access M);
private
   type Port_Status is (Empty, Full, Closed);
end IC2;
