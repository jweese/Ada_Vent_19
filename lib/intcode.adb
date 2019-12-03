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

   type Op_Code is (
      Add,
      Mul,
      Halt
   );

   function Get_Op_Code(V: Memory.Value) return Op_Code is
   begin
      case V is
            when 1 => return Add;
            when 2 => return Mul;
            when 99 => return Halt;
            when others => raise Constraint_Error;
      end case;
   end Get_Op_Code;

   procedure Run(M: in out Machine) is
   begin
      null;
   end;
end Intcode;
