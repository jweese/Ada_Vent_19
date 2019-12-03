with Intcode.Binary_Op;

package body Intcode is
   use type Memory.Address;
   use type Memory.Value;

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
            when others => raise Constraint_Error with "op code" & V'Image;
      end case;
   end Get_Op_Code;

   type Op is access procedure(M: in out Machine);
   type Op_Table is array(Op_Code) of Op;

   function Deref(M: Machine; Ptr: Memory.Value) return Memory.Value is
      (M.Mem(Memory.Address(Ptr)));

   procedure Op_Add is new Intcode.Binary_Op(Bin_Op => "+");

   procedure Op_Mul(M: in out Machine) is
      A: constant Memory.Value := M.Mem(M.PC + 1);
      B: constant Memory.Value := M.Mem(M.PC + 2);
      Dst: constant Memory.Value := M.Mem(M.PC + 3);
   begin
      M.Mem(Memory.Address(Dst)) := Deref(M, A) * Deref(M, B);
      M.PC := M.PC + 4;
   end Op_Mul;

   Ops: constant Op_Table := (
      Add => Op_Add'Access,
      Mul => Op_Mul'Access,
      others => null
   );

   procedure Run(M: in out Machine) is
      Curr_Op: Op_Code;
   begin
      loop
         Curr_Op := Get_Op_Code(M.Mem(M.PC));
         exit when Curr_Op = Halt;

         if Ops(Curr_Op) = null then
            raise Program_Error;
         else
            Ops(Curr_Op)(M);
         end if;
      end loop;
   end;
end Intcode;
