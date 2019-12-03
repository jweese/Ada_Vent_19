procedure Intcode.Binary_Op(M: in out Intcode.Machine) is
   use type Memory.Address;

   A: constant Memory.Value := M.Mem(M.PC + 1);
   B: constant Memory.Value := M.Mem(M.PC + 2);
   D: constant Memory.Value := M.Mem(M.PC + 3);

   Dst: constant Memory.Address := Memory.Address(D);

   function Deref(Ptr: Memory.Value) return Memory.Value is
      (M.Mem(Memory.Address(Ptr)));
begin
   M.Mem(Dst) := Bin_Op(Deref(A), Deref(B));
   M.PC := M.PC + 4;
end Intcode.Binary_Op;
