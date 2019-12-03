with Intcode;
with Memory;

generic
   with function Bin_Op(X, Y: Memory.Value) return Memory.Value;
procedure Intcode.Binary_Op(M: in out Intcode.Machine);
