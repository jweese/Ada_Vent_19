with Memory;

package Intcode.Op is
   type Code is (
      Add,
      Mul,
      Get,
      Put,
      Jnz,
      Jz,
      Lt,
      Eq,
      Mrb,
      Halt
   );

   type Parameter_Mode is (Position, Immediate, Relative);
   type Parameter_List is array (Positive range <>) of Parameter_Mode;

   type Schema(Num_Params: Natural) is record
      Instruction: Code;
      Params: Parameter_List(1 .. Num_Params);
   end record;

   function Decode(V: Memory.Value) return Schema;
end Intcode.Op;
