package body Intcode.Op is
   use type Memory.Value;

   function Instruction_Size(Instruction: Code) return  Natural is
      (case Instruction is
         when Add|Mul => 3,
         when Get|Put|Mrb => 1,
         when Jnz|Jz => 2,
         when Lt|Eq => 3,
         when Halt => 1);

   function Get_Code(V: Memory.Value) return Code is
   begin
      case V is
         when 1 => return Add;
         when 2 => return Mul;
         when 3 => return Get;
         when 4 => return Put;
         when 5 => return Jnz;
         when 6 => return Jz;
         when 7 => return Lt;
         when 8 => return Eq;
         when 9 => return Mrb;
         when 99 => return Halt;
         when others => raise Constraint_Error with "unknown op code" & V'Image;
      end case;
   end Get_Code;

   function Decode(V: Memory.Value) return Schema is
      I: constant Code := Get_Code(V mod 100);
      Size: constant Natural := Instruction_Size(I);
      Result: Schema :=
         (Num_Params => Size, Instruction => I, Params => (others => Position));
      W: Memory.Value := V / 100;
   begin
      for I in Result.Params'Range loop
         Result.Params(I) :=
            (case W mod 10 is
               when 0 => Position,
               when 1 => Immediate,
               when 2 => Relative,
               when others => raise Constraint_Error with "unknown mode");
         W := W / 10;
      end loop;
      return Result;
   end Decode;
end Intcode.Op;
