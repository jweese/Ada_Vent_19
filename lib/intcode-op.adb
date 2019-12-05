package body Intcode.Op is
   use type Memory.Value;

   Instruction_Size: constant array (Code) of Natural := (
      Add => 3,
      Mul => 3,
      Halt => 0
   );

   function Get_Code(V: Memory.Value) return Code is
   begin
      case V is
            when 1 => return Add;
            when 2 => return Mul;
            when 99 => return Halt;
            when others => raise Constraint_Error with "op code" & V'Image;
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
         if W mod 10 = 1 then
            Result.Params(I) := Immediate;
         else
            Result.Params(I) := Position;
         end if;
         W := W / 10;
      end loop;
      return Result;
   end Decode;

   procedure Exec(S: in Schema; M: in out Machine) is
   begin
      null;
   end Exec;
end Intcode.Op;
