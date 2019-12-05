package body Intcode.Op is
   use type Memory.Address;
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

   function Load(From: Machine; PC_Offset: Positive; Mode: Parameter_Mode)
         return Memory.Value is
      V: constant Memory.Value := From.Mem(From.PC + Memory.Address(PC_Offset));
   begin
      case Mode is
         when Immediate => return V;
         when Position => return From.Mem(Memory.Address(V));
      end case;
   end Load;

   procedure Exec(S: in Schema; M: in out Machine) is
      Params: array (S.Params'Range) of Memory.Value;
   begin
      for I in Params'Range loop
         Params(I) := Load(From => M, PC_Offset => I, Mode => S.Params(I));
      end loop;
      M.PC := M.PC + Params'Length + 1;
   end Exec;
end Intcode.Op;
