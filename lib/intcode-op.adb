with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body Intcode.Op is
   use type Memory.Address;
   use type Memory.Value;

   Instruction_Size: constant array (Code) of Natural := (
      Add => 3,
      Mul => 3,
      Get => 1,
      Put => 1,
      Jnz => 2,
      Jz => 2,
      Lt => 3,
      Eq => 3,
      Halt => 0
   );

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

   function Load(
         From: Machine;
         PC_Offset: Positive;
         Mode: Parameter_Mode) return Memory.Value is
      V: constant Memory.Value := From.Mem(From.PC + Memory.Address(PC_Offset));
   begin
      case Mode is
         when Immediate => return V;
         when Position => return From.Mem(Memory.Address(V));
      end case;
   end Load;

   procedure Exec(S: in Schema; M: in out Machine) is
      Params: array (S.Params'Range) of Memory.Value;
      Store_To: constant Memory.Address := Memory.Address(
         Load(From => M, PC_Offset => Params'Last, Mode => Immediate));
   begin
      for I in Params'Range loop
         Params(I) := Load(From => M, PC_Offset => I, Mode => S.Params(I));
      end loop;

      case S.Instruction is
         when Halt => return;

         -- Arithmetic
         when Add => M.Mem(Store_To) := Params(1) + Params(2);
         when Mul => M.Mem(Store_To) := Params(1) * Params(2);

         -- IO
         when Get =>
            Ada.Text_IO.Put("? ");
            Ada.Integer_Text_IO.Get(Integer(M.Mem(Store_To)));
         when Put =>
            Ada.Integer_Text_IO.Put(Integer(Params(1)));
            Ada.Text_IO.New_Line;

         -- Transfer Control
         when Jnz =>
            if Params(1) /= 0 then
               M.PC := Memory.Address(Params(2));
               return;
            end if;
         when Jz =>
            if Params(1) = 0 then
               M.PC := Memory.Address(Params(2));
               return;
            end if;

         -- Comparison
         when Lt => M.Mem(Store_To) := (if Params(1) < Params(2) then 1 else 0);
         when Eq => M.Mem(Store_To) := (if Params(1) = Params(2) then 1 else 0);
      end case;

      M.PC := M.PC + Params'Length + 1;
   end Exec;
end Intcode.Op;
