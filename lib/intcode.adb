with Ada.Text_IO;
with Intcode.Op;
use Intcode.Op;

package body Intcode is
   protected body Port is
      entry Put(I: in Memory.Value) when Status /= Full is
      begin
         if Status = Empty then
            Value := I;
            Status := Full;
         end if;
      end Put;

      entry Get(X: out Maybe_Memory_Value) when Status /= Empty is
      begin
         case Status is
            when Full =>
               X := (Present => True, Value => Value);
               Status := Empty;
            when others => X := (Present => False);
         end case;
      end Get;

      entry Close when Status /= Full is
      begin
         Status := Closed;
      end Close;
   end Port;

   task body Executor is
      PC: Memory.Address := 0;
      Relative_Base: Memory.Value := 0;

      -- physical layer of memory: Peek/Poke
      function Peek(From: Memory.Address) return Memory.Value is
      begin
         if From in AM.Mem'Range then
            return AM.Mem(From);
         elsif AM.Aux_Mem.Contains(From) then
            return AM.Aux_Mem.Element(From);
         else
            return 0;
         end if;
      end Peek;

      procedure Poke(To: Memory.Address; Value: Memory.Value) is
      begin
         if To in AM.Mem'Range then
            AM.Mem(To) := Value;
         elsif AM.Aux_Mem.Contains(To) then
            AM.Aux_Mem.Replace(To, Value);
         else
            AM.Aux_Mem.Insert(To, Value);
         end if;
      end Poke;

      -- logical layer of memory: Read/Store
      function Read(
            From: Memory.Address;
            Mode: Parameter_Mode) return Memory.Value is
         V: constant Memory.Value := Peek(From);
      begin
         return (case Mode is
            when Immediate => V,
            when Position => Peek(Memory.Address(V)),
            when Relative => Peek(Memory.Address(Relative_Base + V)));
      end Read;

      procedure Store(
            To: Memory.Address; Mode: Parameter_Mode; Value: Memory.Value) is
         V: constant Memory.Value := Peek(To);
         M: constant Memory.Address :=
            (case Mode is
               when Position => Memory.Address(V),
               when Relative => Memory.Address(Relative_Base + V),
               when Immediate =>
                  raise Constraint_Error with "store to relative");
      begin
         Poke(M, Value);
      end Store;

   begin
      loop
         declare
            Curr_Op: constant Schema := Decode(AM.Mem(PC));
            Params: array (Curr_Op.Params'Range) of Memory.Value;
            Dst: constant Memory.Address :=
                  PC + Memory.Address(Params'Last);
            M: constant Parameter_Mode := Curr_Op.Params(Params'Last);
            Recv: Maybe_Memory_Value;
         begin
            for I in Params'Range loop
               Params(I) := Read(
                  From => PC + Memory.Address(I), Mode => Curr_Op.Params(I));
            end loop;

            PC := PC + Params'Length + 1;
            -- Ada.Text_IO.Put_Line(Curr_Op.Instruction'Image);
            case Curr_Op.Instruction is
               when Halt => exit;

               -- Arithmetic
               when Add => Store(
                  To => Dst,
                  Mode => M,
                  Value => Params(1) + Params(2));
               when Mul => Store(
                  To => Dst,
                  Mode => M,
                  Value => Params(1) * Params(2));

               -- I/O
               when Get =>
                  AM.Input.Get(Recv);
                  if Recv.Present then
                     Store(To => Dst, Mode => M, Value => Recv.Value);
                  end if;
               when Put => AM.Output.Put(Params(1));

               -- Transfer of Control
               when Jz =>
                  if Params(1) = 0 then
                     PC := Memory.Address(Params(2));
                  end if;
               when Jnz =>
                  if Params(1) /= 0 then
                     PC := Memory.Address(Params(2));
                  end if;

               -- Modify relative base
               when Mrb => Relative_Base := Relative_Base + Params(1);

               -- Comparison
               when Lt =>
                  Store(
                     To => Dst,
                     Mode => M,
                     Value => (if Params(1) < Params(2) then 1 else 0));
               when Eq =>
                  Store(
                     To => Dst,
                     Mode => M,
                     Value => (if Params(1) = Params(2) then 1 else 0));
            end case;
         end;
      end loop;

      AM.Input.Close;
      AM.Output.Close;
   end Executor;
end Intcode;
