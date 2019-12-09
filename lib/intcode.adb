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
      Relative_Base: Memory.Address := 0;

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

      function Read(
            From: Memory.Address;
            Mode: Parameter_Mode) return Memory.Value is
         V: constant Memory.Value := Peek(From);

      begin
         return (case Mode is
            when Immediate => V,
            when Position => Peek(Memory.Address(V)),
            when Relative => Peek(Relative_Base + Memory.Address(V)));
      end Read;
   begin
      loop
         declare
            Curr_Op: constant Schema := Decode(AM.Mem(PC));
            Params: array (Curr_Op.Params'Range) of Memory.Value;
            Store_To: constant Memory.Address := Memory.Address(
                  Read(
                     From => PC + Memory.Address(Params'Last),
                     Mode => Immediate));
            Recv: Maybe_Memory_Value;
         begin
            for I in Params'Range loop
               Params(I) := Read(
                  From => PC + Memory.Address(I), Mode => Curr_Op.Params(I));
            end loop;

            PC := PC + Params'Length + 1;
            case Curr_Op.Instruction is
               when Halt => exit;

               -- Arithmetic
               when Add => AM.Mem(Store_To) := Params(1) + Params(2);
               when Mul => AM.Mem(Store_To) := Params(1) * Params(2);

               -- I/O
               when Get =>
                  AM.Input.Get(Recv);
                  if Recv.Present then
                     AM.Mem(Store_To) := Recv.Value;
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
               when Mrb => Relative_Base := Memory.Address(Params(1));

               -- Comparison
               when Lt =>
                  AM.Mem(Store_To) := (if Params(1) < Params(2) then 1 else 0);
               when Eq =>
                  AM.Mem(Store_To) := (if Params(1) = Params(2) then 1 else 0);
            end case;
         end;
      end loop;

      AM.Input.Close;
      AM.Output.Close;
   end Executor;
end Intcode;
