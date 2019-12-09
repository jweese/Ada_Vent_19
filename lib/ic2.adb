with Intcode.Op;
use Intcode.Op;

package body IC2 is
   use type Memory.Address;
   use type Memory.Value;

   protected body Port is
      entry Put(I: in Integer) when Status /= Full is
      begin
         if Status = Empty then
            Value := I;
            Status := Full;
         end if;
      end Put;

      entry Get(X: out Maybe_Integer) when Status /= Empty is
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

      function Read(
            From: Memory.Address;
            Mode: Parameter_Mode) return Memory.Value is
         V: constant Memory.Value := AM.Mem(From);

      begin
         return (case Mode is
            when Immediate => V,
            when Position => AM.Mem(Memory.Address(V)));
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
            Recv: Maybe_Integer;
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
                     AM.Mem(Store_To) := Memory.Value(Recv.Value);
                  end if;
               when Put => AM.Output.Put(Integer(Params(1)));

               -- Transfer of Control
               when Jz =>
                  if Params(1) = 0 then
                     PC := Memory.Address(Params(2));
                  end if;
               when Jnz =>
                  if Params(1) /= 0 then
                     PC := Memory.Address(Params(2));
                  end if;

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
end IC2;