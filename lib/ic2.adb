with Intcode.Op;
use Intcode.Op;

package body IC2 is
   use type Memory.Address;

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
         begin
            for I in Params'Range loop
               Params(I) := Read(
                  From => PC + Memory.Address(I), Mode => Curr_Op.Params(I));
            end loop;

            exit;
         end;
      end loop;

      AM.Input.Close;
      AM.Output.Close;
   end Executor;
end IC2;
