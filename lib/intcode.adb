with Intcode.Op; use Intcode.Op;

package body Intcode is
   use type Memory.Address;
   use type Memory.Value;

   task body Machine is
      PC: Memory.Address; -- program counter
      Mem: Memory.Block(0 .. Hi_Mem);

      function Read(
            From: Memory.Address;
            Mode: Parameter_Mode) return Memory.Value is
         V: constant Memory.Value := Mem(From);
      begin
         return (case Mode is
            when Immediate => V,
            when Position => Mem(Memory.Address(V)));
      end Read;

   begin
      accept Load_And_Exec(From: in Memory.Block) do
         Mem := From;
         PC := 0;
      end Load_And_Exec;

      loop
         declare
            Curr_Op: constant Schema := Decode(Mem(PC));
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

            case Curr_Op.Instruction is
               when Halt => exit;

               -- Arithmetic
               when Add => Mem(Store_To) := Params(1) + Params(2);
               when Mul => Mem(Store_To) := Params(1) * Params(2);

               -- I/O
               when Get =>
                  accept Put(I: in Integer) do
                     Mem(Store_To) := Memory.Value(I);
                  end Put;
               when Put =>
                  accept Get(I: out Integer) do
                     I := Integer(Params(1));
                  end Get;

               -- Transfer of Control
               when Jz =>
                  if Params(1) = 0 then
                     PC := Memory.Address(Params(2));
                     goto Continue;
                  end if;
               when Jnz =>
                  if Params(1) /= 0 then
                     PC := Memory.Address(Params(2));
                     goto Continue;
                  end if;

               -- Comparison
               when Lt =>
                  Mem(Store_To) := (if Params(1) < Params(2) then 1 else 0);
               when Eq =>
                  Mem(Store_To) := (if Params(1) = Params(2) then 1 else 0);
            end case;

            PC := PC + Params'Length + 1;
            <<Continue>>
         end;
      end loop;

      -- Optionally save memory contents.
      select
         accept Save(To: out Memory.Block) do
            To := Mem;
         end Save;
         or
         accept Shutdown;
         or
         terminate;
      end select;
   end Machine;
end Intcode;
