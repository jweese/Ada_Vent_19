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
      accept Load(From: in Memory.Block) do
         Mem := From;
         PC := 0;
      end Load;

      accept Exec;
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

               when others => null;
            end case;

            PC := PC + Params'Length + 1;
         end;
      end loop;

      -- Optionally save memory contents.
      select
         accept Save(To: out Memory.Block) do
            To := Mem;
         end Save;
         or
         terminate;
      end select;
   end Machine;
end Intcode;
