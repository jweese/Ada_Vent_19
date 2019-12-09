with Ada.Command_Line;
with Ada.Text_IO;
use Ada.Text_IO;

with Intcode;
with Memory;

procedure Day_05 is
   package IO is new Ada.Text_IO.Integer_IO(Memory.Value);
   Program_Name: constant String := Ada.Command_Line.Argument(1);
   F: File_Type;
begin
   Open(F, In_File, Program_Name);
   declare 
      Mem: constant Memory.Block := Memory.Read_Comma_Separated(F);
      M: aliased Intcode.Machine :=
        (Hi_Mem => Mem'Last,
         Mem => Mem,
         Aux_Mem => Intcode.Aux_Memory.Empty_Map,
         Input => new Intcode.Port,
         Output => new Intcode.Port);
      Exec: Intcode.Executor(M'Access);
      I: Memory.Value;
      O: Intcode.Maybe_Memory_Value;
   begin
      Ada.Text_IO.Put("? ");
      IO.Get(I);
      M.Input.Put(I);
      loop
         M.Output.Get(O);
         exit when not O.Present;
         IO.Put(O.Value);
         Ada.Text_IO.New_Line;
      end loop;
   end;
end Day_05;
