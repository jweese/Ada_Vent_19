with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
use Ada.Text_IO;

with Intcode;
with Memory;

procedure Day_05 is
   Program_Name: constant String := Ada.Command_Line.Argument(1);
   F: File_Type;
begin
   Open(F, In_File, Program_Name);
   declare 
      Mem: constant Memory.Block := Memory.Read_Comma_Separated(F);
      M: aliased Intcode.Machine :=
        (Hi_Mem => Mem'Last,
         Mem => Mem,
         Input => new Intcode.Port,
         Output => new Intcode.Port);
      Exec: Intcode.Executor(M'Access);
      I: Integer;
      O: Intcode.Maybe_Integer;
   begin
      Ada.Text_IO.Put("? ");
      Ada.Integer_Text_IO.Get(I);
      M.Input.Put(I);
      loop
         M.Output.Get(O);
         exit when not O.Present;
         Ada.Integer_Text_IO.Put(O.Value);
         Ada.Text_IO.New_Line;
      end loop;
   end;
end Day_05;
