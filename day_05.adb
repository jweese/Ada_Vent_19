with Ada.Command_Line;
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
      M: Intcode.Machine := Intcode.New_Machine(Mem);
   begin
      M.Run;
   end;
end Day_05;
