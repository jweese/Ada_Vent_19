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
      M: Intcode.Machine(Hi_Mem => Mem'Last);
      I: Integer;
      O: Intcode.Output;
   begin
      M.Load_And_Exec(From => Mem);
      Ada.Text_IO.Put("? ");
      Ada.Integer_Text_IO.Get(I);
      M.Put(I);
      loop
         M.Get(O);
         exit when not O.Present;
         Ada.Integer_Text_IO.Put(O.Value);
         Ada.Text_IO.New_Line;
      end loop;
   end;
end Day_05;
