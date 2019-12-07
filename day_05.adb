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
   begin
      M.Load(From => Mem);
      M.Exec;
      Ada.Text_IO.Put("? ");
      Ada.Integer_Text_IO.Get(I);
      M.Put(I);
      loop
         select
            M.Get(I);
            Ada.Integer_Text_IO.Put(I);
            Ada.Text_IO.New_Line;
         else
            null;
         end select;

         select
            M.Shutdown;
            exit;
         else
            null;
         end select;
      end loop;
   end;
end Day_05;
