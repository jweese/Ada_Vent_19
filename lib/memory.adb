with Ada.Containers.Vectors;
with Ada.IO_Exceptions;

package body Memory is
   package IO is new Ada.Text_IO.Integer_IO(Value);

   function Read_Comma_Separated(
         From: File_Type := Standard_Input) return Block is
      package Vec is new Ada.Containers.Vectors(
         Index_type => Address, Element_Type => Value);
      V: Vec.Vector := Vec.Empty_Vector;
      Curr: Value;
      Comma: Character;
   begin
      loop
         IO.Get(From, Curr);
         V.Append(Curr);
         exit when Ada.Text_IO.End_Of_File(From);
         Ada.Text_IO.Get(From, Comma);
         if Comma /= ',' then
            raise Ada.IO_Exceptions.Data_Error;
         end if;
      end loop;
      declare
         Result: Block(V.First_Index .. V.Last_Index);
      begin
         for Addr in Result'Range loop
            Result(Addr) := V(Addr);
         end loop;
         return Result;
      end;
   end Read_Comma_Separated;
end Memory;
