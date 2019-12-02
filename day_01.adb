with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_01 is
   function Fuel(Mass: Natural) return Natural is
      Result: constant Integer := Mass / 3 - 2;
   begin
      if Result <= 0 then
         return 0;
      else
         return Result + Fuel(Result);
      end if;
   end Fuel;

   Total_Fuel: Natural := 0;
   Module_Mass: Natural;
begin
   while (not Ada.Text_IO.End_Of_File) loop
      Ada.Integer_Text_IO.Get(Module_Mass);
      Total_Fuel := Total_Fuel + Fuel(Module_Mass);
   end loop;
   Ada.Integer_Text_IO.Put(Total_Fuel);
end Day_01;
