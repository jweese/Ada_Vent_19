with Ada.Text_IO;
use Ada.Text_IO;

with Interfaces;

package Memory is
   type Address is range 0 .. 2 ** 63 - 1;
   type Value is new Interfaces.Integer_64;
   type Block is array(Address range <>) of Value;

   function Read_Comma_Separated(
         From: File_Type := Standard_Input) return Block;
end Memory;
