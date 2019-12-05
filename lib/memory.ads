with Ada.Text_IO;
use Ada.Text_IO;

package Memory is
   type Address is new Natural;
   type Value is new Integer;
   type Block is array(Address range <>) of Value;

   function Read_Comma_Separated(
         From: File_Type := Standard_Input) return Block;
end Memory;
