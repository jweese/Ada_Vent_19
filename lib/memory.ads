package Memory is
   type Address is new Natural;
   type Value is new Integer;
   type Block is array(Address range <>) of Value;

   function Read_Comma_Separated return Block;
end Memory;
