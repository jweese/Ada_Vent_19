with Ada.Integer_Text_IO;

procedure Day_04 is
   type Cand is new Integer range 284639 .. 748759;

   function Is_Valid(C: Cand) return Boolean is
      Image: String(1..8) := (others => ' ');
      subtype Image_Middle is String(1..4);

      function Has_Double(S: Image_Middle) return Boolean is
         (S(1) /= S(2) and S(2) = S(3) and S(3) /= S(4));

      Non_Descending, Any_Has_Double: Boolean;
   begin
      Ada.Integer_Text_IO.Put(To => Image(2..7), Item => Integer(C));
      Any_Has_Double := (for some I in 1 .. 5 => Has_Double(Image(I .. I + 3)));
      Non_Descending := (for all I in 2 .. 6 => Image(I) <= Image(I + 1));
      return Non_Descending and Any_Has_Double;
   end Is_Valid;

   Count: Natural := 0;
begin
   for C in Cand'Range loop
      if Is_Valid(C) then
         Count := Count + 1;
      end if;
   end loop;
   Ada.Integer_Text_IO.Put(Count);
end Day_04;
