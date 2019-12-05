with Ada.Integer_Text_IO;

procedure Day_04 is
   type Cand is new Integer range 284639 .. 748759;

   function Is_Valid(C: Cand) return Boolean is
      Image: String(1..6);
      subtype Image_Edge is String(1..3);
      subtype Image_Middle is String(1..4);

      function Has_Double_Begin(S: Image_Edge) return Boolean is
         (S(1) = S(2) and S(2) /= S(3));

      function Has_Double_End(S: Image_Edge) return Boolean is
         (S(1) /= S(2) and S(2) = S(3));

      function Has_Double_Middle(S: Image_Middle) return Boolean is
         (S(1) /= S(2) and S(2) = S(3) and S(3) /= S(4));

      Non_Descending: Boolean := True;
      Has_Double: Boolean := False;
   begin
      Ada.Integer_Text_IO.Put(To => Image, Item => Integer(C));

      Has_Double :=
         Has_Double_Begin(Image(1..3))
         or Has_Double_End(Image(4..6))
         or (for some I in 1 .. 3 => Has_Double_Middle(Image(I .. I + 3)));
      Non_Descending := (for all I in 1 .. 5 => Image(I) <= Image(I + 1));

      return Non_Descending and Has_Double;
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
