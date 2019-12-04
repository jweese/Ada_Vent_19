with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;

procedure Day_03 is
   type Point is record
      X, Y: Integer;
   end record;

   package Wire_Vec is new Ada.Containers.Vectors(
      Index_Type => Natural, Element_Type => Point);

   procedure Append_Comma_Separated_Wire_Points(W: in out Wire_Vec.Vector) is
      Curr: constant Point := W.Last_Element;
      Dir: Character;
      Distance: Natural;
   begin
      Ada.Text_IO.Get(Dir);
      Ada.Integer_Text_IO.Get(Distance);
      case Dir is
         when 'U' =>
            for I in 1 .. Distance loop
               W.Append(Point'(X => Curr.X, Y => Curr.Y + I));
            end loop;
         when 'D' =>
            for I in 1 .. Distance loop
               W.Append(Point'(X => Curr.X, Y => Curr.Y - I));
            end loop;
         when 'R' =>
            for I in 1 .. Distance loop
               W.Append(Point'(X => Curr.X + I, Y => Curr.Y));
            end loop;
         when 'L' =>
            for I in 1 .. Distance loop
               W.Append(Point'(X => Curr.X - I, Y => Curr.Y));
            end loop;
         when others => raise Ada.IO_Exceptions.Data_Error with Character'Image(Dir);
      end case;
   end Append_Comma_Separated_Wire_Points;

   function Read_Wire return Wire_Vec.Vector is
      W: Wire_Vec.Vector;
      Comma: Character;
   begin
      W.Append(Point'(0, 0));
      loop
         Append_Comma_Separated_Wire_Points(W);
         exit when Ada.Text_IO.End_Of_Line;
         Ada.Text_IO.Get(Comma);
      end loop;
      return W;
   end Read_Wire;

   -- only used in part 1
   -- function Manhattan(P: Point) return Natural is (abs(P.X) + abs(P.Y));

   Wire_1: constant Wire_Vec.Vector := Read_Wire;
   Wire_2: constant Wire_Vec.Vector := Read_Wire;
   Min_Dist: Natural := 0;
begin
   for I in Wire_1.First_Index .. Wire_1.Last_Index loop
      declare
         P: constant Point := Wire_1(I);
         J: constant Wire_Vec.Extended_Index := Wire_2.Find_Index(P);
         Dist: constant Natural := I + J;
      begin
         if J /= Wire_Vec.No_Index then
            if Min_Dist = 0 or else Dist < Min_Dist then
               Min_Dist := Dist;
            end if;
         end if;
      end;
   end loop;
   Ada.Integer_Text_IO.Put(Min_Dist);
end Day_03;
