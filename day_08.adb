with Ada.Containers.Vectors;
with Ada.Text_IO;
with Space_Image_Format;
use Space_Image_Format;

procedure Day_08 is
   function Count_Pixel(Within: Layer; Value: Pixel) return Natural is
      Total: Natural := 0;
   begin
      for P of Within loop
         if P = Value then Total := Total + 1; end if;
      end loop;
      return Total;
   end Count_Pixel;

   package Layer_Vector is new Ada.Containers.Vectors(
      Index_Type => Positive, Element_Type => Layer);

   All_Layers: Layer_Vector.Vector;
   Least_Zeroed_Layer: Layer;
   Zero_Count: Natural := Natural'Last;
begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         L: Layer;
         Z: Natural;
      begin
         Get_Layer(L);
         Z := Count_Pixel(Within => L, Value => '0');
         if Z < Zero_Count then
            Zero_Count := Z;
            Least_Zeroed_Layer := L;
         end if;
         All_Layers.Append(L);
      end;
   end loop;

   declare
      Ones: constant Natural :=
            Count_Pixel(Within => Least_Zeroed_Layer, Value => '1');
      Twos: constant Natural :=
            Count_Pixel(Within => Least_Zeroed_Layer, Value => '2');

      LS: Layer_Stack(All_Layers.First_Index .. All_Layers.Last_Index);
      Output: Image;
   begin
      Ada.Text_IO.Put_Line("checksum:" & Natural'Image(Ones * Twos));
      Ada.Text_IO.New_Line;
      for I in LS'Range loop
         LS(I) := All_Layers.Element(I);
      end loop;
      Output := To_Image(LS);
      Put_Image(Output);
   end;
end Day_08;
