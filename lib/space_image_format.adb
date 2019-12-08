with Ada.Text_IO;

package body Space_Image_Format is
   procedure Get_Pixel(P: out Pixel) is
      C: Character;
   begin
      Ada.Text_IO.Get(C);
      P := Pixel(C);
   end Get_Pixel;

   procedure Get_Layer(L: out Layer) is
   begin
      for Row in Height_Range loop
         for Col in Width_Range loop
            Get_Pixel(L(Col, Row));
         end loop;
      end loop;
   end Get_Layer;

   type Color_Stack is array (Positive range <>) of Color;

   function To_Color(P: Pixel) return Color is
      (case P is
         when '0' => Black,
         when '1' => White,
         when '2' => Transparent,
         when others => raise Constraint_Error);

   function Get_Stack(
         LS: Layer_Stack; W: Width_Range; H: Height_Range) return Color_Stack is
      Result: Color_Stack(LS'Range);
   begin
      for I in LS'Range loop
         Result(I) := To_Color(LS(I)(W, H));
      end loop;
      return Result;
   end Get_Stack;

   function Visible(CS: Color_Stack) return Visible_Color is
   begin
      for C of CS loop
         if C in Visible_Color'Range then return C; end if;
      end loop;
      raise Constraint_Error;
   end Visible;

   function To_Image(S: Layer_Stack) return Image is
      Result: Image;
   begin
      for Row in Height_Range loop
         for Col in Width_Range loop
            Result(Col, Row) := Visible(Get_Stack(S, Col, Row));
         end loop;
      end loop;
      return Result;
   end To_Image;

   procedure Put_Image(I: in Image) is
      function Render(VC: Visible_Color) return Character is
         (case VC is
            when Black => ' ',
            when White => 'X');
   begin
      for Row in Height_Range loop
         for Col in Width_Range loop
            Ada.Text_IO.Put(Render(I(Col, Row)));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Put_Image;
end Space_Image_Format;
