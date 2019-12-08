package Space_Image_Format is
   Width: constant := 25;
   Height: constant := 6;

   type Width_Range is range 1 .. Width;
   type Height_Range is range 1 .. Height;

   type Pixel is new Character range '0' .. '9';
   type Layer is array (Width_Range, Height_Range) of Pixel;
   type Layer_Stack is array (Positive range <>) of Layer;

   procedure Get_Pixel(P: out Pixel);
   procedure Get_Layer(L: out Layer);

   type Color is (Black, White, Transparent);
   subtype Visible_Color is Color range Black .. White;
   type Image is array (Width_Range, Height_Range) of Visible_Color;
   function To_Image(S: Layer_Stack) return Image;

   procedure Put_Image(I: in Image);
end Space_Image_Format;
