with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Day_06 is
   subtype Name is String(1 .. 3);
   You: constant Name := "YOU";
   Santa: constant Name := "SAN";

   type Orbit is record
      Self, Parent: Name;
   end record;

   type Chain is array (Positive range <>) of Name;

   package Orbits is new Ada.Containers.Ordered_Maps(
         Key_Type => Name, Element_Type => Orbit);
   Locals: Orbits.Map;

   function Get(S: String) return Orbit is
   begin
      for I in S'Range loop
         if S(I) = ')' then
            return (
               Parent => S(S'First .. I - 1),
               Self => S(I + 1 .. S'Last)
            );
         end if;
      end loop;
      raise Constraint_Error with "bad orbit" & S;
   end Get;
   
   function Count_Orbits(O: Orbit) return Positive is
      E: constant Orbits.Cursor := Locals.Find(O.Parent);
      use type Orbits.Cursor;
   begin
      if E = Orbits.No_Element then
         return 1;  -- only a direct orbit
      else
         return 1 + Count_Orbits(Orbits.Element(E));
      end if;
   end Count_Orbits;

   function Ancestors(O: Orbit) return Chain is
      Size : constant Natural := Count_Orbits(O) - 1;
      C: Chain(1 .. Size);
      Curr: Orbit := O;
   begin
      for I in C'Range loop
         C(I) := Curr.Parent;
         Curr := Locals.Element(Curr.Parent);
      end loop;
      return C;
   end Ancestors;

begin
   while (not Ada.Text_IO.End_Of_File) loop
      declare
         O: constant Orbit := Get(Ada.Text_IO.Get_Line);
      begin
         Locals.Insert(Key => O.Self, New_Item => O);
      end;
   end loop;

   declare
      Total: Natural := Natural'Last;
      You_Chain: constant Chain := Ancestors(Locals.Element(You));
      Santa_Chain: constant Chain := Ancestors(Locals.Element(Santa));
   begin
      for I in You_Chain'Range loop
         for J in Santa_Chain'Range loop
            if You_Chain(I) = Santa_Chain(J) then
               declare
                  Distance: constant Natural := I + J - 2;
               begin
                  if Distance < Total then Total := Distance; end if;
               end;
            end if;
         end loop;
      end loop;
      Ada.Integer_Text_IO.Put(Total);
   end;
end Day_06;
