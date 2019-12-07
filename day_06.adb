with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Day_06 is
   subtype Name is String(1 .. 3);
   type Orbit is record
      Self, Parent: Name;
   end record;

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
   
   package Orbits is new Ada.Containers.Ordered_Maps(
         Key_Type => Name, Element_Type => Orbit);

   Locals: Orbits.Map;
   Total: Natural := 0;

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
begin
   while (not Ada.Text_IO.End_Of_File) loop
      declare
         O: constant Orbit := Get(Ada.Text_IO.Get_Line);
      begin
         Locals.Insert(Key => O.Self, New_Item => O);
      end;
   end loop;

   for E of Locals loop
      Total := Total + Count_Orbits(E);
   end loop;
   Ada.Integer_Text_IO.Put(Total);
end Day_06;
