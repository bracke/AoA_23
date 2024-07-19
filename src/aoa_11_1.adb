with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure AOA_11_1 is
   Size : constant := 140;

   type Map_Row  is array (1 .. Size) of Boolean;
   type Map_Grid is array (1 .. Size) of Map_Row;

   type Coordinate is record
      Y : Positive;
      X : Positive;
   end record;

   package Coord_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Coordinate);

   procedure Increment_X (Galaxy : in out Coord_Lists.Vector; X : in Positive);
   -- Increments the X component of all elements of Galaxy with an X component > X

   procedure Increment_Y (Galaxy : in out Coord_Lists.Vector; Y : in Positive);
   -- Increments the y component of all elements of Galaxy with a y component > Y

   procedure Increment_X (Galaxy : in out Coord_Lists.Vector; X : in Positive) is
      Coord : Coordinate;
   begin -- Increment_X
      All_Galaxies : for I in 1 .. Galaxy.Last_Index loop
         Coord := Galaxy.Element (I);

         if Coord.X > X then
            Coord.X := Coord.X + 1;
            Galaxy.Replace_Element (Index => I, New_Item => Coord);
         end if;
      end loop All_Galaxies;
   end Increment_X;

   procedure Increment_Y (Galaxy : in out Coord_Lists.Vector; Y : in Positive) is
      Coord : Coordinate;
   begin -- Increment_Y
      All_Galaxies : for I in 1 .. Galaxy.Last_Index loop
         Coord := Galaxy.Element (I);

         if Coord.Y > Y then
            Coord.Y := Coord.Y + 1;
            Galaxy.Replace_Element (Index => I, New_Item => Coord);
         end if;
      end loop All_Galaxies;
   end Increment_Y;

   Input  : Ada.Text_IO.File_Type;
   Map    : Map_Grid := (others => (others => False) );
   Y      : Integer  := Size;
   X      : Integer  := Size;
   Galaxy : Coord_Lists.Vector;
   Sum    : Natural := 0;
begin -- AOA_11_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_11");

   All_Lines : for Y in 1 .. Size loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin -- One_Line
         All_Points : for X in Line'Range loop
            Map (Y) (X) := Line (X) = '#';

            if Map (Y) (X) then
               Galaxy.Append (New_Item => (Y => Y, X => X) );
            end if;
         end loop All_Points;
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);

   Replicate_Columns : loop
      exit Replicate_Columns when X < 1;

      if (for all Y in 1 .. Size => not Map (Y) (X) ) then
         Increment_X (Galaxy => Galaxy, X => X);
      end if;

      X := X - 1;
   end loop Replicate_Columns;

   Replicate_Rows : loop
      exit Replicate_Rows when Y < 1;

      if Map (Y) = Map_Row'(others => False) then
         Increment_Y (Galaxy => Galaxy, Y => Y);
      end if;

      Y := Y - 1;
   end loop Replicate_Rows;

   Calc_Distances : for I in 1 .. Galaxy.Last_Index - 1 loop
      All_Others : for J in I + 1 .. Galaxy.Last_Index loop
         Sum := Sum + abs (Galaxy.Element (I).Y - Galaxy.Element (J).Y) + abs (Galaxy.Element (I).X - Galaxy.Element (J).X);
      end loop All_Others;
   end loop Calc_Distances;

   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_11_1;
