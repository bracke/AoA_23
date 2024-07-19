with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure AOA_18_1 is
   Size : constant := 1_000;

   type Map_Grid is array (1 .. Size, 1 .. Size) of Character;

   Ground : constant Character := '.';
   Hole   : constant Character := '#';

   procedure Fill (Map : in out Map_Grid; X : in Positive; Y : in Positive);
   -- Flood fill starting at X, Y

   Min_X : Positive := Integer'Last;
   Min_Y : Positive := Integer'Last;
   Max_X : Natural  := 0;
   Max_Y : Natural  := 0;

   procedure Fill (Map : in out Map_Grid; X : in Positive; Y : in Positive) is
      -- Empty
   begin -- Fill
      if Map (X, Y) = Hole then
         return;
      end if;

      Map (X, Y) := Hole;

      if X > Min_X then
         Fill (Map => Map, X => X - 1, Y => Y);
      end if;

      if Y > Min_Y then
         Fill (Map => Map, X => X, Y => Y - 1);
      end if;

      if X < Max_X then
         Fill (Map => Map, X => X + 1, Y => Y);
      end if;

      if Y < Max_Y then
         Fill (Map => Map, X => X, Y => Y + 1);
      end if;
   end Fill;

   Input : Ada.Text_IO.File_Type;
   Map   : Map_Grid := (others => (others => Ground) );
   X     : Natural  := Size / 2;
   Y     : Natural  := X;
   Sum   : Natural  := 0;
begin -- AOA_18_1
   Map (X, Y) := Hole;
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_18");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant String   := Ada.Text_IO.Get_Line (Input);
         Stop : constant Natural  := Ada.Strings.Fixed.Index (Line, " ", From => 3);
         Num  : constant Positive := Integer'Value (Line (3 .. Stop - 1) );
      begin -- One_Line
         case Line (1) is
         when 'U' =>
            Dig_Up : for I in Y - Num .. Y - 1 loop
               Map (X, I) := Hole;
            end loop Dig_Up;

            Y := Y - Num;
         when 'R' =>
            Dig_Right : for I in X + 1 .. X + Num loop
               Map (I, Y) := Hole;
            end loop Dig_Right;

            X := X + Num;
         when 'D' =>
            Dig_Down : for I in Y + 1 .. Y + Num loop
               Map (X, I) := Hole;
            end loop Dig_Down;

            Y := Y + Num;
         when 'L' =>
            Dig_Left : for I in X - Num .. X - 1 loop
               Map (I, Y) := Hole;
            end loop Dig_Left;

            X := X - Num;
         when others =>
            raise Program_Error with "Invalid direction " & Line (1);
         end case;
      end One_Line;

      Min_X := Integer'Min (X, Min_X);
      Min_Y := Integer'Min (Y, Min_Y);
      Max_X := Integer'Max (X, Max_X);
      Max_Y := Integer'Max (Y, Max_Y);
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);

   Y := (Min_Y + Max_Y) / 2;

   Find_Hole : for I in Min_X .. Max_X loop
      if Map (I, Y) = Hole then
         X := I + 1;

         exit Find_Hole;
      end if;
   end loop Find_Hole;

   Find_Ground : for I in X .. Max_X loop
      if Map (I, Y) = Ground then
         Fill (Map => Map, X => I, Y => Y);

         exit Find_Ground;
      end if;
   end loop Find_Ground;

   Sum_Y : for Y in Min_Y .. Max_Y loop
      Sum_X : for X in Min_X .. Max_X loop
         if Map (X, Y) = Hole then
            Sum := Sum + 1;
         end if;
      end loop Sum_X;
   end loop Sum_Y;

   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_18_1;
