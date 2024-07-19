with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure AOA_23_1 is
   subtype Slope is Character with Dynamic_Predicate => Slope in '^' | 'v' | '<' | '>';
   subtype Path_Char is Character with Dynamic_Predicate => Path_Char in Slope | '.';

   type Tile_Info is record
      Kind    : Character;
      Visited : Boolean := False;
   end record;

   package Tile_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Tile_Info);
   package Row_Lists  is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Tile_Lists.Vector, "=" => Tile_Lists."=");

   type Map_Grid is array (Positive range <>, Positive range <>) of Tile_Info;

   package Map_Holders is new Ada.Containers.Indefinite_Holders (Element_Type => Map_Grid);

   function Max (Map : in out Map_Grid; X : in Positive; Y : in Positive; Count : in Natural) return Natural with
      Pre => (X in Map'Range (1) and Y in Map'Range (2) ) and then (Map (X, Y).Kind in Path_Char and not Map (X, Y).Visited);
   -- Finds the max-length path to the end point, given that reaching X, Y took Count steps
   -- Map has all visited tiles (excluding X, Y) so marked

   function Max (Map : in out Map_Grid; X : in Positive; Y : in Positive; Count : in Natural) return Natural is
      Store  : Map_Holders.Holder;
      Result : Natural  := 0;
   begin -- Max
      Map (X, Y).Visited := True;
      Store.Replace_Element (New_Item => Map);

      if Y = Map'Last (2) then
         return Count;
      end if;

      if Y + 1 = Map'Last (2) and Map (X, Y).Kind = 'v' then
         return Count + 1;
      end if;

      if Map (X, Y).Kind in Slope then
         if Map (X, Y).Kind = '^' and not Map (X, Y - 1).Visited then
            return Max (Map, X, Y - 1, Count + 1);
         elsif Map (X, Y).Kind = 'v' and not Map (X, Y + 1).Visited then
            return Max (Map, X, Y + 1, Count + 1);
         elsif Map (X, Y).Kind = '<' and not Map (X - 1, Y).Visited then
            return Max (Map, X - 1, Y, Count + 1);
         elsif Map (X, Y).Kind = '>' and not Map (X + 1, Y).Visited then
            return Max (Map, X + 1, Y, Count + 1);
         else
            return Integer'Max (0, Count - 1); -- Can't step here
         end if;
      end if; -- else Map (X, Y).Kind = '.'

      if X > 1 and then (Map (X - 1, Y).Kind in Path_Char and not Map (X - 1, Y).Visited) then
         Result := Max (Map, X - 1, Y, Count + 1);
      end if;

      if X < Map'Last (1) and then (Map (X + 1, Y).Kind in Path_Char and not Map (X + 1, Y).Visited) then
         Map := Store.Element;
         Result := Integer'Max (Max (Map, X + 1, Y, Count + 1), Result);
      end if;

      if Y > 1 and then (Map (X, Y - 1).Kind in Path_Char and not Map (X, Y - 1).Visited) then
         Map := Store.Element;
         Result := Integer'Max (Max (Map, X, Y - 1, Count + 1), Result);
      end if;

      if Y < Map'Last (2) and then (Map (X, Y + 1).Kind in Path_Char and not Map (X, Y + 1).Visited) then
         Map := Store.Element;
         Result := Integer'Max (Max (Map, X, Y + 1, Count + 1), Result);
      end if;

      return Result;
   end Max;

   Input : Ada.Text_IO.File_Type;
   Map_V : Row_Lists.Vector;
begin -- AOA_23_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_23");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         Row : Tile_Lists.Vector;
      begin -- One_Line
         All_Tiles : for C of Line loop
            Row.Append (New_Item => (Kind => C, others => <>) );
         end loop All_Tiles;

         Map_V.Append (New_Item => Row);
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);

   Convert : declare
      Map : Map_Grid (1 .. Map_V.Element (1).Last_Index, 1 .. Map_V.Last_Index);
      Row : Tile_Lists.Vector;
   begin -- Convert
      All_Rows : for Y in Map'Range (2) loop
         Row := Map_V.Element (Y);

         All_Columns : for X in Map'Range (1) loop
            Map (X, Y) := Row.Element (X);
         end loop All_Columns;
      end loop All_Rows;

      Find_Start : for X in Map'Range (1) loop
         if Map (X, 1).Kind = '.' then
            Ada.Text_IO.Put_Line (Item => Integer'Image (Max (Map, X, 1, 0) ) );

            exit Find_Start;
         end if;
      end loop Find_Start;
   end Convert;
end AOA_23_1;
