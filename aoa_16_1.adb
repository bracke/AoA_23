with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO;
with PragmARC.Data_Structures.Queues.Unbounded.Unprotected;

procedure AOA_16_1 is
   type Tile_Kind_ID is (Empty, Vertical, Horizontal, Sinister, Dexter); -- .|-/\

   type Tile_Info is record
      Kind      : Tile_Kind_ID;
      Energized : Boolean := False;
   end record;

   package Tile_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Tile_Info);

   package Contraption is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Tile_Lists.Vector, "=" => Tile_Lists."=");

   type Direction_ID is (Up, Right, Down, Left);

   type Position_Info is record
      X   : Natural;
      Y   : Natural;
      Dir : Direction_ID;
   end record;

   package Position_Queues is new PragmARC.Data_Structures.Queues.Unbounded.Unprotected (Element => Position_Info);

   function Hash (Position : in Position_Info) return Ada.Containers.Hash_Type;

   procedure Move (Position : in out Position_Info);
   -- Simple move with no reflection or splitting

   function Hash (Position : in Position_Info) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin -- Hash
      return Ada.Containers.Hash_Type (Position.X) xor
             2 **  8 * Ada.Containers.Hash_Type (Position.Y) xor
             2 ** 30 * Direction_ID'Pos (Position.Dir);
   end Hash;

   procedure Move (Position : in out Position_Info) is
      -- Empty
   begin -- Move
      case Position.Dir is
      when Up =>
         Position.Y := Position.Y - 1;
      when Right =>
         Position.X := Position.X + 1;
      when Down =>
         Position.Y := Position.Y + 1;
      when Left =>
         Position.X := Position.X - 1;
      end case;
   end Move;

   package Position_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Position_Info, Hash => Hash, Equivalent_Elements => "=");

   Input : Ada.Text_IO.File_Type;
   Grid  : Contraption.Vector;
   Row   : Tile_Lists.Vector;
   Tile  : Tile_Info;
   Pos   : Position_Info;
   Beam  : Position_Queues.Handle;
   Seen  : Position_Sets.Set;
   Sum   : Natural := 0;
begin -- AOA_16_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_16");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         Tile : Tile_Info;
         Row  : Tile_Lists.Vector;
      begin -- One_Line
         Convert : for C of Line loop
            Tile.Kind := (case C is when '.' => Empty,
                                    when '|' => Vertical,
                                    when '-' => Horizontal,
                                    when '/' => Sinister,
                                    when '\' => Dexter,
                                    when others => raise Program_Error with "invalid input " & C);
            Row.Append (New_Item => Tile);
         end loop Convert;

         Grid.Append (New_Item => Row);
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);

   Row := Grid.Element (1);
   Tile := Row.Element (1);
   Tile.Energized := True;
   Row.Replace_Element (Index => 1, New_Item => Tile);
   Grid.Replace_Element (Index => 1, New_Item => Row);
   Pos := (X => 1, Y => 1, Dir => Right);
   Beam.Put (Item => Pos);

   All_Beams : loop
      exit All_Beams when Beam.Is_Empty;

      Beam.Get (Item => Pos);
      Tile := Grid.Element (Pos.Y).Element (Pos.X); -- Assert Tile.Energized

      All_Steps : loop
         exit All_Steps when Seen.Contains (Pos); -- Found a cycle

         Seen.Insert (New_Item => Pos);

         case Tile.Kind is
         when Empty =>
            Move (Position => Pos);
         when Vertical =>
            if Pos.Dir in Up | Down then
               Move (Position => Pos);
            else -- Split
               Pos.Dir := Up;
               Beam.Put (Item => Pos);
               Pos.Dir := Down;
               Beam.Put (Item => Pos);

               exit All_Steps;
            end if;
         when Horizontal =>
            if Pos.Dir in Right | Left then
               Move (Position => Pos);
            else -- Split
               Pos.Dir := Right;
               Beam.Put (Item => Pos);
               Pos.Dir := Left;
               Beam.Put (Item => Pos);

               exit All_Steps;
            end if;
         when Sinister =>
            case Pos.Dir is
            when Up =>
               Pos.X := Pos.X + 1;
               Pos.Dir := Right;
            when Right =>
               Pos.Y := Pos.Y - 1;
               Pos.Dir := Up;
            when Down =>
               Pos.X := Pos.X - 1;
               Pos.Dir := Left;
            when Left =>
               Pos.Y := Pos.Y + 1;
               Pos.Dir := Down;
            end case;
         when Dexter =>
            case Pos.Dir is
            when Up =>
               Pos.X := Pos.X - 1;
               Pos.Dir := Left;
            when Right =>
               Pos.Y := Pos.Y + 1;
               Pos.Dir := Down;
            when Down =>
               Pos.X := Pos.X + 1;
               Pos.Dir := Right;
            when Left =>
               Pos.Y := Pos.Y - 1;
               Pos.Dir := Up;
            end case;
         end case;

         exit All_Steps when Pos.X not in 1 .. Grid.Last_Index or Pos.Y not in 1 .. Grid.Last_Index; -- Off the square grid

         Row := Grid.Element (Pos.Y);
         Tile := Row.Element (Pos.X);
         Tile.Energized := True;
         Row.Replace_Element (Index => Pos.X, New_Item => Tile);
         Grid.Replace_Element (Index => Pos.Y, New_Item => Row);
      end loop All_Steps;
   end loop All_Beams;

   Count_Rows : for R in 1 .. Grid.Last_Index loop
      One_Row : declare
         Row : Tile_Lists.Vector renames Grid.Element (R);
      begin -- One_Row
         Count_Columns : for C in 1 .. Row.Last_Index loop
            Sum := Sum + (if Row.Element (C).Energized then 1 else 0);
         end loop Count_Columns;
      end One_Row;
   end loop Count_Rows;

   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_16_1;
