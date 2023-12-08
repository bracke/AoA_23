with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure AOA_08_1 is
   type Dir_ID is (Left, Right);

   package Dir_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Dir_ID);

   subtype Node_Str is String (1 .. 3);

   type Node_ID_Base is range -1 .. 26 ** 3 - 1; -- -1 is invalid
   subtype Node_ID is Node_ID_Base range 0 .. Node_ID_Base'Last;

   function To_ID (Node : in Node_Str) return Node_ID with
      Pre => (for all C of Node => C in 'A' .. 'Z');
   -- Converts the String representation of a node to an ID

   function To_ID (Node : in Node_Str) return Node_ID is
      Result : Node_ID := 0;
   begin -- To_ID
      Convert : for C of Node loop
         Result := 26 * Result + Character'Pos (C) - Character'Pos ('A');
      end loop Convert;

      return Result;
   end To_ID;

   type Dir_Set is array (Dir_ID) of Node_ID_Base;

   type Dir_Map is array (Node_ID) of Dir_Set;

   type Counter is mod 2 ** 64;

   Input    : Ada.Text_IO.File_Type;
   Dir      : Dir_Lists.Vector;
   Next_Dir : Positive := 1;
   Map      : Dir_Map  := (others => (others => -1) );
   Node     : Node_ID  := 0;
   Set      : Dir_Set;
   Count    : Counter := 0;
begin -- AOA_08_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_08");

   Get_Dir : declare
      Line : constant String := Ada.Text_IO.Get_Line (Input);
   begin -- Get_Dir
      All_Dirs : for C of Line loop
         Dir.Append (New_Item => (if C = 'L' then Left else Right) );
      end loop All_Dirs;
   end Get_Dir;

   Ada.Text_IO.Skip_Line (File => Input);

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant String  := Ada.Text_IO.Get_Line (Input);
         Key  : constant Node_ID := To_ID (Line (1 .. 3) );
         Set  : constant Dir_Set := (Left => To_ID (Line (8 .. 10) ), Right => To_ID (Line (13 .. 15) ) );
      begin -- One_Line
         Map (Key) := Set;
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);

   Follow : loop
      exit Follow when Node = Node_ID'Last;

      Set := Map (Node);
      Node := Set (Dir.Element (Next_Dir) );
      Next_Dir := (if Next_Dir >= Dir.Last_Index then 1 else Next_Dir + 1);
      Count := Count + 1;
   end loop Follow;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => Count'Image);
end AOA_08_1;
