with Ada.Text_IO;

procedure AOA_10_1 is
   subtype Map_Char is Character with Dynamic_Predicate => Map_Char in '|' | '-' | 'L' | 'J' | '7' | 'F' | '.' | 'S';

   subtype Map_Line is String with Dynamic_Predicate => (for all C of Map_Line => C in Map_Char);

   type Dir_ID is (North, East, South, West);

   function Opposite (Dir : in Dir_ID) return Dir_ID is
      (case Dir is
       when North => South,
       when East  => West,
       when South => North,
       when West  => East);

   procedure Find_Adjacent (X_In : in Positive; Y_In : in Positive; Dir : in Dir_ID; X_Out : out Positive; Y_Out : out Positive);
   -- Finds the coordinates of the tile adjacent to (X_In, Y_In) in direction Dir

   procedure Find_Adjacent (X_In : in Positive; Y_In : in Positive; Dir : in Dir_ID; X_Out : out Positive; Y_Out : out Positive)
   is
      -- Empty
   begin -- Find_Adjacent
      case Dir is
      when North =>
         X_Out := X_In;
         Y_Out := Y_In - 1;
      when East =>
         X_Out := X_In + 1;
         Y_Out := Y_In;
      when South =>
         X_Out := X_In;
         Y_Out := Y_In + 1;
      when West =>
         X_Out := X_In - 1;
         Y_Out := Y_In;
      end case;
   end Find_Adjacent;

   type Connected_Set is array (Dir_ID) of Boolean;

   function To_Connected (Char : in Map_Char) return Connected_Set is
      (case Char is
       when '|' => (North | South => True, others => False),
       when '-' => (East  | West  => True, others => False),
       when 'L' => (North | East  => True, others => False),
       when 'J' => (North | West  => True, others => False),
       when '7' => (South | West  => True, others => False),
       when 'F' => (East  | South => True, others => False),
       when '.' | 'S' => (others => False),
       when others => raise Program_Error with "Invalid map character" & Char);

   Size : constant := 140;

   type Connected_Map is array (0 .. Size + 1, 0 .. Size + 1) of Connected_Set;

   Input   : Ada.Text_IO.File_Type;
   Connect : Connected_Map := (others => (others => (others => False) ) );
   Start_X : Positive;
   Start_Y : Positive;
   D1      : Dir_ID;
   D2      : Dir_ID;
   X1      : Positive;
   Y1      : Positive;
   X2      : Positive;
   Y2      : Positive;
   Count   : Natural       := 0;
begin -- AOA_10_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_10");

   All_Lines : for Y in 1 .. Size loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant Map_Line := Ada.Text_IO.Get_Line (Input);
      begin -- One_Line
         All_Columns : for X in 1 .. Line'Last loop
            Connect (X, Y) := To_Connected (Line (X) );

            if Line (X) = 'S' then
               Start_X := X;
               Start_Y := Y;
            end if;
         end loop All_Columns;
      end One_Line;
   end loop All_Lines;

   Ada.Text_IO.Close (File => Input);

   Find_D1 : for D in Connected_Set'Range loop
      Find_Adjacent (X_In => Start_X, Y_In => Start_Y, Dir => D, X_Out => X1, Y_Out => Y1);

      if Connect (X1, Y1) (Opposite (D) ) then
         D1 := D;

         exit Find_D1;
      end if;
   end loop Find_D1;

   Find_D2 : for D in Dir_ID'Succ (D1) .. Dir_ID'Last loop
      Find_Adjacent (X_In => Start_X, Y_In => Start_Y, Dir => D, X_Out => X1, Y_Out => Y1);

      if Connect (X1, Y1) (Opposite (D) ) then
         D2 := D;

         exit Find_D2;
      end if;
   end loop Find_D2;

   Find_Adjacent (X_In => Start_X, Y_In => Start_Y, Dir => D1, X_Out => X1, Y_Out => Y1);
   Find_Adjacent (X_In => Start_X, Y_In => Start_Y, Dir => D2, X_Out => X2, Y_Out => Y2);

   Follow : loop
      if Count > Size ** 2 then
         raise Program_Error with "Two many steps" & Count'Image;
      end if;

      Count := Count + 1;

      exit Follow when X1 = X2 and Y1 = Y2;

      Next_D1 : for D in Connected_Set'Range loop
         if Connect (X1, Y1) (D) and D /= Opposite (D1) then
            Find_Adjacent (X_In => X1, Y_In => Y1, Dir => D, X_Out => X1, Y_Out => Y1);
            D1 := D;

            exit Next_D1;
         end if;
      end loop Next_D1;

      Next_D2 : for D in Connected_Set'Range loop
         if Connect (X2, Y2) (D) and D /= Opposite (D2) then
            Find_Adjacent (X_In => X2, Y_In => Y2, Dir => D, X_Out => X2, Y_Out => Y2);
            D2 := D;

            exit Next_D2;
         end if;
      end loop Next_D2;
   end loop Follow;

   Ada.Text_IO.Put_Line (Item => Count'Image);
end AOA_10_1;
