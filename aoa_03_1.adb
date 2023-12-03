with Ada.Text_IO;

procedure AOA_03_1 is
   Size : constant := 140; -- Map is Size x Size

   subtype Digit is Character range '0' .. '9';

   type Schematic is array (0 .. Size + 1, 0 .. Size + 1) of Character; -- Extra row around the edge to simplify checking

   procedure Find_Number (Row : in out Positive; From : in Positive; Start : out Positive; Stop : out Positive);
   -- Finds the Row and Start and Stop columns for the next number in Map
   -- Row > Size indicates no more numbers

   function Number (Row : in Positive; Start : in Positive; Stop : in Positive) return Natural with
      Pre => Stop >= Start;
   -- Converts the Characters in Map on Row in Start .. Stop to a number

   Map : Schematic := (others => (others => '.') ); -- Delayed-initialization constant

   procedure Find_Number (Row : in out Positive; From : in Positive; Start : out Positive; Stop : out Positive) is
      -- Empty
   begin -- Find_Number
      Stop := Integer'Last;
      Start := From;

      Find_Start : loop
         exit Find_Start when Map (Row, Start) in Digit;

         Start := Start + 1;

         if Start > Size then
            Row := Row + 1;

            if Row > Size then
               return;
            end if;

            Start := 1;
         end if;
      end loop Find_Start;

      Stop := Start;

      Find_Stop : loop
         exit Find_Stop when Map (Row, Stop + 1) not in Digit;

         Stop := Stop + 1;
      end loop Find_Stop;
   end Find_Number;

   function Number (Row : in Positive; Start : in Positive; Stop : in Positive) return Natural is
      Str : String (Start .. Stop);
   begin -- Number
      Copy : for I in Start .. Stop loop
         Str (I) := Map (Row, I);
      end loop Copy;

      return Integer'Value (Str);
   end Number;

   Input : Ada.Text_IO.File_Type;
   Row   : Positive := 1;
   Start : Positive;
   Stop  : Natural := 0;
   Sum   : Natural := 0;
begin -- AOA_03_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_03");

   All_Lines : for Row in 1 .. Size loop
      --  exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin -- One_Line
         Copy : for Column in Line'Range loop
            Map (Row, Column) := Line (Column);
         end loop Copy;
      end One_Line;
   end loop All_Lines; -- Map now a constant

   Add_Parts : loop
      Find_Number (Row => Row, From => Stop + 1, Start => Start, Stop => Stop);

      exit Add_Parts when Row > Size;

      Check_Rows : for R in Row - 1 .. Row + 1 loop
         Check_Columns : for C in Start - 1 .. Stop + 1 loop
            if Map (R, C) not in Digit | '.' then -- Adjacent symbol
               Sum := Sum + Number (Row, Start, Stop);

               exit Check_Rows;
            end if;
         end loop Check_Columns;
      end loop Check_Rows;
   end loop Add_Parts;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end AOA_03_1;
