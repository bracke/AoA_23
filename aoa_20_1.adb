with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with PragmARC.Conversions.Unbounded_Strings;
with PragmARC.Data_Structures.Queues.Unbounded.Unprotected;
with PragmARC.Line_Fields;

procedure AOA_20_1 is
   use Ada.Strings.Unbounded;
   use PragmARC.Conversions.Unbounded_Strings;

   subtype Name_List is PragmARC.Line_Fields.Field_Lists.Vector;

   type Node_Kind_ID is (Broadcaster, Conjunction, Flip_Flop);

   type From_Info is record
      Name : Unbounded_String;
      High : Boolean := False;
   end record;

   package From_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => From_Info);

   type Node_Info (Kind : Node_Kind_ID := Broadcaster) is record
      To_Node : Name_List;

      case Kind is
      when Broadcaster =>
         null;
      when Conjunction =>
         From_Node : From_Lists.Vector;
      when Flip_Flop =>
         On : Boolean := False;
      end case;
   end record;

   package Node_Maps is new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                                   Element_Type    => Node_Info,
                                                                   Hash            => Ada.Strings.Hash,
                                                                   Equivalent_Keys => "=");

   type Pulse_Info is record
      From : Unbounded_String;
      To   : Unbounded_String;
      High : Boolean;
   end record;

   package Pulse_Queues is new PragmARC.Data_Structures.Queues.Unbounded.Unprotected (Element => Pulse_Info);

   type Result_Value is range 0 .. 2 ** 63 - 1;

   Input      : Ada.Text_IO.File_Type;
   Node_Map   : Node_Maps.Map;
   Cursor     : Node_Maps.Cursor;
   Node       : Node_Info;
   From_Pos   : Node_Maps.Cursor;
   From_Node  : Node_Info;
   Pulse_Q    : Pulse_Queues.Handle;
   Pulse      : Pulse_Info;
   Sender     : From_Info;
   All_High   : Boolean;
   Count_Low  : Result_Value := 0;
   Count_High : Result_Value := 0;

   use type Node_Maps.Cursor;
begin -- AOA_20_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_20");

   All_Lines : loop
      exit All_Lines when Ada.Text_IO.End_Of_File (Input);

      One_Line : declare
         Line    : constant String                               := Ada.Text_IO.Get_Line (Input);
         Start   : constant Positive                             := (if Line (1) in '%' | '&' then 2 else 1);
         Space   : constant Natural                              := Ada.Strings.Fixed.Index (Line, " ");
         To_Node : constant PragmARC.Line_Fields.Line_Field_Info :=
            PragmARC.Line_Fields.Parsed (Line (Space + 4 .. Line'Last), ',');

         Node : Node_Info (Kind => (if Start = 1 then Broadcaster elsif Line (1) = '%' then Flip_Flop else Conjunction) );
      begin -- One_Line
         Node.To_Node := To_Node.Field; -- On already set; From_Node must wait until all nodes are seen

         Trim_Spaces : for I in 1 .. Node.To_Node.Last_Index loop -- To names separated by ", "; parsing will leave the space
            Node.To_Node.Replace_Element (Index => I, New_Item => Trim (Node.To_Node.Element (I), Side => Ada.Strings.Both) );
         end loop Trim_Spaces;

         Node_Map.Insert (Key => Line (Start .. Space - 1), New_Item => Node);
      end One_Line;
   end loop All_Lines;

   Cursor := Node_Map.First;

   Fill_Conjunctions : loop
      exit Fill_Conjunctions when Cursor = Node_Maps.No_Element;

      Node := Node_Maps.Element (Cursor);

      if Node.Kind = Conjunction then
         From_Pos := Node_Map.First;

         All_Others : loop
            exit All_Others when From_Pos = Node_Maps.No_Element;

            if Cursor /= From_Pos then
               From_Node := Node_Maps.Element (From_Pos);

               if From_Node.To_Node.Contains (+Node_Maps.Key (Cursor) ) then
                  Node.From_Node.Append (New_Item => (Name => +Node_Maps.Key (From_Pos), others => <>) );
               end if;
            end if;

            Node_Maps.Next (Position => From_Pos);
         end loop All_Others;

         Node_Map.Replace_Element (Position => Cursor, New_Item => Node);
      end if;

      Node_Maps.Next (Position => Cursor);
   end loop Fill_Conjunctions;

   All_Trains : for Num in 1 .. 1_000 loop
      Pulse_Q.Put (Item => (From => +"button", To => +"broadcaster", High => False) );
      Count_Low := Count_Low + 1;

      All_Pulses : loop
         exit All_Pulses when Pulse_Q.Is_Empty;

         Pulse_Q.Get (Item => Pulse);

         if Node_Map.Contains (+Pulse.To) then
            Node := Node_Map.Element (+Pulse.To);

            case Node.Kind is
            when Broadcaster =>
               Forward : for I in 1 .. Node.To_Node.Last_Index loop
                  Pulse_Q.Put (Item => (From => Pulse.To, To => Node.To_Node.Element (I), High => Pulse.High) );
               end loop Forward;

               if Pulse.High then
                  Count_High := Count_High + Result_Value (Node.To_Node.Last_Index);
               else
                  Count_Low := Count_Low + Result_Value (Node.To_Node.Last_Index);
               end if;
            when Conjunction =>
               All_High := True;

               Update_From : for I in 1 .. Node.From_Node.Last_Index loop
                  Sender := Node.From_Node.Element (I);

                  if Sender.Name = Pulse.From then
                     Sender.High := Pulse.High;
                     Node.From_Node.Replace_Element (Index => I, New_Item => Sender);
                  end if;

                  All_High := All_High and Sender.High;
               end loop Update_From;

               Node_Map.Replace (Key => +Pulse.To, New_Item => Node);

               Conjunct : for I in 1 .. Node.To_Node.Last_Index loop
                  Pulse_Q.Put (Item => (From => Pulse.To, To => Node.To_Node.Element (I), High => not All_High) );
               end loop Conjunct;

               if All_High then
                  Count_Low := Count_Low + Result_Value (Node.To_Node.Last_Index);
               else
                  Count_High := Count_High + Result_Value (Node.To_Node.Last_Index);
               end if;
            when Flip_Flop =>
               if not Pulse.High then -- Flip_Flop only responds to a low pulse
                  Node.On := not Node.On;

                  Flipped : for I in 1 .. Node.To_Node.Last_Index loop
                     Pulse_Q.Put (Item => (From => Pulse.To, To => Node.To_Node.Element (I), High => Node.On) );
                  end loop Flipped;

                  if Node.On then
                     Count_High := Count_High + Result_Value (Node.To_Node.Last_Index);
                  else
                     Count_Low := Count_Low + Result_Value (Node.To_Node.Last_Index);
                  end if;

                  Node_Map.Replace (Key => +Pulse.To, New_Item => Node);
               end if;
            end case;
         end if;
      end loop All_Pulses;
   end loop All_Trains;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Result_Value'Image (Count_Low * Count_High) );
end AOA_20_1;
