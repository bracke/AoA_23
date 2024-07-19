with Ada.Text_IO;

procedure AOA_06_2 is
   Time : constant :=          46_689_866;
   Dist : constant := 358_105_418_071_080;

   type Big is mod 2 ** 64;

   Count : Natural := 0;
begin -- AOA_06_2
   All_Speeds : for Speed in Big range 1 .. Time - 1 loop
      if Speed * (Time - Speed) > Dist then
         Count := Count + 1;
      end if;
   end loop All_Speeds;

   Ada.Text_IO.Put_Line (Item => Count'Image);
end AOA_06_2;
