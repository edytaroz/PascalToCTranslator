program test;
var
   num1 : integer;
   num2 : integer;
   num3 : integer;
   num4 : integer;
   num5 : integer;
   k : integer;

begin
   num1 := 5;
   num2 := 20000;
   num3 := 2;
   num4 := 1;
   num5 := 0;
   while num1 <= num2 do
      begin
         num5 := num5 + 1;
         if num5 > 1 then
         begin
            write (num5);
         end
         write (num1);
         if (num5 = num4) and not (num1 * 2 > num2) then
            begin
               write(num4);
               num5 := 0;
            end
         num1 := num1 * num3;
      end
    for k := 0 to 2 do
    begin
        write('Hello');
    end
    for k := 4 downto 0 do
    begin
        write('World');
    end
   write(num1);

end