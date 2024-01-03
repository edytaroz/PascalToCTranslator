program Fibonnacci;
function fibonacci(n: integer): integer;
begin
	if n > 1 then
	begin
		fibonacci := fibonacci(n - 2) + fibonacci(n - 1);
	end
    else
	begin
		fibonacci := n;
	end
end
begin
    write(fibonnacci(5));
end