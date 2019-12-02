FLAGS = -Wall -O3 -Ilib

%: %.adb
	gnatmake $(FLAGS) $^

.PHONY: clean

clean:
	find . -type f \( -name "*.o" -o -name "*.ali" \) -delete
	find . -name "day_*" -executable -delete
