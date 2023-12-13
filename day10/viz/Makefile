
CC=gcc
CFLAGS=`pkg-config --cflags sdl2 SDL2_image SDL2_mixer SDL2_ttf sdl12_compat`
CLIBS=`pkg-config --libs sdl2 SDL2_image SDL2_mixer SDL2_ttf sdl12_compat`

hello: tour.c
	$(CC) -Werror -Wfatal-errors -O3 -o tour tour.c $(CFLAGS) $(CLIBS)

