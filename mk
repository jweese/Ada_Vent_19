#!/bin/bash

gnatflags=(-Ilib -Wall -O3)


if [[ -z "$1" ]]; then
  printf "usage: mk (clean|##)\n" >&2
  exit 1
elif [[ "$1" = clean ]]; then
  find . -type f \( -name "*.o" -o -name "*.ali" \) -delete
  find . -type f -name "day_*" -executable -delete
elif printf -v day "%02d" "$1"; then
  exec gnatmake "${gnatflags[@]}" day_"$day"
else
  printf "unknown argument '%s'\n" "$1" >&2
  exit 1
fi
