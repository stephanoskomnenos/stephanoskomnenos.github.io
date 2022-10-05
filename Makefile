all: build

listen: 
	cabal exec site watch & \
	tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css -w & \
	tailwindcss -o ./css/tailwind.css -w

build:
	cabal build & \
	tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css & \
	tailwindcss -o ./css/tailwind.css