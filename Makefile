all: build

listen: build
	cabal exec site watch & \
	tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css -w & \
	tailwindcss -o ./css/tailwind.css -w

build:
	cabal build && cabal exec site build & \
	tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css & \
	tailwindcss -o ./css/tailwind.css

clean:
	rm -r _cache & \
	rm -r _site