all: gen

listen: build
	cabal exec site watch & \
	npx tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css -w & \
	npx tailwindcss -o ./css/tailwind.css -w

gen: tailwind build
	cabal exec site build

build:
	cabal build

tailwind:
	npx tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css && \
	npx tailwindcss -o ./css/tailwind.css

clean:
	cabal exec site clean & \
	rm css/tailwind.css css/tailwind-extra.css
