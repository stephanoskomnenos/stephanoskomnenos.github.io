all: build

listen: build
	cabal exec site watch & \
	npx tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css -w & \
	npx tailwindcss -o ./css/tailwind.css -w

build:
	cabal build && \
	npx tailwindcss -i ./css/extra.css -o ./css/tailwind-extra.css && \
	npx tailwindcss -o ./css/tailwind.css && \
	cabal exec site build

clean:
	rm -r _cache _site & \
	rm css/tailwind.css css/tailwind-extra.css