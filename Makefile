run: lambda-machine
	open static/index.html

lambda-machine: src/**/*.purs
	pulp build -O --to static/js/main.js

clean:
	rm -rf output

