run: lambda-machine
	open index.html

lambda-machine: deps src/**/*.purs
	pulp browserify -O --to static/js/main.js

deps: bower.json package.json
	bower install
	npm install

clean:
	rm -rf output

