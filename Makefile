all:
	cd app/server/bin && ./compile-extensions.rb
	cd app/gui/qt && ./rp-build-app
