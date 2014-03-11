
update: priv/nmap-service-probes
	cd priv; wget --no-clobber https://svn.nmap.org/nmap/nmap-service-probes
build: 
	rebar compile

run: build 
	erl -s nnop_tree main -pa ebin

all: build update
