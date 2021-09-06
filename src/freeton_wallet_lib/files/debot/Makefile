all: build

build:
	ft contract import -f --make !{name}Debot.spp

deploy: build
	ft contract deploy !{name}Debot --create debot_!{name} -f
	ft call debot_!{name} setABI '{ "dabi": "%{hex:read:contract:abi:!{name}Debot}" }'
	ft call debot_!{name} setIcon '{ "icon": "%{hex:string:data:image/png;base64,%{base64:file:!{name}Debot.png}}" }'

run:
	ft client -- debot fetch %{account:address:debot_!{name}}

invoke:
	ft client -- debot invoke %{account:address:debot_!{name}} '%{account:in-message:debot_!{name}:0:other_entry:{ "x": 3 \\}}'

clean:
	rm -f *~ *.sol *.code

distclean: clean
	rm -f *.tvm *.abi.json

